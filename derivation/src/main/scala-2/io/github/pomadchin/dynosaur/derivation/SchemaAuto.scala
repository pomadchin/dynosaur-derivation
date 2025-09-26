package io.github.pomadchin.dynosaur.derivation

import dynosaur.Schema

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object SchemaAuto {
  def derive[T]: Schema[T] = macro SchemaMacro.implNoDiscriminator[T]

  def derive[T](nullabilityLenient: Boolean): Schema[T] =
    macro SchemaMacro.implNoDiscriminatorAndNullability[T]

  def derive[T](discriminatorName: String): Schema[T] =
    macro SchemaMacro.implWithDiscriminator[T]

  def derive[T](discriminatorName: String, nullabilityLenient: Boolean): Schema[T] =
    macro SchemaMacro.implWithDiscriminatorAndNullability[T]
}

object SchemaMacro {
  def implNoDiscriminator[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[Schema[T]] =
    impl(c)(None, nullabilityLenient = true)

  def implNoDiscriminatorAndNullability[T: c.WeakTypeTag](c: blackbox.Context)(nullabilityLenient: c.Expr[Boolean]): c.Expr[Schema[T]] = {
    import c.universe._
    val Literal(Constant(nullability: Boolean)) = nullabilityLenient.tree
    impl(c)(None, nullability)
  }

  def implWithDiscriminator[T: c.WeakTypeTag](c: blackbox.Context)(discriminatorName: c.Expr[String]): c.Expr[Schema[T]] = {
    import c.universe._
    val Literal(Constant(discriminator: String)) = discriminatorName.tree
    impl(c)(Some(discriminator), nullabilityLenient = true)
  }

  def implWithDiscriminatorAndNullability[T: c.WeakTypeTag](
    c: blackbox.Context
  )(discriminatorName: c.Expr[String], nullabilityLenient: c.Expr[Boolean]): c.Expr[Schema[T]] = {
    import c.universe._
    val Literal(Constant(discriminator: String)) = discriminatorName.tree
    val Literal(Constant(nullability: Boolean)) = nullabilityLenient.tree
    impl(c)(Some(discriminator), nullability)
  }

  private def impl[T: c.WeakTypeTag](c: blackbox.Context)(discriminatorName: Option[String], nullabilityLenient: Boolean): c.Expr[Schema[T]] = {
    import c.universe._
    val sym = weakTypeOf[T].typeSymbol.asClass

    if (sym.isSealed) sumImpl[T](c)(discriminatorName, nullabilityLenient)
    else productImpl[T](c)(discriminatorName, nullabilityLenient)
  }

  // when nullabilityLenient is set to true, optional fields can be interpreted as either explicitly null or as completely missing
  // schema favours missing fields on writes, but accepts both on reads
  private def productImpl[T: c.WeakTypeTag](c: blackbox.Context)(discriminatorName: Option[String], nullabilityLenient: Boolean): c.Expr[Schema[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val companion = tpe.typeSymbol.companion
    val fields = tpe.decls.collect {
      case m: MethodSymbol if m.isCaseAccessor => m
    }.toList

    // Generate:
    // (field("name", _.name), field.opt("opt", _.opt), ...).mapN(Companion.apply)
    // however this approach is limited by the function arity of 22
    // the solution is to recursively split fields into tuples of 22
    // ((field1, field2, ... (field21, field22, ...))
    val fieldTrees = fields.map { field =>
      // scalafix:off Disable.toString
      val name = field.name.decodedName.toString
      // scalafix:on
      val getter = q"(x: $tpe) => x.${field.name}"
      val getterNullable = q"(x: $tpe) => x.${field.name}.map(Some(_))"

      val fieldType = field.returnType

      if (fieldType.typeConstructor <:< typeOf[Option[?]].typeConstructor)
        fieldType match {
          case TypeRef(_, _, List(inner)) =>
            if (nullabilityLenient)
              q"""field.opt[$fieldType]($name, $getterNullable)(Schema.nullable).map(_.flatten)"""
            else
              q"""field.opt[$inner]($name, $getter)"""
          case _ =>
            c.abort(
              c.enclosingPosition,
              s"Expected Option[?], but got: $fieldType"
            )
        }
      else q"""field[$fieldType]($name, $getter)"""
    }

    // index names, to pass into the case class constructor to overcome arity limitation
    val paramNames = fields.indices.map(i => TermName(s"x$i")).toList

    def groupTupled(list: List[Tree]): Tree =
      if (list.length <= 22) q"(..$list)"
      else {
        val (head, tail) = list.splitAt(21)
        q"(..$head, ${groupTupled(tail)}.tupled)"
      }

    def groupPattern(list: List[TermName]): Tree =
      if (list.length <= 22) pq"(..${list.map(p => pq"$p")})"
      else {
        val (head, tail) = list.splitAt(21)
        pq"(..${head.map(p => pq"$p")}, ${groupPattern(tail)})"
      }

    val nestedTuple = groupTupled(fieldTrees)
    val nestedPattern = groupPattern(paramNames)
    val constructorCall = q"$companion(..$paramNames)"
    val convertedRecord =
      if (paramNames.length == 1)
        q"$nestedTuple.map { case $nestedPattern => $constructorCall }"
      else
        q"$nestedTuple.mapN { case $nestedPattern => $constructorCall }"

    // scalafix:off Disable.toString
    val companionLiteral = Literal(Constant(companion.name.toString))
    // scalafix:on
    val discriminatorLiteral =
      discriminatorName.map(str => Literal(Constant(str)))

    // const is used a discriminator
    val discriminatedRecord =
      discriminatorLiteral.fold(convertedRecord)(discriminator => q"""field.const[String]($discriminator, $companionLiteral) *> $convertedRecord""")

    val instance =
      q"""
        import _root_.cats.syntax.apply._
        _root_.dynosaur.Schema.record[$tpe] { field =>
          $discriminatedRecord
        }
      """

    c.Expr[Schema[T]](instance)
  }

  private def sumImpl[T: c.WeakTypeTag](c: blackbox.Context)(discriminatorName: Option[String], nullabilityLenient: Boolean): c.Expr[Schema[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass

    val discriminatorNameLiteral = discriminatorName.map(str => Literal(Constant(str)))

    // Collect direct subclasses
    val knownDirectSubclasses = sym.knownDirectSubclasses.toList
    if (knownDirectSubclasses.isEmpty)
      c.abort(c.enclosingPosition, s"Sealed type ${sym.fullName} has no known direct subclasses (macro visibility issue).")

    // Map Base[A, B, ...] type args onto subclass if needed
    def applyTypeArgs(sub: Symbol): Type = {
      val raw = sub.asType.toType
      if (tpe.typeArgs.nonEmpty && raw.typeParams.nonEmpty && raw.typeParams.size == tpe.typeArgs.size)
        appliedType(raw.typeConstructor, tpe.typeArgs)
      else raw
    }

    val knownDirectSubclassesTypes = knownDirectSubclasses.map(applyTypeArgs(_)).sortBy(_.typeSymbol.fullName)

    // For each subtype S, expand:
    //   val sS = SchemaAuto.derive[S](discriminatorName, nullabilityLenient)
    //   val aS = alt[S](sS)(implicitly[Prism[T, S]])
    // then combine with |+|
    val altTrees = knownDirectSubclassesTypes.map { sTpe =>
      discriminatorNameLiteral match {
        case Some(discriminatorName) =>
          q"""alt(_root_.io.github.pomadchin.dynosaur.derivation.SchemaAuto.derive[$sTpe]($discriminatorName, $nullabilityLenient))"""

        case None =>
          q"""alt(_root_.io.github.pomadchin.dynosaur.derivation.SchemaAuto.derive[$sTpe]($nullabilityLenient))"""
      }
    }
    val combined = altTrees.reduceLeft[Tree] { (acc, next) => q"$acc |+| $next" }

    val tree =
      q"""
        _root_.dynosaur.Schema.oneOf[$tpe] { alt =>
          import _root_.cats.syntax.semigroup._
          $combined
        }
      """

    c.Expr[Schema[T]](tree)
  }
}
