package io.github.pomadchin.dynosaur.derivation

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import dynosaur.Schema

object SchemaAuto {
  def derive[T]: Schema[T] = macro SchemaMacro.implNoDiscriminator[T]
  def derive[T](discriminatorName: String): Schema[T] = macro SchemaMacro.implWithDiscriminator[T]
}

object SchemaMacro {
  def implNoDiscriminator[T: c.WeakTypeTag](
    c: whitebox.Context
  ): c.Expr[Schema[T]] =
    impl(c)(None)

  def implWithDiscriminator[T: c.WeakTypeTag](
    c: whitebox.Context
  )(discriminatorName: c.Expr[String]): c.Expr[Schema[T]] = {
    import c.universe._
    val Literal(Constant(fieldName: String)) = discriminatorName.tree
    impl(c)(Some(fieldName))
  }

  private def impl[T: c.WeakTypeTag](
    c: whitebox.Context
  )(discriminatorName: Option[String]): c.Expr[Schema[T]] = {
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
      val fieldType = field.returnType

      if (fieldType.typeConstructor <:< typeOf[Option[?]].typeConstructor)
        fieldType match {
          case TypeRef(_, _, List(inner)) =>
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
}
