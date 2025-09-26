package io.github.pomadchin.dynosaur.derivation

import dynosaur.{Prism, Schema}
import dynosaur.Schema.structure.{Alt, Field}
import dynosaur.Schema.AltBuilder

import cats.syntax.apply.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.free.FreeApplicative
import cats.data.Chain

import scala.deriving.Mirror
import scala.compiletime.{constValue, constValueTuple, erasedValue, summonFrom, summonInline}

object SchemaAuto:
  inline def derive[T](using m: Mirror.Of[T]): Schema[T] = derive[T](true)

  inline def derive[T](nullabilityLenient: Boolean)(using m: Mirror.Of[T]): Schema[T] =
    derive[T](None, nullabilityLenient)

  inline def derive[T](discriminatorName: String)(using m: Mirror.Of[T]): Schema[T] =
    derive[T](discriminatorName, true)

  inline def derive[T](discriminatorName: String, nullabilityLenient: Boolean)(using m: Mirror.Of[T]): Schema[T] =
    derive[T](discriminatorName.some, nullabilityLenient)

  inline def derive[T](discriminatorName: Option[String], nullabilityLenient: Boolean)(using m: Mirror.Of[T]): Schema[T] =
    inline m match
      case p: Mirror.ProductOf[T] => deriveProduct[T](discriminatorName, nullabilityLenient)(using p)
      case s: Mirror.SumOf[T] => deriveSum[T](discriminatorName, nullabilityLenient)(using s)

  // when nullabilityLenient is set to true, optional fields can be interpreted as either explicitly null or as completely missing
  // schema favours missing fields on writes, but accepts both on reads
  private inline def deriveProduct[T](discriminatorName: Option[String], nullabilityLenient: Boolean)(using m: Mirror.ProductOf[T]): Schema[T] =
    val names = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)
    val fields = collectFields[T, m.MirroredElemTypes](names, 0)(using nullabilityLenient).map(nested => m.fromProduct(flatten(nested)))

    discriminatorName match
      case Some(discriminator) =>
        val className = constValue[m.MirroredLabel]
        Schema.fields(Schema.field[T].const[String](discriminator, className) *> fields)
      case None =>
        Schema.fields(fields)

  private inline def collectFields[T, Elems <: Tuple](names: List[String], idx: Int)(using nullabilityLenient: Boolean): FreeApplicative[Field[T, *], Tuple] =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        FreeApplicative.pure(EmptyTuple)
      case _: (t0 *: t1 *: t2 *: t3 *: t4 *: t5 *: t6 *: t7 *: t8 *: t9 *: t10 *: t11 *: t12 *: t13 *: t14 *: t15 *: t16 *: t17 *: t18 *: t19 *: t20 *: tail) =>
        val (headNames, tailNames) = names.splitAt(21)
        val head = buildField[T, Tuple.Take[Elems, 21]](headNames, idx)
        val tail = collectFields[T, Tuple.Drop[Elems, 21]](tailNames, idx + 21)
        (head, tail).mapN((h, t) => h *: t)
      case _ =>
        buildField[T, Elems](names, idx)

  private inline def buildField[T, Elems <: Tuple](names: List[String], idx: Int)(using nullabilityLenient: Boolean): FreeApplicative[Field[T, *], Tuple] =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        FreeApplicative.pure(EmptyTuple)
      case _: (h *: t) =>
        val head = fieldFor[T, h](names.head, idx)
        val tail = buildField[T, t](names.tail, idx + 1)
        (head, tail).mapN(_ *: _)

  private inline def fieldFor[T, A](name: String, idx: Int)(using nullabilityLenient: Boolean): FreeApplicative[Field[T, *], A] =
    inline erasedValue[A] match
      case _: Option[a] =>
        val schema = summonInline[Schema[a]]
        val schemaNullable = Schema.nullable[a](using schema)
        val getter: T => Option[a] = (t: T) => t.productElement[Option[a]](idx)
        val getterNullable: T => Option[Option[a]] = (t: T) => t.productElement[Option[a]](idx).map(_.some)

        val field =
          if nullabilityLenient then Schema.field[T].opt[Option[a]](name, getterNullable)(using schemaNullable).map(_.flatten)
          else Schema.field[T].opt[a](name, getter)(using schema)

        // FreeApplicative[Field[T, *], Option[a]] =:= FreeApplicative[Field[T, *], A]
        summonInline[Option[a] =:= A].liftCo(field)

      case _ =>
        val schema = summonInline[Schema[A]]
        val getter: T => A = (t: T) => t.productElement[A](idx)
        Schema.field[T](name, getter)(using schema)

  private inline def deriveSum[T](discriminatorName: Option[String], nullabilityLenient: Boolean)(using m: Mirror.SumOf[T]): Schema[T] =
    Schema.oneOf[T](altBuilder[T, m.MirroredElemTypes](_, discriminatorName, nullabilityLenient))

  private inline def altBuilder[T, Elems <: Tuple](alt: AltBuilder[T], discriminatorName: Option[String], nullabilityLenient: Boolean): Chain[Alt[T]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        Chain.nil
      case _: (h *: t) =>
        // attempt to summon Schema[h], if not found, derive it
        val headSchema = summonOrElse(deriveProduct[h](discriminatorName, nullabilityLenient)(using summonInline[Mirror.ProductOf[h]]))
        val head = alt[h](headSchema)(using summonInline[Prism[T, h]])

        inline erasedValue[t] match
          case _: EmptyTuple => head
          case _ => head |+| altBuilder[T, t](alt, discriminatorName, nullabilityLenient)

  private inline def summonOrElse[T](orElse: => T): T = summonFrom {
    case t: T => t
    case _ => orElse
  }

  private def flatten(t: Tuple): Tuple =
    def loop(elems: List[Any]): Tuple = elems match
      case Nil => EmptyTuple
      case (h: Tuple) :: tail => flatten(h) ++ loop(tail)
      case h :: tail => h *: loop(tail)

    loop(t.toList)

  extension [T](t: T)
    inline def productElement[R](idx: Int): R =
      summonInline[T <:< Product](t).productElement(idx).asInstanceOf[R]
