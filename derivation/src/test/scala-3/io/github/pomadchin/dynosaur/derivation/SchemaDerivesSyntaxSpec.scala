package io.github.pomadchin.dynosaur.derivation

import dynosaur.{DynamoValue as V, Schema}
import cats.syntax.option.*
import cats.syntax.either.*
import cats.syntax.monoid.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SchemaDerivesSyntaxSpec extends AnyFunSpec with Matchers with SchemaCheckers:
  import SchemaDerivesSyntaxSpec.*

  describe("SchemaDerivesSyntaxSpec"):
    it("should derive schema with discriminator for the optional fields, leniency to nullability"):
      import nullable.discriminator.*

      val actual = DocumentSmall("id", Some("field"))
      val actualNone = DocumentSmall("id", None)
      val schema = summon[Schema[DocumentSmall]]

      val expected = V.m(
        "discriminator" -> V.s("DocumentSmall"),
        "field0" -> V.s("id"),
        "field1" -> V.s("field")
      )

      val expectedNull = V.m(
        "discriminator" -> V.s("DocumentSmall"),
        "field0" -> V.s("id"),
        "field1" -> V.nul
      )

      val expectedOptional = V.m(
        "discriminator" -> V.s("DocumentSmall"),
        "field0" -> V.s("id")
      )

      check(schema, actual, expected)
      check(schema, actualNone, expectedOptional)
      schema.read(expectedNull).shouldBe(actualNone.asRight)

    it("should derive schema with discriminator for the optional fields, leniency to nullability is disabled"):
      import optional.discriminator.*

      val actual = DocumentSmall("id", Some("field"))
      val actualNone = DocumentSmall("id", None)
      val schema = summon[Schema[DocumentSmall]]

      val expected = V.m(
        "discriminator" -> V.s("DocumentSmall"),
        "field0" -> V.s("id"),
        "field1" -> V.s("field")
      )

      val notExpectedNull = V.m(
        "discriminator" -> V.s("DocumentSmall"),
        "field0" -> V.s("id"),
        "field1" -> V.nul
      )

      val expectedOptional = V.m(
        "discriminator" -> V.s("DocumentSmall"),
        "field0" -> V.s("id")
      )

      check(schema, actual, expected)
      check(schema, actualNone, expectedOptional)
      schema.read(notExpectedNull).isLeft.shouldBe(true)

    it("should derive schema without discriminator for the optional fields, leniency to nullability"):
      import nullable.*

      val actual = DocumentSmall("id", Some("field"))
      val actualNone = DocumentSmall("id", None)
      val schema = summon[Schema[DocumentSmall]]

      val expected = V.m(
        "field0" -> V.s("id"),
        "field1" -> V.s("field")
      )

      val expectedNull = V.m(
        "field0" -> V.s("id"),
        "field1" -> V.nul
      )

      val expectedOptional = V.m(
        "field0" -> V.s("id")
      )

      check(schema, actual, expected)
      check(schema, actualNone, expectedOptional)
      schema.read(expectedNull).shouldBe(actualNone.asRight)

    it("should derive schema without discriminator for the optional fields, leniency to nullability is disabled"):
      import optional.*;

      val actual = DocumentSmall("id", Some("field"))
      val actualNone = DocumentSmall("id", None)
      val schema = summon[Schema[DocumentSmall]]

      val expected = V.m(
        "field0" -> V.s("id"),
        "field1" -> V.s("field")
      )

      val notExpectedNull = V.m(
        "field0" -> V.s("id"),
        "field1" -> V.nul
      )

      val expectedOptional = V.m(
        "field0" -> V.s("id")
      )

      check(schema, actual, expected)
      check(schema, actualNone, expectedOptional)
      schema.read(notExpectedNull).isLeft.shouldBe(true)

object SchemaDerivesSyntaxSpec:
  object optional:
    import io.github.pomadchin.dynosaur.derivation.optional.derived

    final case class DocumentSmall(
      field0: String,
      field1: Option[String]
    ) derives Schema

    object discriminator:
      import io.github.pomadchin.dynosaur.derivation.optional.discriminator.derived

      final case class DocumentSmall(
        field0: String,
        field1: Option[String]
      ) derives Schema

  object nullable:
    import io.github.pomadchin.dynosaur.derivation.nullable.derived

    final case class DocumentSmall(
      field0: String,
      field1: Option[String]
    ) derives Schema

    object discriminator:
      import io.github.pomadchin.dynosaur.derivation.nullable.discriminator.derived

      final case class DocumentSmall(
        field0: String,
        field1: Option[String]
      ) derives Schema
