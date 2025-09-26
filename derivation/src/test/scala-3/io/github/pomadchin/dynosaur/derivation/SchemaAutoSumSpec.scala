package io.github.pomadchin.dynosaur.derivation

import dynosaur.{DynamoValue as V, Schema}
import cats.syntax.option.*
import cats.syntax.either.*
import cats.syntax.monoid.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SchemaAutoSumSpec extends AnyFunSpec with Matchers with SchemaCheckers:
  import SchemaAutoSumSpec.*

  describe("SchemaAutoSumSpec"):
    it("should auto derive schema for the sealed family"):
      val actual1 = DocumentA("field0")
      val actual2 = DocumentB("field0", "field1")

      implicit val schemaA: Schema[DocumentA] = SchemaAuto.derive("discriminator")
      implicit val schemaB: Schema[DocumentB] = SchemaAuto.derive("discriminator")

      val schema = SchemaAuto.derive[DocumentFamily]("discriminator")

      val expected1 = V.m(
        "discriminator" -> V.s("DocumentA"),
        "field0" -> V.s(actual1.field0)
      )

      val expected2 = V.m(
        "discriminator" -> V.s("DocumentB"),
        "field0" -> V.s(actual2.field0),
        "field1" -> V.s(actual2.field1)
      )

      check(schema, actual1: DocumentFamily, expected1)
      check(schema, actual2: DocumentFamily, expected2)

object SchemaAutoSumSpec:
  sealed trait DocumentFamily
  case class DocumentA(field0: String) extends DocumentFamily
  case class DocumentB(field0: String, field1: String) extends DocumentFamily
