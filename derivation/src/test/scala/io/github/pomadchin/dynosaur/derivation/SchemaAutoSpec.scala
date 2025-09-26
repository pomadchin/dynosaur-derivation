package io.github.pomadchin.dynosaur.derivation

import dynosaur.{DynamoValue as V, Schema}
import cats.syntax.option.*
import cats.syntax.either.*
import cats.syntax.monoid.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SchemaAutoSpec extends AnyFunSpec with Matchers with SchemaCheckers {
  import SchemaAutoSpec.*

  describe("SchemaAutoSpec") {
    it("should derive schema for case class with no discriminator") {
      val actual = Document("id", 1, 2L, Some(true), Some("test"))
      val schema = SchemaAuto.derive[Document]
      val expected = V.m(
        "field0" -> V.s(actual.field0),
        "field1" -> V.n(actual.field1),
        "field2" -> V.n(actual.field2),
        "field3" -> V.bool(actual.field3.getOrElse(false)),
        "field4" -> V.s(actual.field4.getOrElse("bad string"))
      )

      check(schema, actual, expected)
    }

    it("should derive schema for case class with discriminator") {
      val actual = Document("id", 1, 2L, Some(true), Some("test"))
      val schema = SchemaAuto.derive[Document]("discriminator")
      val expected = V.m(
        "discriminator" -> V.s("Document"),
        "field0" -> V.s(actual.field0),
        "field1" -> V.n(actual.field1),
        "field2" -> V.n(actual.field2),
        "field3" -> V.bool(actual.field3.getOrElse(false)),
        "field4" -> V.s(actual.field4.getOrElse("bad string"))
      )

      check(schema, actual, expected)
    }

    it("should derive schema for case class of 25 with discriminator") {
      // format: off
      val actual = Document25(
        "field0",  "field1",  "field2",  "field3",  "field4",
        "field5",  "field6",  "field7",  "field8",  "field9",
        "field10", "field11", "field12", "field13", "field14",
        "field15", "field16", "field17", "field18", "field19",
        "field20", "field21", "field22", "field23", "field24"
      )
      // format: on

      val schema = SchemaAuto.derive[Document25]("discriminator")

      // format: off
      val expected = V.m(
        "discriminator" -> V.s("Document25"),
        "field0"  -> V.s(actual.field0),  "field1"  -> V.s(actual.field1),  "field2"  -> V.s(actual.field2),
        "field3"  -> V.s(actual.field3),  "field4"  -> V.s(actual.field4),  "field5"  -> V.s(actual.field5),
        "field6"  -> V.s(actual.field6),  "field7"  -> V.s(actual.field7),  "field8"  -> V.s(actual.field8),
        "field9"  -> V.s(actual.field9),  "field10" -> V.s(actual.field10), "field11" -> V.s(actual.field11),
        "field12" -> V.s(actual.field12), "field13" -> V.s(actual.field13), "field14" -> V.s(actual.field14),
        "field15" -> V.s(actual.field15), "field16" -> V.s(actual.field16), "field17" -> V.s(actual.field17),
        "field18" -> V.s(actual.field18), "field19" -> V.s(actual.field19), "field20" -> V.s(actual.field20),
        "field21" -> V.s(actual.field21), "field22" -> V.s(actual.field22), "field23" -> V.s(actual.field23),
        "field24" -> V.s(actual.field24)
      )
      // format: on

      check(schema, actual, expected)
    }

    it("should derive schema for case class of 25 optionals with discriminator") {
      // format: off
      val actual = Document25o(
        "field0".some,  "field1".some,  "field2".some,  "field3".some,  "field4".some,
        "field5".some,  "field6".some,  "field7".some,  "field8".some,  "field9".some,
        "field10".some, "field11".some, "field12".some, "field13".some, "field14".some,
        "field15".some, "field16".some, "field17".some, "field18".some, "field19".some,
        "field20".some, "field21".some, "field22".some, "field23".some, "field24".some
      )
      // format: on

      val schema = SchemaAuto.derive[Document25o]("discriminator")

      val expected = V.m(
        "discriminator" -> V.s("Document25o"),
        "field0" -> V.s(actual.field0.getOrElse("bad string")),
        "field1" -> V.s(actual.field1.getOrElse("bad string")),
        "field2" -> V.s(actual.field2.getOrElse("bad string")),
        "field3" -> V.s(actual.field3.getOrElse("bad string")),
        "field4" -> V.s(actual.field4.getOrElse("bad string")),
        "field5" -> V.s(actual.field5.getOrElse("bad string")),
        "field6" -> V.s(actual.field6.getOrElse("bad string")),
        "field7" -> V.s(actual.field7.getOrElse("bad string")),
        "field8" -> V.s(actual.field8.getOrElse("bad string")),
        "field9" -> V.s(actual.field9.getOrElse("bad string")),
        "field10" -> V.s(actual.field10.getOrElse("bad string")),
        "field11" -> V.s(actual.field11.getOrElse("bad string")),
        "field12" -> V.s(actual.field12.getOrElse("bad string")),
        "field13" -> V.s(actual.field13.getOrElse("bad string")),
        "field14" -> V.s(actual.field14.getOrElse("bad string")),
        "field15" -> V.s(actual.field15.getOrElse("bad string")),
        "field16" -> V.s(actual.field16.getOrElse("bad string")),
        "field17" -> V.s(actual.field17.getOrElse("bad string")),
        "field18" -> V.s(actual.field18.getOrElse("bad string")),
        "field19" -> V.s(actual.field19.getOrElse("bad string")),
        "field20" -> V.s(actual.field20.getOrElse("bad string")),
        "field21" -> V.s(actual.field21.getOrElse("bad string")),
        "field22" -> V.s(actual.field22.getOrElse("bad string")),
        "field23" -> V.s(actual.field23.getOrElse("bad string")),
        "field24" -> V.s(actual.field24.getOrElse("bad string"))
      )

      check(schema, actual, expected)
    }

    it("should derive schema for case class of 55 with discriminator") {
      // format: off
      val actual = Document55(
        "field0",  "field1",  "field2",  "field3",  "field4",
        "field5",  "field6",  "field7",  "field8",  "field9",
        "field10", "field11", "field12", "field13", "field14",
        "field15", "field16", "field17", "field18", "field19",
        "field20", "field21", "field22", "field23", "field24",
        "field25", "field26", "field27", "field28", "field29",
        "field30", "field31", "field32", "field33", "field34",
        "field35", "field36", "field37", "field38", "field39",
        "field40", "field41", "field42", "field43", "field44",
        "field45", "field46", "field47", "field48", "field49",
        "field50", "field51", "field52", "field53", "field54"
      )
      // format: on

      val schema = SchemaAuto.derive[Document55]("discriminator")

      // format: off
      val expected = V.m(
        "discriminator" -> V.s("Document55"),
        "field0"  -> V.s(actual.field0),  "field1"  -> V.s(actual.field1),  "field2"  -> V.s(actual.field2),
        "field3"  -> V.s(actual.field3),  "field4"  -> V.s(actual.field4),  "field5"  -> V.s(actual.field5),
        "field6"  -> V.s(actual.field6),  "field7"  -> V.s(actual.field7),  "field8"  -> V.s(actual.field8),
        "field9"  -> V.s(actual.field9),  "field10" -> V.s(actual.field10), "field11" -> V.s(actual.field11),
        "field12" -> V.s(actual.field12), "field13" -> V.s(actual.field13), "field14" -> V.s(actual.field14),
        "field15" -> V.s(actual.field15), "field16" -> V.s(actual.field16), "field17" -> V.s(actual.field17),
        "field18" -> V.s(actual.field18), "field19" -> V.s(actual.field19), "field20" -> V.s(actual.field20),
        "field21" -> V.s(actual.field21), "field22" -> V.s(actual.field22), "field23" -> V.s(actual.field23),
        "field24" -> V.s(actual.field24), "field25" -> V.s(actual.field25), "field26" -> V.s(actual.field26),
        "field27" -> V.s(actual.field27), "field28" -> V.s(actual.field28), "field29" -> V.s(actual.field29),
        "field30" -> V.s(actual.field30), "field31" -> V.s(actual.field31), "field32" -> V.s(actual.field32),
        "field33" -> V.s(actual.field33), "field34" -> V.s(actual.field34), "field35" -> V.s(actual.field35),
        "field36" -> V.s(actual.field36), "field37" -> V.s(actual.field37), "field38" -> V.s(actual.field38),
        "field39" -> V.s(actual.field39), "field40" -> V.s(actual.field40), "field41" -> V.s(actual.field41),
        "field42" -> V.s(actual.field42), "field43" -> V.s(actual.field43), "field44" -> V.s(actual.field44),
        "field45" -> V.s(actual.field45), "field46" -> V.s(actual.field46), "field47" -> V.s(actual.field47),
        "field48" -> V.s(actual.field48), "field49" -> V.s(actual.field49), "field50" -> V.s(actual.field50),
        "field51" -> V.s(actual.field51), "field52" -> V.s(actual.field52), "field53" -> V.s(actual.field53),
        "field54" -> V.s(actual.field54)
      )
      // format: on

      check(schema, actual, expected)
    }

    it("should derive schema for the sealed family") {
      val actual1 = Document("id", 1, 2L, Some(true), Some("test"))
      val actual2 = Document2("id", 1, 2L, Some(true), Some("test"))

      val schema = Schema.oneOf[DocumentBase] { alt =>
        val documentSchema = SchemaAuto.derive[Document]("discriminator")
        val document2Schema =
          SchemaAuto.derive[Document2]("discriminator")
        alt(documentSchema) |+| alt(document2Schema)
      }

      val expected1 = V.m(
        "discriminator" -> V.s("Document"),
        "field0" -> V.s(actual1.field0),
        "field1" -> V.n(actual1.field1),
        "field2" -> V.n(actual1.field2),
        "field3" -> V.bool(actual1.field3.getOrElse(false)),
        "field4" -> V.s(actual1.field4.getOrElse("bad string"))
      )

      val expected2 = V.m(
        "discriminator" -> V.s("Document2"),
        "field0" -> V.s(actual2.field0),
        "field1" -> V.n(actual2.field1),
        "field2" -> V.n(actual2.field2),
        "field3" -> V.bool(actual2.field3.getOrElse(false)),
        "field4" -> V.s(actual2.field4.getOrElse("bad string"))
      )

      check(schema, actual1: DocumentBase, expected1)
      check(schema, actual2: DocumentBase, expected2)
    }

    it("should derive schema with discriminator for the optional fields, leniency to nullability") {
      val actual = DocumentSmall("id", Some("field"))
      val actualNone = DocumentSmall("id", None)
      val schema = SchemaAuto.derive[DocumentSmall]("discriminator")

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
      schema.read(expectedNull) shouldBe actualNone.asRight
    }

    it("should derive schema with discriminator for the optional fields, leniency to nullability is disabled") {
      val actual = DocumentSmall("id", Some("field"))
      val actualNone = DocumentSmall("id", None)
      val schema = SchemaAuto.derive[DocumentSmall]("discriminator", false)

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
      schema.read(notExpectedNull).isLeft shouldBe true
    }

    it("should derive schema without discriminator for the optional fields, leniency to nullability") {
      val actual = DocumentSmall("id", Some("field"))
      val actualNone = DocumentSmall("id", None)
      val schema = SchemaAuto.derive[DocumentSmall]

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
      schema.read(expectedNull) shouldBe actualNone.asRight
    }

    it("should derive schema without discriminator for the optional fields, leniency to nullability is disabled") {
      val actual = DocumentSmall("id", Some("field"))
      val actualNone = DocumentSmall("id", None)
      val schema = SchemaAuto.derive[DocumentSmall](false)

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
      schema.read(notExpectedNull).isLeft shouldBe true
    }
  }
}

object SchemaAutoSpec {
  sealed trait DocumentBase

  final case class DocumentSmall(field0: String, field1: Option[String]) extends DocumentBase

  final case class Document(
    field0: String,
    field1: Int,
    field2: Long,
    field3: Option[Boolean],
    field4: Option[String]
  ) extends DocumentBase

  final case class Document2(
    field0: String,
    field1: Int,
    field2: Long,
    field3: Option[Boolean],
    field4: Option[String]
  ) extends DocumentBase

  final case class Document25(
    field0: String,
    field1: String,
    field2: String,
    field3: String,
    field4: String,
    field5: String,
    field6: String,
    field7: String,
    field8: String,
    field9: String,
    field10: String,
    field11: String,
    field12: String,
    field13: String,
    field14: String,
    field15: String,
    field16: String,
    field17: String,
    field18: String,
    field19: String,
    field20: String,
    field21: String,
    field22: String,
    field23: String,
    field24: String
  ) extends DocumentBase

  final case class Document25o(
    field0: Option[String],
    field1: Option[String],
    field2: Option[String],
    field3: Option[String],
    field4: Option[String],
    field5: Option[String],
    field6: Option[String],
    field7: Option[String],
    field8: Option[String],
    field9: Option[String],
    field10: Option[String],
    field11: Option[String],
    field12: Option[String],
    field13: Option[String],
    field14: Option[String],
    field15: Option[String],
    field16: Option[String],
    field17: Option[String],
    field18: Option[String],
    field19: Option[String],
    field20: Option[String],
    field21: Option[String],
    field22: Option[String],
    field23: Option[String],
    field24: Option[String]
  ) extends DocumentBase

  final case class Document55(
    field0: String,
    field1: String,
    field2: String,
    field3: String,
    field4: String,
    field5: String,
    field6: String,
    field7: String,
    field8: String,
    field9: String,
    field10: String,
    field11: String,
    field12: String,
    field13: String,
    field14: String,
    field15: String,
    field16: String,
    field17: String,
    field18: String,
    field19: String,
    field20: String,
    field21: String,
    field22: String,
    field23: String,
    field24: String,
    field25: String,
    field26: String,
    field27: String,
    field28: String,
    field29: String,
    field30: String,
    field31: String,
    field32: String,
    field33: String,
    field34: String,
    field35: String,
    field36: String,
    field37: String,
    field38: String,
    field39: String,
    field40: String,
    field41: String,
    field42: String,
    field43: String,
    field44: String,
    field45: String,
    field46: String,
    field47: String,
    field48: String,
    field49: String,
    field50: String,
    field51: String,
    field52: String,
    field53: String,
    field54: String
  ) extends DocumentBase
}
