package io.github.pomadchin.dynosaur.derivation

import dynosaur.{DynamoValue as V, Schema}

import cats.syntax.option.*

import org.scalatest.matchers.should.Matchers

trait SchemaCheckers { self: Matchers =>
  def check[A](schema: Schema[A], actual: A, expected: V) = {
    val output = schema.write(actual).toOption
    val roundTrip = output.flatMap(schema.read(_).toOption)

    output shouldBe expected.some
    roundTrip shouldBe actual.some
  }
}
