package io.github.pomadchin.dynosaur.derivation

import io.github.pomadchin.dynosaur.derivation.SchemaAuto
import dynosaur.Schema

import scala.deriving.Mirror

object optional:
  extension (x: Schema.type) inline def derived[T](using m: Mirror.Of[T]): Schema[T] = SchemaAuto.derive[T](None, false)

  object discriminator:
    extension (x: Schema.type) inline def derived[T](using m: Mirror.Of[T]): Schema[T] = SchemaAuto.derive[T](Some("discriminator"), false)

object nullable:
  extension (x: Schema.type) inline def derived[T](using m: Mirror.Of[T]): Schema[T] = SchemaAuto.derive[T](None, true)

  object discriminator:
    extension (x: Schema.type) inline def derived[T](using m: Mirror.Of[T]): Schema[T] = SchemaAuto.derive[T](Some("discriminator"), true)
