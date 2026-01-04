# Dynosaur Derivation

[![CI](https://github.com/pomadchin/dynosaur-derivation/actions/workflows/ci.yml/badge.svg)](https://github.com/pomadchin/tagless-mid/actions/workflows/ci.yml)
[![Maven Badge](https://img.shields.io/maven-central/v/io.github.pomadchin/dynosaur-derivation_3?color=blue)](https://central.sonatype.com/search?q=g%3Aio.github.pomadchin&smo=true&name=dynosaur-derivation_3)
[![Snapshots Badge](https://img.shields.io/badge/snapshots-available-orange)](https://central.sonatype.com/service/rest/repository/browse/maven-snapshots/io/github/pomadchin/dynosaur-derivation_3/) 

[Dynosaur](https://github.com/SystemFw/dynosaur) DynamoDB Schema derivation library for case classes.

Before using this library, ensure to align with the [Dynosaur documentation and the underlying motivation](https://systemfw.org/dynosaur/#/motivation?id=motivation).
Dynosaur is designed around explicit, hand-crafted schemas, and that approach should generally be preferred.

However, in cases where modeling the DynamoDB domain as case classes is unavoidable, this library can help reduce boilerplate.

## Quick Start with SBT

```scala
// `<latest version>` refers to the version indicated by the badge above
libraryDependencies += "io.github.pomadchin" %% "dynosaur-derivation" % "<latest version>"
```

## Examples

Additional, more detailed examples are available in the tests:

* [Scala 3 Derivation Syntax Spec](https://github.com/pomadchin/dynosaur-derivation/blob/main/derivation/src/test/scala-3/io/github/pomadchin/dynosaur/derivation/SchemaDerivesSyntaxSpec.scala)
* [Scala 2 / Scala 3 Serivation Spec](https://github.com/pomadchin/dynosaur-derivation/blob/main/derivation/src/test/scala/io/github/pomadchin/dynosaur/derivation/SchemaAutoSpec.scala)

### Scala 3 Derivation Syntax

```scala
import io.github.pomadchin.dynosaur.derivation.*
import nullable.discriminator.* // configures the derivation mode

import dynosaur.Schema

case class Document(id: String, text: Option[String], number: Long) derives Schema

sealed trait Documents derives Schema
case class Document1(id: String, text: Option[String], number: Long) extends Documents
case class Document2(id: String, text: Option[String], number: Long) extends Documents
```

* `nullable.discriminator.*` - derivation with discriminator and leniency to nullability enabled
* `optional.discriminator.*` - derivation with discriminator and leniency to nullability disabled
* `nullable.*` - derivation without discriminator and leniency to nullability enabled
* `optional.*` - derivation without discriminator and leniency to nullability disabled

### Scala 2 / Scala 3 Simple Derivation

```scala
import io.github.pomadchin.dynosaur.derivation.*
import dynosaur.Schema

// Dynosaur Schema derivation example
case class Document(id: String, text: Option[String], number: Long)

// leniency to nullability enabled
implicit val documentSchema: Schema[Document] = SchemaAuto.derive
// leniency to nullability disabled
implicit val documentSchema: Schema[Document] = SchemaAuto.derive(false)

// Dynosaur Schema derivation with a discriminator field; useful for the sealed families
sealed trait Documents
case class Document1(id: String, text: Option[String], number: Long) extends Documents
case class Document2(id: String, text: Option[String], number: Long) extends Documents

// leniency to nullability enabled
implicit val documentsSchema: Schema[Documents] = SchemaAuto.derive("discriminator")
// leniency to nullability disabled
implicit val documentsSchema: Schema[Documents] = SchemaAuto.derive("discriminator", false)
```


## License
Code is provided under the Apache 2.0 license available at http://opensource.org/licenses/Apache-2.0,
as well as in the LICENSE file.
