val scala213 = "2.13.16"
val scala3 = "3.7.0"
val scalaVersions = Seq(scala213, scala3)

lazy val commonSettings = Seq(
  scalaVersion := scalaVersions.head,
  crossScalaVersions := scalaVersions,
  organization := "io.github.pomadchin",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:reflectiveCalls",
    "-language:higherKinds",
    "-language:postfixOps",
    "-language:existentials",
    "-feature"
  ),
  scalacOptions ++= when(scalaBinaryVersion.value == "2.13")("-Ymacro-annotations", "-Xsource:3"),
  scalacOptions ++= (scalaBinaryVersion.value match {
    case "3" => Nil
    case _ => List("-Xsource:3")
  }),
  Test / scalacOptions ++= when(scalaBinaryVersion.value.startsWith("3"))("-experimental"),
  description := "Dynosaur derivation library",
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
  homepage := Some(url("https://github.com/pomadchin/dynosaur-derivation")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/pomadchin/dynosaur-derivation"), "scm:git:git@github.com:pomadchin/dynosaur-derivation.git")
  ),
  versionScheme := Some("semver-spec"),
  Test / publishArtifact := false,
  Test / fork := true,
  developers := List(
    Developer(
      "pomadchin",
      "Grigory Pomadchin",
      "@pomadchin",
      url("https://github.com/pomadchin")
    )
  )
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(name := "dynosaur-derivation-root")
  .settings(publish / skip := true, publishLocal / skip := true)
  .aggregate(derivation)

lazy val derivation = project
  .settings(commonSettings)
  .settings(name := "dynosaur-derivation")
  .settings(
    libraryDependencies ++= Seq(
      "org.systemfw" %% "dynosaur-core" % "0.7.1",
      "org.typelevel" %% "cats-core" % "2.13.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
  .settings(
    libraryDependencies ++= when(scalaBinaryVersion.value == "2.13")(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

def when[A](condition: Boolean)(values: A*): Seq[A] =
  if (condition) values else Nil
