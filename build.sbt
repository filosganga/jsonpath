import org.typelevel.scalacoptions.ScalacOptions

val circeV = "0.14.15"

val catsParseV = "1.1.0"

val catsV = "2.13.0"

val catsEffectV = "3.4.8"

val munitV = "1.2.1"

val munitScalacheckV = "1.2.0"

val munitCatsEffectV = "2.1.0"

val literallyV = "1.2.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.7.4"
ThisBuild / crossScalaVersions ++= List("2.13.18")
ThisBuild / organization := "com.filippodeluca"
ThisBuild / organizationName := "Filippo De Luca"

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

ThisBuild / developers := List(
  Developer(
    id = "filosganga",
    name = "Filippo De Luca",
    email = "me@filippodeluca.com",
    url = url("https://github.com/filosganga")
  )
)
ThisBuild / licenses := List(License.Apache2)
ThisBuild / startYear := Some(2023)
ThisBuild / homepage := Some(url("https://github.com/filosganga/jsonpath"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/filosganga/jsonpath"),
    "scm:git@github.com:filosganga/jsonpath.git"
  )
)

ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true
ThisBuild / sonatypeCredentialHost := xerial.sbt.Sonatype.sonatypeCentralHost
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / credentials ++= {
  for {
    usr <- sys.env.get("SONATYPE_USER")
    password <- sys.env.get("SONATYPE_PASS")
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    "central.sonatype.com",
    usr,
    password
  )
}.toList

val scalacOptionsSettings = List(
  tpolecatScalacOptions ++= Set(
    ScalacOptions.source3
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "jsonpath",
    publish / skip := true
  )
  .aggregate(ast, parser, literal, circe)

lazy val ast =
  project // (JSPlatform, JVMPlatform /* cats-parse native does not exist  , NativePlatform */ )
    .in(file("modules/ast"))
    .settings(
      name := "jsonpath-ast",
      scalacOptionsSettings,
      libraryDependencies ++= List(
        "org.typelevel" %%% "cats-core" % catsV,
        "org.scalameta" %%% "munit" % munitV % Test,
        "org.scalameta" %%% "munit-scalacheck" % munitScalacheckV % Test,
        "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test
      )
    )

lazy val parser =
  project // (JSPlatform, JVMPlatform /* cats-parse native does not exist  , NativePlatform */ )
    .in(file("modules/parser"))
    .dependsOn(ast)
    .settings(
      name := "jsonpath-parser",
      scalacOptionsSettings,
      libraryDependencies ++= List(
        "org.typelevel" %%% "cats-parse" % catsParseV,
        "org.scalameta" %%% "munit" % munitV % Test,
        "org.scalameta" %%% "munit-scalacheck" % munitScalacheckV % Test,
        "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test
      )
    )

lazy val literal =
  project // (JSPlatform, JVMPlatform /* cats-parse native does not exist  , NativePlatform */ )
    .in(file("modules/literal"))
    .dependsOn(ast, parser)
    .settings(
      name := "jsonpath-literal",
      scalacOptionsSettings,
      libraryDependencies ++= List(
        "org.typelevel" %%% "literally" % literallyV,
        "org.scalameta" %%% "munit" % munitV % Test,
        "org.scalameta" %%% "munit-scalacheck" % munitScalacheckV % Test,
        "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test
      )
    )

lazy val circe =
  project // (JSPlatform, JVMPlatform /* cats-parse native does not exist  , NativePlatform */ )
    // .crossType(CrossType.Pure)
    .in(file("modules/circe"))
    .dependsOn(ast)
    // See https://github.com/portable-scala/sbt-crossproject/issues/102
    // .jsConfigure(
    //   _.dependsOn(core.jvm)
    // )
    // .jsConfigure {
    //   _.dependsOn(core.js)
    // }
    .settings(
      name := "jsonpath-circe",
      scalacOptionsSettings,
      libraryDependencies ++= List(
        "io.circe" %%% "circe-core" % circeV,
        "io.circe" %%% "circe-parser" % circeV,
        "io.circe" %%% "circe-testing" % circeV % Test,
        "io.circe" %%% "circe-literal" % circeV % Test
      ),
      libraryDependencies ++= List(
        "org.scalameta" %%% "munit" % munitV % Test,
        "org.scalameta" %%% "munit-scalacheck" % munitScalacheckV % Test,
        "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test
      )
    )
// .jsSettings(
//   scalaJSUseMainModuleInitializer := false
// )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
