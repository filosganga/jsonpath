val circeV = "0.14.6"

val catsParseV = "1.0.0"

val catsV = "2.10.0"

val catsEffectV = "3.4.8"

val munitV = "1.0.0-M12"

val munitCatsEffectV = "2.0.0-M5"

val literallyV = "1.1.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.3.3"
ThisBuild / crossScalaVersions ++= List("2.13.13")
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

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / publishMavenStyle := true,
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / publishMavenStyle := true
ThisBuild / credentials ++= {
  for {
    usr <- sys.env.get("SONATYPE_USER")
    password <- sys.env.get("SONATYPE_PASS")
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    "s01.oss.sonatype.org",
    usr,
    password
  )
}.toList
ThisBuild / versionScheme := Some("semver-spec")

lazy val noPublishSettings = List(
  publish := {},
  publishLocal := {},
  publishTo := None,
  publishArtifact := false,
  publish / skip := true
)

val scalacOptionsSettings = List(
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions += "-Xsource:3"
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
        "org.scalameta" %%% "munit-scalacheck" % munitV % Test,
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
        "org.scalameta" %%% "munit-scalacheck" % munitV % Test,
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
        "org.scalameta" %%% "munit-scalacheck" % munitV % Test,
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
        "org.scalameta" %%% "munit-scalacheck" % munitV % Test,
        "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test
      )
    )
  // .jsSettings(
  //   scalaJSUseMainModuleInitializer := false
  // )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
