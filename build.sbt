val circeV = "0.14.5"
val catsParseV = "0.3.7"
val catsV = "2.9.0"
val catsEffectV = "3.4.8"
val munitV = "1.0.0-M7"
val munitCatsEffectV = "2.0.0-M3"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.2.0"
ThisBuild / crossScalaVersions ++= List("2.13.10", "2.12.17")
ThisBuild / organization := "com.filippodeluca"
ThisBuild / organizationName := "Filippo De Luca"

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

lazy val root = (project in file("."))
  .settings(
    name := "jsonpath"
  )
  .aggregate(core, circe)

lazy val core =
  project // (JSPlatform, JVMPlatform /* cats-parse native does not exist  , NativePlatform */ )
    .in(file("modules/core"))
    .settings(
      name := "jsonpath-core",
      scalacOptions -= "-Xfatal-warnings",
      scalacOptions += "-Xsource:3",
      libraryDependencies ++= List(
        "org.typelevel" %%% "cats-parse" % catsParseV,
        "org.scalameta" %%% "munit" % munitV % Test,
        "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test
      )
    )
  // .jsSettings(
  //   scalaJSUseMainModuleInitializer := false
  // )

lazy val circe =
  project // (JSPlatform, JVMPlatform /* cats-parse native does not exist  , NativePlatform */ )
    // .crossType(CrossType.Pure)
    .in(file("modules/circe"))
    .dependsOn(core)
    // See https://github.com/portable-scala/sbt-crossproject/issues/102
    // .jsConfigure(
    //   _.dependsOn(core.jvm)
    // )
    // .jsConfigure {
    //   _.dependsOn(core.js)
    // }
    .settings(
      name := "jsonpath-circe",
      scalacOptions -= "-Xfatal-warnings",
      scalacOptions += "-Xsource:3",
      libraryDependencies ++= List(
        "io.circe" %%% "circe-core" % circeV,
        "org.scalameta" %%% "munit" % munitV % Test,
        "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test
      )
    )
  // .jsSettings(
  //   scalaJSUseMainModuleInitializer := false
  // )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
