import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val catsV = "2.6.1"
val specs2V = "4.12.3"


// Docs
val catsEffectV = "3.2.1"
val kittensV = "2.3.0"

ThisBuild / crossScalaVersions := Seq("2.12.14", "2.13.4")

lazy val selection = project.in(file("."))
  .disablePlugins(MimaPlugin)
  .enablePlugins(NoPublishPlugin)
  .aggregate(coreJVM, coreJS)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "selection",
    libraryDependencies ++= Seq(
      "org.typelevel"               %%% "cats-core"                  % catsV,
      "org.typelevel"               %%% "cats-testkit"               % catsV         % Test,
      "org.typelevel"               %%% "discipline-specs2"          % "1.1.6"       % Test,
      ("org.specs2"                  %%% "specs2-scalacheck"          % specs2V       % Test)
        .cross(CrossVersion.for3Use2_13)
        .exclude("org.scalacheck", "scalacheck_2.13")
        .exclude("org.scalacheck", "scalacheck_sjs1_2.13")
    )
  )

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val site = project.in(file("site"))
  .disablePlugins(MimaPlugin)
  .enablePlugins(NoPublishPlugin)
  .enablePlugins(DavenverseMicrositePlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "kittens"      % kittensV,
      "org.typelevel" %% "cats-effect"  % catsEffectV
    ),
    micrositeDescription := "Functor Transformations for Scala",
  )
  .dependsOn(coreJVM)