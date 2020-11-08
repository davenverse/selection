import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val catsV = "2.2.0"
val catsScalaCheckV = "0.1.0"
// val specs2V = "4.10.5"

val kindProjectorV = "0.10.3"
val betterMonadicForV = "0.3.1"

// Docs
val catsEffectV = "2.2.0"
val kittensV = "2.2.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val selection = project
  .in(file("."))
  .disablePlugins(MimaPlugin)
  .settings(commonSettings, skipOnPublishSettings)
  .aggregate(coreJVM, coreJS, docs)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(commonSettings, mimaSettings)
  .settings(
    name := "selection"
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val docs = project
  .in(file("docs"))
  .disablePlugins(MimaPlugin)
  .settings(
    commonSettings,
    skipOnPublishSettings,
    micrositeSettings
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "kittens"     % kittensV,
      "org.typelevel" %% "cats-effect" % catsEffectV
    )
  )
  .dependsOn(coreJVM)
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(TutPlugin)

lazy val contributors = Seq(
  "ChristopherDavenport" -> "Christopher Davenport"
)

// General Settings
lazy val commonSettings = Seq(
  organization := "io.chrisdavenport",
  scalaVersion := "2.13.3",
  crossScalaVersions := Seq(scalaVersion.value, "2.12.11"),
  scalacOptions += "-Yrangepos",
  scalacOptions in (Compile, doc) ++= Seq(
    "-groups",
    "-sourcepath",
    (baseDirectory in LocalRootProject).value.getAbsolutePath,
    "-doc-source-url",
    "https://github.com/christopherdavenport/selection/blob/v" + version.value + "€{FILE_PATH}.scala"
  ),
  dependencyUpdatesFilter -= moduleFilter(organization =
    "org.scalacheck"
  ), // scalacheck-1.14 is incompatible with cats-laws-1.1
  dependencyUpdatesFilter -= moduleFilter(organization =
    "org.specs2"
  ), // specs2-4.2 is incompatible with scalacheck-1.13
  addCompilerPlugin(
    "org.typelevel" % "kind-projector" % kindProjectorV cross CrossVersion.binary
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForV),
  testFrameworks += new TestFramework("munit.Framework"),
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core"        % catsV,
    "org.typelevel" %%% "cats-testkit"     % catsV    % Test,
    "org.typelevel" %%% "discipline-munit" % "1.0.1"  % Test,
    "org.scalameta" %%% "munit"            % "0.7.16" % Test
  )
)

inThisBuild(
  List(
    organization := "io.chrisdavenport",
    homepage := Some(url("https://github.com/ChristopherDavenport/selection")),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    developers := 
      {
        (for ((username, name) <- contributors)
          yield Developer(username, name, "", url(s"http://github.com/$username"))).toList
      }
  )
)

// Not Used Currently
lazy val mimaSettings = {
  def mimaVersion(version: String) = {
    VersionNumber(version) match {
      case VersionNumber(Seq(major, minor, patch, _*), _, _) if patch.toInt > 0 =>
        Some(s"${major}.${minor}.${patch.toInt - 1}")
      case _ =>
        None
    }
  }

  Seq(
    mimaFailOnProblem := mimaVersion(version.value).isDefined,
    mimaFailOnNoPrevious in ThisBuild := false,
    mimaPreviousArtifacts := (mimaVersion(version.value) map {
      organization.value % s"${moduleName.value}_${scalaBinaryVersion.value}" % _
    }).toSet,
    mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      Seq()
    }
  )
}

lazy val micrositeSettings = {
  import microsites._
  Seq(
    micrositeName := "selection",
    micrositeDescription := "Functor Transformations for Scala",
    micrositeAuthor := "Christopher Davenport",
    micrositeGithubOwner := "ChristopherDavenport",
    micrositeGithubRepo := "selection",
    micrositeBaseUrl := "/selection",
    micrositeDocumentationUrl := "https://www.javadoc.io/doc/io.chrisdavenport/selection_2.12",
    micrositeFooterText := None,
    micrositeHighlightTheme := "atom-one-light",
    micrositePalette := Map(
      "brand-primary" -> "#3e5b95",
      "brand-secondary" -> "#294066",
      "brand-tertiary" -> "#2d5799",
      "gray-dark" -> "#49494B",
      "gray" -> "#7B7B7E",
      "gray-light" -> "#E5E5E6",
      "gray-lighter" -> "#F4F3F4",
      "white-color" -> "#FFFFFF"
    ),
    fork in tut := true,
    scalacOptions in Tut --= Seq(
      "-Xfatal-warnings",
      "-Ywarn-unused-import",
      "-Ywarn-numeric-widen",
      "-Ywarn-dead-code",
      "-Ywarn-unused:imports",
      "-Xlint:-missing-interpolator,_"
    ),
    libraryDependencies += "com.47deg" %% "github4s" % "0.26.0",
    micrositePushSiteWith := GitHub4s,
    micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
    micrositeExtraMdFiles := Map(
      file("CHANGELOG.md") -> ExtraMdFileConfig(
        "changelog.md",
        "page",
        Map(
          "title" -> "changelog",
          "section" -> "changelog",
          "position" -> "100"
        )
      ),
      file("CODE_OF_CONDUCT.md") -> ExtraMdFileConfig(
        "code-of-conduct.md",
        "page",
        Map(
          "title" -> "code of conduct",
          "section" -> "code of conduct",
          "position" -> "101"
        )
      ),
      file("LICENSE") -> ExtraMdFileConfig(
        "license.md",
        "page",
        Map("title" -> "license", "section" -> "license", "position" -> "102")
      )
    )
  )
}

lazy val skipOnPublishSettings = Seq(
  skip in publish := true,
  publish := (()),
  publishLocal := (()),
  publishArtifact := false,
  publishTo := None
)
