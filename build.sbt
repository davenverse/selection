import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val catsV = "1.6.1"
val catsScalaCheckV = "0.1.0"
val specs2V = "4.1.2"

val kindProjectorV = "0.10.3"
val betterMonadicForV = "0.3.1"

// Docs
val catsEffectV = "1.3.1"
val kittensV = "1.2.1"

lazy val selection = project.in(file("."))
  .disablePlugins(MimaPlugin)
  .settings(commonSettings, releaseSettings, skipOnPublishSettings)
  .aggregate(coreJVM, coreJS, docs)

lazy val core = (file("core") / crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure))
  .settings(commonSettings, releaseSettings, mimaSettings)
  .settings(
    name := "selection"
  )

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val docs = project.in(file("docs"))
  .disablePlugins(MimaPlugin)
  .settings(commonSettings, releaseSettings, skipOnPublishSettings, micrositeSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "kittens"      % kittensV,
      "org.typelevel" %% "cats-effect"  % catsEffectV
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

  scalaVersion := "2.12.15",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
  scalacOptions += "-Yrangepos",

  (Compile / doc / scalacOptions) ++= Seq(
      "-groups",
      "-sourcepath", (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-doc-source-url", "https://github.com/christopherdavenport/selection/blob/v" + version.value + "€{FILE_PATH}.scala"
  ),

  dependencyUpdatesFilter -= moduleFilter(organization = "org.scalacheck"), // scalacheck-1.14 is incompatible with cats-laws-1.1
  dependencyUpdatesFilter -= moduleFilter(organization = "org.specs2"), // specs2-4.2 is incompatible with scalacheck-1.13

  addCompilerPlugin("org.typelevel" % "kind-projector" % kindProjectorV cross CrossVersion.binary),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForV),
  
  libraryDependencies ++= Seq(
    "org.typelevel"               %%% "cats-core"                  % catsV,
    "org.typelevel"               %%% "cats-testkit"               % catsV         % Test,
    // "io.chrisdavenport"           %%% "cats-scalacheck"            % catsScalaCheckV % Test,
    "org.specs2"                  %%% "specs2-core"                % specs2V       % Test,
    "org.specs2"                  %%% "specs2-scalacheck"          % specs2V       % Test
  )
)

lazy val releaseSettings = {
  import ReleaseTransformations._
  Seq(
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      // For non cross-build projects, use releaseStepCommand("publishSigned")
      releaseStepCommandAndRemaining("+publishSigned"),
      setNextVersion,
      commitNextVersion,
      releaseStepCommand("sonatypeReleaseAll"),
      pushChanges
    ),
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials ++= (
      for {
        username <- Option(System.getenv().get("SONATYPE_USERNAME"))
        password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
      } yield
        Credentials(
          "Sonatype Nexus Repository Manager",
          "oss.sonatype.org",
          username,
          password
        )
    ).toSeq,
    (Test / publishArtifact) := false,
    releasePublishArtifactsAction := PgpKeys.publishSigned.value,
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/ChristopherDavenport/selection"),
        "git@github.com:ChristopherDavenport/selection.git"
      )
    ),
    homepage := Some(url("https://github.com/ChristopherDavenport/selection")),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    publishMavenStyle := true,
    pomIncludeRepository := { _ =>
      false
    },
    pomExtra := {
      <developers>
        {for ((username, name) <- contributors) yield
        <developer>
          <id>{username}</id>
          <name>{name}</name>
          <url>http://github.com/{username}</url>
        </developer>
        }
      </developers>
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
    (tut / fork) := true,
    (Tut / scalacOptions) --= Seq(
      "-Xfatal-warnings",
      "-Ywarn-unused-import",
      "-Ywarn-numeric-widen",
      "-Ywarn-dead-code",
      "-Ywarn-unused:imports",
      "-Xlint:-missing-interpolator,_"
    ),
    libraryDependencies += "com.47deg" %% "github4s" % "0.20.1",
    micrositePushSiteWith := GitHub4s,
    micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
    micrositeExtraMdFiles := Map(
        file("CHANGELOG.md")        -> ExtraMdFileConfig("changelog.md", "page", Map("title" -> "changelog", "section" -> "changelog", "position" -> "100")),
        file("CODE_OF_CONDUCT.md")  -> ExtraMdFileConfig("code-of-conduct.md",   "page", Map("title" -> "code of conduct",   "section" -> "code of conduct",   "position" -> "101")),
        file("LICENSE")             -> ExtraMdFileConfig("license.md",   "page", Map("title" -> "license",   "section" -> "license",   "position" -> "102"))
    )
  )
}

lazy val mimaSettings = {
  import sbtrelease.Version

  def semverBinCompatVersions(major: Int, minor: Int, patch: Int): Set[(Int, Int, Int)] = {
    val majorVersions: List[Int] = List(major)
    val minorVersions : List[Int] = 
      if (major >= 1) Range(0, minor).inclusive.toList
      else List(minor)
    def patchVersions(currentMinVersion: Int): List[Int] = 
      if (minor == 0 && patch == 0) List.empty[Int]
      else if (currentMinVersion != minor) List(0)
      else Range(0, patch - 1).inclusive.toList

    val versions = for {
      maj <- majorVersions
      min <- minorVersions
      pat <- patchVersions(min)
    } yield (maj, min, pat)
    versions.toSet
  }

  def mimaVersions(version: String): Set[String] = {
    Version(version) match {
      case Some(Version(major, Seq(minor, patch), _)) =>
        semverBinCompatVersions(major.toInt, minor.toInt, patch.toInt)
          .map{case (maj, min, pat) => maj.toString + "." + min.toString + "." + pat.toString}
      case _ =>
        Set.empty[String]
    }
  }
  // Safety Net For Exclusions
  lazy val excludedVersions: Set[String] = Set()

  // Safety Net for Inclusions
  lazy val extraVersions: Set[String] = Set()

  Seq(
    mimaFailOnNoPrevious := false,
    mimaFailOnProblem := mimaVersions(version.value).toList.headOption.isDefined,
    mimaPreviousArtifacts := (mimaVersions(version.value) ++ extraVersions)
      .filterNot(excludedVersions.contains(_))
      .map{v => 
        val moduleN = moduleName.value + "_" + scalaBinaryVersion.value.toString
        organization.value % moduleN % v
      },
    mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      Seq()
    }
  )
}

lazy val skipOnPublishSettings = Seq(
  (publish / skip) := true,
  publish := (()),
  publishLocal := (()),
  publishArtifact := false,
  publishTo := None
)
