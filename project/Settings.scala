import de.johoop.jacoco4sbt.JacocoPlugin.jacoco
import de.johoop.jacoco4sbt.Thresholds
import org.scalastyle.sbt.ScalastylePlugin
import sbt._
import sbt.Keys._
import scala.language.postfixOps

object Settings extends Build {
  lazy val buildSettings = Seq(
    name                  := "redbook",
    normalizedName        := "redbook",
    organization          := "codejitsu",
    organizationHomepage  := Some(url("http://www.codejitsu.net")),
    scalaVersion          := Versions.ScalaVer,
    homepage              := Some(url("http://www.github.com/codejitsu/redbook"))
  )

  override lazy val settings = super.settings ++ buildSettings

  val parentSettings = buildSettings ++ Seq(
    publishArtifact := false,
    publish         := {}
  )

  val scalacSettings = Seq("-encoding", "UTF-8", s"-target:jvm-${Versions.JDKVer}", "-feature", "-language:_",
    "-deprecation", "-unchecked", "-Xfatal-warnings", "-Xlint")

  val javacSettings = Seq("-encoding", "UTF-8", "-source", Versions.JDKVer,
    "-target", Versions.JDKVer, "-Xlint:deprecation", "-Xlint:unchecked")

  lazy val defaultSettings = testSettings ++ Seq(
    autoCompilerPlugins := true,
    //libraryDependencies <+= scalaVersion { v => compilerPlugin("org.brianmckenna" %% "wartremover" % Versions.WartremoverVer) },
    scalacOptions       ++= scalacSettings,
    //scalacOptions       += "-P:wartremover:traverser:org.brianmckenna.wartremover.warts.Unsafe",
    javacOptions        in Compile    ++= javacSettings,
    ivyLoggingLevel     in ThisBuild  := UpdateLogging.Quiet,
    parallelExecution   in ThisBuild  := false,
    parallelExecution   in Global     := false,
    ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }
  )

  val tests = inConfig(Test)(Defaults.testTasks) ++ inConfig(IntegrationTest)(Defaults.itSettings)

  val testOptionSettings = Seq(
    Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    Tests.Argument(TestFrameworks.JUnit, "-oDF", "-v", "-a")
  )

  lazy val testSettings = tests ++ jacoco.settings ++ Seq(
    parallelExecution in Test             := false,
    parallelExecution in IntegrationTest  := false,
    testOptions       in Test             ++= testOptionSettings,
    testOptions       in IntegrationTest  ++= testOptionSettings,
    fork              in Test             := true,
    fork              in IntegrationTest  := true,
    (compile in IntegrationTest)        <<= (compile in Test, compile in IntegrationTest) map { (_, c) => c },
    managedClasspath in IntegrationTest <<= Classpaths.concat(managedClasspath in IntegrationTest, exportedProducts in Test),
    jacoco.thresholds in jacoco.Config := Thresholds(instruction = 70, method = 70, branch = 70, complexity = 70, line = 70, clazz = 70)
  )
}
