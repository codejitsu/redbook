import sbt._
import sbt.Keys._

object ProjectBuild extends Build {
  import Settings._

  lazy val root = Project(
    id = "redbook",
    base = file("."),
    settings = parentSettings ++ defaultSettings ++ Seq(libraryDependencies ++= Dependencies.redbook)
  )
}

object Dependencies {
  import Versions._

  object Compile {
  }

  object Test {
    val scalatest     = "org.scalatest"           %% "scalatest"            % ScalaTestVer      % "test"
    val scalacheck    = "org.scalacheck"          %% "scalacheck"           % ScalaCheckVer     % "test"
  }

  import Compile._
  import Test._

  val test = Seq(scalatest, scalacheck)

  /** Module deps */

  val redbook = test
}
