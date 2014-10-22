import sbt._

import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import scala.scalajs.sbtplugin.env.phantomjs.PhantomJSEnv

object Build extends sbt.Build {

  lazy val root = Project(id = "bugRepro", base = file("."))
    .settings(scalaJSSettings:_*)
    .settings(
      requiresDOM := true,
      postLinkJSEnv := new PhantomJSEnv
    )

}
