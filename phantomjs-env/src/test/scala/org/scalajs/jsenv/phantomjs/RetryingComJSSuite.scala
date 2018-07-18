package org.scalajs.jsenv.phantomjs

import scala.concurrent.Future

import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv._
import org.scalajs.jsenv.test._

import org.junit.runner.RunWith

@RunWith(classOf[JSEnvSuiteRunner])
class RetryingComJSSuite extends JSEnvSuite(RetryingComJSSuite.Config)

object RetryingComJSSuite {

  private final val maxFails = 5

  val Config = {
    val jsEnv = new RetryingComJSEnv(new NodeJSEnv, maxFails)
    JSEnvSuiteConfig(jsEnv)
      .withTerminateVMJSCode("__ScalaJSEnv.exitFunction(0)")
      .withSupportsTimeout(false)
  }

}
