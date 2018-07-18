package org.scalajs.jsenv.phantomjs

import scala.concurrent._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.io._

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv.test._

class RetryingComJSEnvTest {
  import RetryingComJSEnvTest._

  @Test
  def basicComTestWithRetries: Unit = {
    val failingJSEnv = new FailingEnv
    val config = JSEnvSuiteConfig(new RetryingComJSEnv(failingJSEnv))
    val kit = new TestComKit(config)

    val run = kit.start("""
      scalajsCom.init(function(msg) { scalajsCom.send("received: " + msg); });
      setTimeout(function() {
        scalajsCom.send("Hello World");
      }, 1000);
    """, RunConfig())

    try {
      Thread.sleep(200L)
      assertEquals(2, failingJSEnv.attempt)
      run.run.send("Initial")
      Thread.sleep(200L)
      assertEquals(3, failingJSEnv.attempt)
      assertEquals("received: Initial", run.waitNextMessage())

      assertEquals("Hello World", run.waitNextMessage())

      for (i <- 0 to 10) {
        run.run.send(i.toString)
        assertEquals(s"received: $i", run.waitNextMessage())
      }
    } finally {
      run.closeAndWait()
    }
  }

}

object RetryingComJSEnvTest {

  private final class FailingEnv extends JSEnv {
    private val baseEnv = new NodeJSEnv()

    val name: String = s"FailingJSEnv of ${baseEnv.name}"

    @volatile
    var attempt = 0

    def start(input: Input, config: RunConfig): JSRun =
      baseEnv.start(input, config)

    def startWithCom(input: Input, config: RunConfig,
        onMessage: String => Unit): JSComRun = {

      FailingEnv.validator.validate(config)

      attempt += 1
      attempt match {
        case 1 =>
          // An immediately failed JSComRun
          JSComRun.failed(
              new IllegalStateException("Dummy fail for testing purposes 1"))

        case 2 =>
          // A JSComRun that fails after a message has been sent
          new JSComRun {
            private[this] val promise = Promise[Unit]()

            def close(): Unit = promise.trySuccess(())
            def future: Future[Unit] = promise.future

            def send(msg: String): Unit = {
              promise.tryFailure(
                  new UnsupportedOperationException("Dummy fail for testing purposes 2"))
            }
          }

        case 3 =>
          // A correct run
          baseEnv.startWithCom(input, config, onMessage)

        case _ =>
          fail("Trying to retry a FailingEnv for more than 3 times")
          throw new Error("unreachable")
      }
    }
  }

  private object FailingEnv {
    private val validator = {
      RunConfig.Validator()
        .supportsInheritIO()
        .supportsOnOutputStream()
    }
  }

}
