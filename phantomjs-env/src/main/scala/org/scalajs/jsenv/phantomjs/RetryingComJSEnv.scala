/*                     __                                                   *\
**     ________ ___   / /  ___      __ ____  PhantomJS support for Scala.js **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL       **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    https://www.scala-js.org/      **
** /____/\___/_/ |_/____/_/ | |__/ /____/                                   **
**                          |/____/                                         **
\*                                                                          */

package org.scalajs.jsenv.phantomjs

import org.scalajs.io._

import org.scalajs.logging.Logger

import org.scalajs.jsenv._

import scala.concurrent.{Future, Promise, ExecutionContext}
import scala.concurrent.duration.Duration
import scala.collection.mutable
import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{Try, Failure, Success}

/** A RetryingComJSEnv allows to automatically retry if a call to the underlying
 *  ComJSRunner fails.
 *
 *  While it protects the JVM side from observing state that differs inbetween
 *  runs that have been retried, it assumes that the executed JavaScript code
 *  does not have side-effects other than the ones visible through the channel
 *  (e.g. writing to a file). It is the users responsibility to ensure this
 *  property.
 *
 *  No retrying is performed for non-com runs (initiated with `start`).
 *
 *  Although `RetryingComJSEnv` is agnostic of the underlying JS env, and is
 *  therefore not tied to PhantomJS, it is most often used to compensate for
 *  flakiness effects of PhantomJS.
 */
final class RetryingComJSEnv(val baseEnv: JSEnv, val maxRetries: Int)
    extends JSEnv {

  def this(baseEnv: JSEnv) = this(baseEnv, 5)

  val name: String = s"Retrying ${baseEnv.name}"

  def start(input: Input, runConfig: RunConfig): JSRun =
    baseEnv.start(input, runConfig)

  def startWithCom(input: Input, runConfig: RunConfig,
      onMessage: String => Unit): JSComRun = {
    new RetryingJSComRun(input, runConfig, onMessage)
  }

  private class RetryingJSComRun(input: Input, runConfig: RunConfig,
      onMessage: String => Unit)
      extends JSComRun {

    private[this] val promise = Promise[Unit]

    def future: Future[Unit] = promise.future

    private[this] var hasReceived = false
    private[this] var retryCount = 0

    private[this] val log = mutable.Buffer.empty[LogItem]

    private[this] var curRun: JSComRun = _
    startNewRun()

    private def startNewRun(): Unit = {
      import ExecutionContext.Implicits.global

      val myRetryCount = retryCount

      val run = baseEnv.startWithCom(input, runConfig, { msg =>
        val doForward = synchronized {
          if (retryCount == myRetryCount) {
            /* At this point, we are sending state to the JVM, we cannot retry
             * after this.
             */
            hasReceived = true
            true
          } else {
            false
          }
        }
        if (doForward)
          onMessage(msg) // outside of synchronized
      })
      curRun = run

      run.future.onComplete { result =>
        synchronized {
          if (retryCount == myRetryCount) {
            result match {
              case Failure(_) if !hasReceived && retryCount < maxRetries =>
                retryCount += 1
                startNewRun()
                log.foreach(executeTask)

              case _ =>
                promise.complete(result)
            }
          }
        }
      }
    }

    def send(msg: String): Unit =
      logAndDo(Send(msg))

    def close(): Unit =
      logAndDo(Close)

    private def logAndDo(task: LogItem): Unit = synchronized {
      log += task
      executeTask(task)
    }

    /** Async, nothrow. */
    private def executeTask(task: LogItem): Unit = task match {
      case Send(msg) =>
        curRun.send(msg)
      case Close =>
        curRun.close()
    }

    private sealed trait LogItem
    private case class Send(msg: String) extends LogItem
    private case object Close extends LogItem

  }

}
