/*                     __                                                   *\
**     ________ ___   / /  ___      __ ____  PhantomJS support for Scala.js **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL       **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    https://www.scala-js.org/      **
** /____/\___/_/ |_/____/_/ | |__/ /____/                                   **
**                          |/____/                                         **
\*                                                                          */

package org.scalajs.jsenv.phantomjs

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.util.control.NonFatal

import java.io._
import java.net._
import java.nio.file.{Files, StandardCopyOption}

import org.scalajs.jsenv._

import org.scalajs.io._
import org.scalajs.io.URIUtils.fixFileURI
import org.scalajs.io.JSUtils.escapeJS

private final class ComRun(jettyClassLoader: ClassLoader,
    runConfig: RunConfig, onMessage: String => Unit,
    startRun: VirtualBinaryFile => JSRun)
    extends JSComRun {

  import ComRun._

  import runConfig.logger

  private val promise = Promise[Unit]()

  def future: Future[Unit] = promise.future

  private val sendBuffer = mutable.ListBuffer.empty[String]
  private var canSendNow: Boolean = false

  private def loadMgr(): WebsocketManager = {
    val loader =
      if (jettyClassLoader != null) jettyClassLoader
      else getClass().getClassLoader()

    val clazz = loader.loadClass(
        "org.scalajs.jsenv.phantomjs.JettyWebsocketManager")

    val ctors = clazz.getConstructors()
    assert(ctors.length == 1, "JettyWebsocketManager may only have one ctor")

    val listener = new WebsocketListener {
      def onRunning(): Unit = startPhantomJSProcess()
      def onOpen(): Unit = sendPendingMessages()
      def onClose(): Unit = ()
      def onMessage(msg: String): Unit = receiveFrag(msg)
      def log(msg: String): Unit = logger.debug(s"PhantomJS WS Jetty: $msg")
    }

    val mgr = ctors.head.newInstance(listener)

    mgr.asInstanceOf[WebsocketManager]
  }

  private val mgr: WebsocketManager = loadMgr()

  private[this] val fragmentsBuf = new StringBuilder

  // Constructor
  mgr.start()

  private def startPhantomJSProcess(): Unit = synchronized {
    import ExecutionContext.Implicits.global

    val comSetup = makeComSetupFile()
    val underlyingRun = startRun(comSetup)
    underlyingRun.future.onComplete { result =>
      promise.tryComplete(result)
    }
  }

  private def sendPendingMessages(): Unit = synchronized {
    canSendNow = true
    val messages = sendBuffer.toList
    sendBuffer.clear()
    messages.foreach(sendNow)
  }

  private def makeComSetupFile(): VirtualBinaryFile = {
    def maybeExit(code: Int) =
      if (true /*config.autoExit*/)
        s"window.callPhantom({ action: 'exit', returnValue: $code });"
      else
        ""

    val serverPort = mgr.localPort
    assert(serverPort > 0,
        s"Manager running with a non-positive port number: $serverPort")

    val code = s"""
      |(function() {
      |  var MaxPayloadSize = $MaxCharPayloadSize;
      |
      |  // The socket for communication
      |  var websocket = null;
      |
      |  // Buffer for messages sent before socket is open
      |  var outMsgBuf = null;
      |
      |  function sendImpl(msg) {
      |    var frags = (msg.length / MaxPayloadSize) | 0;
      |
      |    for (var i = 0; i < frags; ++i) {
      |      var payload = msg.substring(
      |          i * MaxPayloadSize, (i + 1) * MaxPayloadSize);
      |      websocket.send("1" + payload);
      |    }
      |
      |    websocket.send("0" + msg.substring(frags * MaxPayloadSize));
      |  }
      |
      |  function recvImpl(recvCB) {
      |    var recvBuf = "";
      |
      |    return function(evt) {
      |      var newData = recvBuf + evt.data.substring(1);
      |      if (evt.data.charAt(0) == "0") {
      |        recvBuf = "";
      |        recvCB(newData);
      |      } else if (evt.data.charAt(0) == "1") {
      |        recvBuf = newData;
      |      } else {
      |        throw new Error("Bad fragmentation flag in " + evt.data);
      |      }
      |    };
      |  }
      |
      |  window.scalajsCom = {
      |    init: function(recvCB) {
      |      if (websocket !== null) throw new Error("Com already open");
      |
      |      outMsgBuf = [];
      |
      |      websocket = new WebSocket("ws://localhost:$serverPort");
      |
      |      websocket.onopen = function(evt) {
      |        for (var i = 0; i < outMsgBuf.length; ++i)
      |          sendImpl(outMsgBuf[i]);
      |        outMsgBuf = null;
      |      };
      |      websocket.onclose = function(evt) {
      |        websocket = null;
      |        if (outMsgBuf !== null)
      |          throw new Error("WebSocket closed before being opened: " + evt);
      |        ${maybeExit(0)}
      |      };
      |      websocket.onmessage = recvImpl(recvCB);
      |      websocket.onerror = function(evt) {
      |        websocket = null;
      |        throw new Error("Websocket failed: " + evt);
      |      };
      |
      |      // Take over responsibility to auto exit
      |      window.callPhantom({
      |        action: 'setAutoExit',
      |        autoExit: false
      |      });
      |    },
      |    send: function(msg) {
      |      if (websocket === null)
      |        return; // we are closed already. ignore message
      |
      |      if (outMsgBuf !== null)
      |        outMsgBuf.push(msg);
      |      else
      |        sendImpl(msg);
      |    },
      |    close: function() {
      |      if (websocket === null)
      |        return; // we are closed already. all is well.
      |
      |      if (outMsgBuf !== null)
      |        // Reschedule ourselves to give onopen a chance to kick in
      |        window.setTimeout(window.scalajsCom.close, 10);
      |      else
      |        websocket.close();
      |    }
      |  }
      |}).call(this);""".stripMargin

    MemVirtualBinaryFile.fromStringUTF8("comSetup.js", code)
  }

  def send(msg: String): Unit = synchronized {
    if (canSendNow)
      sendNow(msg)
    else
      sendBuffer += msg
  }

  private def sendNow(msg: String): Unit = {
    val fragParts = msg.length / MaxCharPayloadSize

    for (i <- 0 until fragParts) {
      val payload = msg.substring(
          i * MaxCharPayloadSize, (i + 1) * MaxCharPayloadSize)
      mgr.sendMessage("1" + payload)
    }

    mgr.sendMessage("0" + msg.substring(fragParts * MaxCharPayloadSize))
  }

  private def receiveFrag(frag: String): Unit = synchronized {
    /* The fragments are accumulated in an instance-wide buffer in case
     * receiving a non-first fragment times out.
     */
    fragmentsBuf ++= frag.substring(1)

    frag.charAt(0) match {
      case '0' =>
        // Last fragment of a message, send it
        val result = fragmentsBuf.result()
        fragmentsBuf.clear()
        onMessage(result)

      case '1' =>
        // There are more fragments to come; do nothing

      case _ =>
        throw new AssertionError("Bad fragmentation flag in " + frag)
    }
  }

  def close(): Unit = mgr.stop()
}

object ComRun {
  private final val MaxByteMessageSize = 32768 // 32 KB
  private final val MaxCharMessageSize = MaxByteMessageSize / 2 // 2B per char
  private final val MaxCharPayloadSize = MaxCharMessageSize - 1 // frag flag

  /** Starts a [[JSComRun]] using the provided [[JSRun]] launcher.
   *
   *  @param jettyClassLoader A ClassLoader to isolate jetty.
   *  @param config Configuration for the run.
   *  @param onMessage callback upon message reception.
   *  @param startRun [[JSRun]] launcher. Gets passed a
   *      [[org.scalajs.io.VirtualBinaryFile VirtualBinaryFile]] that
   *      initializes `scalaJSCom` on `global`. Requires PhantomJS libraries.
   */
  def start(jettyClassLoader: ClassLoader, config: RunConfig,
      onMessage: String => Unit)(
      startRun: VirtualBinaryFile => JSRun): JSComRun = {
    new ComRun(jettyClassLoader, config, onMessage, startRun)
  }

  /** Starts a [[JSComRun]] using the provided [[JSRun]] launcher.
   *
   *  @param config Configuration for the run.
   *  @param onMessage callback upon message reception.
   *  @param startRun [[JSRun]] launcher. Gets passed a
   *      [[org.scalajs.io.VirtualBinaryFile VirtualBinaryFile]] that
   *      initializes `scalaJSCom` on `global`. Requires PhantomJS libraries.
   */
  def start(config: RunConfig, onMessage: String => Unit)(
      startRun: VirtualBinaryFile => JSRun): JSComRun = {
    start(null, config, onMessage)(startRun)
  }
}
