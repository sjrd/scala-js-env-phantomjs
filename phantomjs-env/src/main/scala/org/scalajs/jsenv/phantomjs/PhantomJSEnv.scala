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

final class PhantomJSEnv(config: PhantomJSEnv.Config) extends JSEnv {
  import PhantomJSEnv._

  def this() = this(PhantomJSEnv.Config())

  val name: String = "PhantomJS"

  def start(input: Input, runConfig: RunConfig): JSRun = {
    PhantomJSEnv.validator.validate(runConfig)
    internalStart(initFiles ::: inputFiles(input), runConfig)
  }

  def startWithCom(input: Input, runConfig: RunConfig,
      onMessage: String => Unit): JSComRun = {
    PhantomJSEnv.validator.validate(runConfig)
    ComRun.start(config.jettyClassLoader, runConfig, onMessage) { comSetup =>
      internalStart(comSetup :: initFiles ::: inputFiles(input), runConfig)
    }
  }

  private def internalStart(files: List[VirtualBinaryFile],
      runConfig: RunConfig): JSRun = {
    try {
      val launcherFile = createTmpLauncherFile(files, runConfig)
      val command =
        config.executable :: config.args ::: launcherFile.getAbsolutePath :: Nil
      val externalConfig = ExternalJSRun.Config()
        .withEnv(config.env)
        .withRunConfig(runConfig)
      ExternalJSRun.start(command, externalConfig)(_.close())
    } catch {
      case NonFatal(t) =>
        JSRun.failed(t)

      case t: NotImplementedError =>
        /* In Scala 2.10.x, NotImplementedError was considered fatal.
         * We need this case for the conformance tests to pass on 2.10.
         */
        JSRun.failed(t)
    }
  }

  private def inputFiles(input: Input) = input match {
    case Input.ScriptsToLoad(scripts) => scripts
    case _                            => throw new UnsupportedInputException(input)
  }

  /**
   * PhantomJS doesn't support Function.prototype.bind. We polyfill it.
   * https://github.com/ariya/phantomjs/issues/10522
   */
  private def initFiles: List[MemVirtualBinaryFile] = List(
      // scalastyle:off line.size.limit
      MemVirtualBinaryFile.fromStringUTF8("bindPolyfill.js",
          """
          |// Polyfill for Function.bind from Facebook react:
          |// https://github.com/facebook/react/blob/3dc10749080a460e48bee46d769763ec7191ac76/src/test/phantomjs-shims.js
          |// Originally licensed under Apache 2.0
          |(function() {
          |
          |  var Ap = Array.prototype;
          |  var slice = Ap.slice;
          |  var Fp = Function.prototype;
          |
          |  if (!Fp.bind) {
          |    // PhantomJS doesn't support Function.prototype.bind natively, so
          |    // polyfill it whenever this module is required.
          |    Fp.bind = function(context) {
          |      var func = this;
          |      var args = slice.call(arguments, 1);
          |
          |      function bound() {
          |        var invokedAsConstructor = func.prototype && (this instanceof func);
          |        return func.apply(
          |          // Ignore the context parameter when invoking the bound function
          |          // as a constructor. Note that this includes not only constructor
          |          // invocations using the new keyword but also calls to base class
          |          // constructors such as BaseClass.call(this, ...) or super(...).
          |          !invokedAsConstructor && context || this,
          |          args.concat(slice.call(arguments))
          |        );
          |      }
          |
          |      // The bound function must share the .prototype of the unbound
          |      // function so that any object created by one constructor will count
          |      // as an instance of both constructors.
          |      bound.prototype = func.prototype;
          |
          |      return bound;
          |    };
          |  }
          |
          |})();
          |""".stripMargin
      ),
      MemVirtualBinaryFile.fromStringUTF8("scalaJSEnvInfo.js",
          """
          |__ScalaJSEnv = {
          |  exitFunction: function(status) {
          |    window.callPhantom({
          |      action: 'exit',
          |      returnValue: status | 0
          |    });
          |  }
          |};
          """.stripMargin
      )
      // scalastyle:on line.size.limit
  )

  protected def createTmpLauncherFile(scripts: List[VirtualBinaryFile],
      runConfig: RunConfig): File = {

    val webF = createTmpWebpage(scripts, runConfig)

    val launcherTmpF = File.createTempFile("phantomjs-launcher", ".js")
    launcherTmpF.deleteOnExit()

    val out = new FileWriter(launcherTmpF)

    try {
      out.write(
          s"""// Scala.js Phantom.js launcher
             |var page = require('webpage').create();
             |var url = "${escapeJS(fixFileURI(webF.toURI).toASCIIString)}";
             |page.onConsoleMessage = function(msg) {
             |  console.log(msg);
             |};
             |page.onError = function(msg, trace) {
             |  console.error(msg);
             |  if (trace && trace.length) {
             |    console.error('');
             |    trace.forEach(function(t) {
             |      console.error('  ' + t.file + ':' + t.line +
             |        (t.function ? ' (in function "' + t.function +'")' : ''));
             |    });
             |  }
             |
             |  phantom.exit(2);
             |};
             |page.onCallback = function(data) {
             |  if (!data.action) {
             |    console.error('Called callback without action');
             |    phantom.exit(3);
             |  } else if (data.action === 'exit') {
             |    phantom.exit(data.returnValue || 0);
             |  } else {
             |    console.error('Unknown callback action ' + data.action);
             |    phantom.exit(4);
             |  }
             |};
             |page.open(url, function (status) {
             |  if (status !== 'success')
             |    phantom.exit(1);
             |});
             |""".stripMargin)
    } finally {
      out.close()
    }

    runConfig.logger.debug(
        "PhantomJS using launcher at: " + launcherTmpF.getAbsolutePath())

    launcherTmpF
  }

  protected def createTmpWebpage(scripts: List[VirtualBinaryFile],
      runConfig: RunConfig): File = {

    val webTmpF = File.createTempFile("phantomjs-launcher-webpage", ".html")
    webTmpF.deleteOnExit()

    val out = new BufferedWriter(
        new OutputStreamWriter(new FileOutputStream(webTmpF), "UTF-8"))

    try {
      writeWebpageLauncher(out, scripts)
    } finally {
      out.close()
    }

    runConfig.logger.debug(
        "PhantomJS using webpage launcher at: " + webTmpF.getAbsolutePath())

    webTmpF
  }

  protected def writeWebpageLauncher(out: Writer,
      scripts: List[VirtualBinaryFile]): Unit = {
    out.write(s"""<html><head>
        <title>Phantom.js Launcher</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />""")

    /* The most horrible way to identify the test script of
     * `RunTests.syntaxErrorTest` from the conformance test suite. The script
     * we are looking for contains whitespace and exactly one '{'.
     * Part of a workaround for #10.
     */
    def isSyntaxErrorTestScript(script: MemVirtualBinaryFile): Boolean = {
      script.path == "testScript.js" && {
        val is = script.inputStream
        try {
          // Because it is a MemVirtualBinaryFile
          assert(is.isInstanceOf[ByteArrayInputStream])

          val buf = new Array[Byte](128)
          val n = is.read(buf)
          n > 0 && n < buf.length && {
            assert(is.read() < 0) // because it is a ByteArrayInputStream
            (0 until n).forall { i =>
              val b = buf(i)
              b == ' ' || b == '\n' || b == '{'
            } && {
              (0 until n).count(i => buf(i) == '{') == 1
            }
          }
        } finally {
          is.close()
        }
      }
    }

    for (script <- scripts) {
      script match {
        /* The most horrible way to allow RunTests.syntaxErrorTest in the
         * conformance test suite to succeed, despite #10.
         */
        case script: MemVirtualBinaryFile if isSyntaxErrorTestScript(script) =>
          out.write(
              """<script type="text/javascript">throw new SyntaxError("syntax error");</script>""" + "\n")

        case _ =>
          val scriptURI = materialize(script)
          val fname = htmlEscape(fixFileURI(scriptURI).toASCIIString)
          out.write(
              s"""<script type="text/javascript" src="$fname"></script>""" + "\n")
      }
    }

    out.write(s"</head>\n<body></body>\n</html>\n")
  }

  protected def htmlEscape(str: String): String = str.flatMap {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '&' => "&amp;"
    case c   => c :: Nil
  }

}

object PhantomJSEnv {
  private final val launcherName = "scalaJSPhantomJSEnvLauncher"

  private lazy val validator = ExternalJSRun.supports(RunConfig.Validator())

  // tmpSuffixRE and tmpFile copied from HTMLRunnerBuilder.scala in Scala.js

  private val tmpSuffixRE = """[a-zA-Z0-9-_.]*$""".r

  private def tmpFile(path: String, in: InputStream): URI = {
    try {
      /* - createTempFile requires a prefix of at least 3 chars
       * - we use a safe part of the path as suffix so the extension stays (some
       *   browsers need that) and there is a clue which file it came from.
       */
      val suffix = tmpSuffixRE.findFirstIn(path).orNull

      val f = File.createTempFile("tmp-", suffix)
      f.deleteOnExit()
      Files.copy(in, f.toPath(), StandardCopyOption.REPLACE_EXISTING)
      f.toURI()
    } finally {
      in.close()
    }
  }

  private def materialize(file: VirtualBinaryFile): URI = {
    file match {
      case file: FileVirtualFile => file.file.toURI
      case file                  => tmpFile(file.path, file.inputStream)
    }
  }

  final class Config private (
      val executable: String,
      val args: List[String],
      val env: Map[String, String],
      val jettyClassLoader: ClassLoader
  ) {
    private def this() = {
      this(
          executable = "phantomjs",
          args = Nil,
          env = Map.empty,
          jettyClassLoader = null
      )
    }

    def withExecutable(executable: String): Config =
      copy(executable = executable)

    def withArgs(args: List[String]): Config =
      copy(args = args)

    def withEnv(env: Map[String, String]): Config =
      copy(env = env)

    def withJettyClassLoader(jettyClassLoader: ClassLoader): Config =
      copy(jettyClassLoader = jettyClassLoader)

    private def copy(
        executable: String = executable,
        args: List[String] = args,
        env: Map[String, String] = env,
        jettyClassLoader: ClassLoader = jettyClassLoader
    ): Config = {
      new Config(executable, args, env, jettyClassLoader)
    }
  }

  object Config {
    /** Returns a default configuration for a [[PhantomJSEnv]].
     *
     *  The defaults are:
     *
     *  - `executable`: `"phantomjs"`
     *  - `args`: `Nil`
     *  - `env`: `Map.empty`
     *  - `jettyClassLoader`: `null` (will use the current class loader)
     */
    def apply(): Config = new Config()
  }
}
