package mua

import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.util.{Try,Success,Failure}
import java.io.IOException

/** Returned value for a process */
case class Output(status: Int, stdout: String, stderr: String) {
  /** The process exited successfully */
  def isSuccess: Boolean = status == 0
}

/** Process Logger to get access to stdout and stderr */
sealed class StdLogger extends ProcessLogger {
  private var _stdout = new ListBuffer[String]()
  private var _stderr = new ListBuffer[String]()

  def buffer[T](f: => T): T = f
  def out(s: => String): Unit = { this._stdout += s }
  def err(s: => String): Unit = { this._stderr += s }

  def stdout: String = _stdout.mkString("\n")
  def stderr: String = _stderr.mkString("\n")
}

object Command {

  /** Execute the command, wait it to finish and collects its output.
   *
   * The output is either a string with the standard output if
   * everything runs as expected or a process error if an errors
   * occurs.  A process errors could be either an error returned by
   * the shell (e.g., the programs does not exist) or the Program that
   * does not exit normally.
   */
  def output(args: String*): Try[Output] = Try {
    val stdlogger = new StdLogger()
    val status = Process(args).!(stdlogger)
    Output(status, stdlogger.stdout, stdlogger.stderr)
  }
}
