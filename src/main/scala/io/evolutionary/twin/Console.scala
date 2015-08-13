package io.evolutionary.twin

import java.io.{IOException, File}
import java.nio.file.{Paths, Files}

import org.http4s.{Request, Uri}
import org.http4s.client.Client

import scala.util.parsing.combinator.JavaTokenParsers
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz._
import Scalaz._
import scalaz.stream.nio.file

import scala.collection.JavaConverters._

object Console extends JavaTokenParsers with TwinLogging {

  import Command._

  implicit class TaskFromParseResult(val p: Task.type) {
    def fromParseResult(p: ParseResult[Task[Unit]]): Task[Unit] = p match {
      case Success(s, _) => s
      case Failure(msg, next) => Task.delay {
        logger.error("Command failed to parse!")
      }
      case Error(msg, next) => Task.fail(CommandException(msg))
    }
  }

  implicit class ProcessFromParseResult(val p: Process.type) {
    def fromParseResult(p: ParseResult[Process[Task, Nothing]]): Process[Task, Nothing] = p match {
      case Success(s, _) => s
      case Failure(msg, next) => Process.fromEffect[Task, Nothing](logger.error("Command failed to parse!"))
      case Error(msg, next) => Process.eval(Task.fail(CommandException(msg)))
    }
  }

  def url = "^(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]".r
  def siteRegex = "[a-zA-Z0-9]+".r

  // case insensitive literal parser
  def uncased(s: String*): Parser[String] = s match {
    case x +: xs if xs.isEmpty => ("(?i)" + x).r
    case x +: xs => s"(?i)$x".r | uncased(s.tail: _*)
  }

  // sbt input doesn't backspace things, it just adds backspace characters
  def handleBackspaces(cmd: String): String = {
    val cmdList = cmd.toList
    def handleBackspacesRec(f: List[Char]): List[Char] = f match {
      case Nil => Nil
      case x :: '\u0008' :: xs => handleBackspacesRec(xs)
      case '\u0008' :: xs => handleBackspacesRec(xs)
      case x :: xs => x :: handleBackspacesRec(xs)
    }
    handleBackspacesRec(cmdList).mkString
  }

  def commandExceptionHandler: PartialFunction[Throwable, Task[Unit]] = {
    case ex: Exception =>
      Task.delay {
        logger.error(ex)("an uncaught exception has occurred: ")
      }
    case t: Throwable =>
      Task.delay {
        logger.error(t)("a serious error has occurred; the program will exit");
        sys.exit(1)
      }
  }

  def deleteAll: Parser[Cmd] = uncased("deleteall") ^^ (_ => deleteAllCommand)
  def delete: Parser[Cmd] = uncased("delete") ~> siteRegex ^^ deleteCommand
  def fetch: Parser[Cmd] = (uncased("fetch") ~> siteRegex) ~ url ^^ { case site ~ url => fetchCommand(site, url) }
  def create: Parser[Cmd] = uncased("create", "c") ~> siteRegex ^^ createCommand
  def exit: Parser[Cmd] = uncased("quit", "exit", "q") ^^ (_ => exitCommand)
  def list: Parser[Cmd] = uncased("list", "ls", "l") ^^ (_ => listCommand)
  def usage: Parser[Cmd] = uncased("help", "usage", "\\?") ^^ (_ => usageCommand)

  def wsp: Parser[Cmd] = "\\s".r.* ^^ (_ => ignore)
  def command: Parser[Cmd] = deleteAll | delete | fetch | exit | list | usage | create | wsp

  def parseCommand(cmd: String, router: Router)(implicit client: Client): Task[Unit] = {
    val correctCmd = handleBackspaces(cmd)
    val parseResult = parseAll(command, correctCmd)
    Task.delay {
      logger.debug(s"command entered: $correctCmd")
      println()
    } >>
      Task.fromParseResult(parseResult.map(_(client, router))).handleWith(commandExceptionHandler)
  }

}
