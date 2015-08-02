package io.evolutionary.twin

import java.io.File
import java.nio.file.{Paths, Files}

import org.http4s.Uri
import org.http4s.client.Client

import scala.util.parsing.combinator.JavaTokenParsers
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz._
import Scalaz._

object Command extends JavaTokenParsers {

  case class ParseException(reason: String) extends RuntimeException(reason)

  implicit class ProcessFromParseResult(val p: Process.type) {
    def fromParseResult[T](p: ParseResult[Process[Task, T]]): Process[Task, T] = p match {
      case Success(s, _) => s
      case Failure(msg, next) => Process.eval_(Task.delay {
        println("Command failed to parse!")
      })
      case Error(msg, next) => Process.eval(Task.fail(ParseException(msg)))
    }
  }

  def url = """_^(?:(?:https?|ftp)://)(?:\S+(?::\S*)?@)?(?:(?!10(?:\.\d{1,3}){3})(?!127(?:\.\d{1,3}){3})(?!169\.254(?:\.\d{1,3}){2})(?!192\.168(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z\x{00a1}-\x{ffff}0-9]+-?)*[a-z\x{00a1}-\x{ffff}0-9]+)(?:\.(?:[a-z\x{00a1}-\x{ffff}0-9]+-?)*[a-z\x{00a1}-\x{ffff}0-9]+)*(?:\.(?:[a-z\x{00a1}-\x{ffff}]{2,})))(?::\d{2,5})?(?:/[^\s]*)?$_iuS""".r
  def siteRegex = "[a-zA-Z0-9]+".r

  type Cmd = (Client, Router) => Process[Task, String]

  def caseInsensitive(s: String): Parser[String] = ("(?i)" + s).r
  def ignore(client: Client, router: Router): Process[Task, String] = Process.empty

  def deleteAllCommand(client: Client, router: Router): Process[Task, String] = {
    val siteDeletes: List[Process[Task, String]] = router.allSites.map(site => Paths.get(Sites.siteToFileName(site))).map { file =>
      Process.eval(Task.delay(s"Deleting file... ${file.toString}")) ++ Process.eval(Task.delay(Files.delete(file))).map(_ => "")
    }
    if (siteDeletes.isEmpty) Process.eval(Task.delay("No sites to delete!"))
    else siteDeletes.reduce(_ ++ _)
  }

  def fetchCommand(args: ~[String, String])(client: Client, router: Router): Process[Task, String] = args match {
    case site ~ url =>
      implicit val C = client
      val uriParseResult = Uri.fromString(url)
      uriParseResult.fold(
        l = _ => Process.fail(new ParseException("Invalid URI")),
        r = uriParsed => Sites.forceFetchRoute(site, uriParsed))
  }

  def deleteAll: Parser[Cmd] = caseInsensitive("deleteall") ^^ (_ => deleteAllCommand _)
  def fetch: Parser[Cmd] = ("fetch" ~> whiteSpace ~> siteRegex) ~ url ^^ fetchCommand
  def blank: Parser[Cmd] = whiteSpace.? ^^ (_ => ignore)
  def command: Parser[Cmd] = deleteAll | fetch | blank

  def parseCommand(cmd: String, router: Router)(implicit client: Client): Process[Task, String] = {
    Process.fromParseResult(parseAll(command, cmd).map(_(client, router)))
  }

}
