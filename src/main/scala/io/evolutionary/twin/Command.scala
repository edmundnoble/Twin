package io.evolutionary.twin

import java.io.{IOException, File}
import java.nio.file.{Paths, Files}

import org.http4s.Uri
import org.http4s.client.Client

import scala.util.parsing.combinator.JavaTokenParsers
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz._
import Scalaz._
import scalaz.stream.nio.file

object Command extends JavaTokenParsers with TwinLogging {

  case class ParseException(reason: String) extends RuntimeException(reason)

  implicit class ProcessFromParseResult(val p: Process.type) {
    def fromParseResult(p: ParseResult[Process[Task, Nothing]]): Process[Task, Nothing] = p match {
      case Success(s, _) => s
      case Failure(msg, next) => Process.fromEffect[Task, Nothing](logger.error("Command failed to parse!"))
      case Error(msg, next) => Process.eval(Task.fail(ParseException(msg)))
    }
  }

  def url = """_^(?:(?:https?|ftp)://)(?:\S+(?::\S*)?@)?(?:(?!10(?:\.\d{1,3}){3})(?!127(?:\.\d{1,3}){3})(?!169\.254(?:\.\d{1,3}){2})(?!192\.168(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z\x{00a1}-\x{ffff}0-9]+-?)*[a-z\x{00a1}-\x{ffff}0-9]+)(?:\.(?:[a-z\x{00a1}-\x{ffff}0-9]+-?)*[a-z\x{00a1}-\x{ffff}0-9]+)*(?:\.(?:[a-z\x{00a1}-\x{ffff}]{2,})))(?::\d{2,5})?(?:/[^\s]*)?$_iuS""".r
  def siteRegex = "[a-zA-Z0-9]+".r

  type Cmd = (Client, Router) => Process[Task, Nothing]

  // case insensitive literal parser
  def uncased(s: String): Parser[String] = ("(?i)" + s).r

  // empty command
  def ignore(client: Client, router: Router): Process[Task, Nothing] = Process.empty

  def exitCommand(client: Client, router: Router): Process[Task, Nothing] = Process.eval_(Task.delay {
    System.exit(0)
  })

  def fileExceptionHandler: PartialFunction[Throwable, Process[Task, Nothing]] = {
    case ex: IOException =>
      Process.fromEffect[Task, Nothing](logger.error(ex)("Could not delete file!"))
  }

  def deleteAllCommand(client: Client, router: Router): Process[Task, Nothing] = {
    val siteDeletes: List[Process[Task, Unit]] = router.allSites.map(site => Paths.get(Sites.siteToFileName(site))).map { file =>
      Process.fromEffect[Task, Unit] {
        logger.warn(s"Deleting file... ${file.toString}")
        FileUtils.deleteFolder(file)
      }
    }
    if (siteDeletes.isEmpty) Process.fromEffect[Task, Nothing](logger.error("No sites to delete!"))
    else siteDeletes.reduce(_ ++ _).handle(fileExceptionHandler)
  }

  def deleteCommand(site: String)(client: Client, router: Router): Process[Task, Nothing] = {
    val file = Paths.get(Sites.siteToFileName(site))
    val siteDelete = Process.fromEffect[Task, Nothing](logger.warn("Deleting file... ${file.toString}")) ++ Process.fromEffect[Task, Nothing](FileUtils.deleteFolder(file))
    val errorsHandled = siteDelete.handle(fileExceptionHandler)
    errorsHandled
  }

  def fetchCommand(args: ~[String, String])(client: Client, router: Router): Process[Task, Nothing] = args match {
    case site ~ url =>
      implicit val C = client
      val uriParseResult = Uri.fromString(url)
      uriParseResult.fold(
        l = _ => Process.fail(new ParseException("Invalid URI")),
        r = uriParsed => Sites.forceFetchRoute(site, uriParsed).drain)
  }

  def deleteAll: Parser[Cmd] = uncased("deleteall") ^^ (_ => deleteAllCommand)
  def delete: Parser[Cmd] = uncased("delete") ~> whiteSpace ~> siteRegex ^^ deleteCommand
  def fetch: Parser[Cmd] = (uncased("fetch") ~> whiteSpace ~> siteRegex) ~ url ^^ fetchCommand
  def exit: Parser[Cmd] = (uncased("quit") | uncased("exit") | uncased("q")) ^^ (_ => exitCommand)
  def blank: Parser[Cmd] = whiteSpace.* ^^ (_ => ignore)
  def command: Parser[Cmd] = deleteAll | fetch | exit | blank

  def parseCommand(cmd: String, router: Router)(implicit client: Client): Process[Task, Nothing] = {
    val parseResult = parseAll(command, cmd)
    Process.fromEffect[Task, Nothing] {
      logger.debug(s"command entered: $cmd\nparse result: $parseResult")
      println()
    } ++ Process.fromParseResult(parseAll(command, cmd).map(_(client, router)))
  }

}
