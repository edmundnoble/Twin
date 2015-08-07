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

import scala.collection.JavaConverters._

object Command extends JavaTokenParsers with TwinLogging {

  case class ParseException(reason: String) extends RuntimeException(reason)

  implicit class TaskFromParseResult(val p: Task.type) {
    def fromParseResult(p: ParseResult[Task[Unit]]): Task[Unit] = p match {
      case Success(s, _) => s
      case Failure(msg, next) => Task.delay {
        logger.error("Command failed to parse!")
      }
      case Error(msg, next) => Task.fail(ParseException(msg))
    }
  }

  implicit class ProcessFromParseResult(val p: Process.type) {
    def fromParseResult(p: ParseResult[Process[Task, Nothing]]): Process[Task, Nothing] = p match {
      case Success(s, _) => s
      case Failure(msg, next) => Process.fromEffect[Task, Nothing](logger.error("Command failed to parse!"))
      case Error(msg, next) => Process.eval(Task.fail(ParseException(msg)))
    }
  }
  def url = "^(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]".r
  def siteRegex = "[a-zA-Z0-9]+".r

  type Cmd = (Client, Router) => Task[Unit]

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

  // empty command
  def ignore(client: Client, router: Router): Task[Unit] = Task.delay(())

  def exitCommand(client: Client, router: Router): Task[Unit] = Task.delay(System.exit(0))

  def fileExceptionHandler: PartialFunction[Throwable, Task[Unit]] = {
    case ex: IOException =>
      Task.delay(logger.error(ex)("Could not delete file!"))
  }

  def deleteAllCommand(client: Client, router: Router): Task[Unit] = {
    val siteDeletes: List[Task[Unit]] = router.allSites.map(site => Paths.get(Sites.siteToFileName(site))).map { file =>
      logger.warn(s"Deleting file... ${file.toString}")
      FileUtils.deleteFolder(file)
    }
    if (siteDeletes.isEmpty) Task.delay(logger.error("No sites to delete!"))
    else siteDeletes.reduce(_ >> _).handleWith(fileExceptionHandler)
  }

  def deleteCommand(site: String)(client: Client, router: Router): Task[Unit] = {
    val file = Paths.get(Sites.siteToFileName(site))
    val siteDelete = Task.delay(logger.warn(s"Deleting file... ${file.toString}")) >> FileUtils.deleteFolder(file)
    val errorsHandled = siteDelete.handleWith(fileExceptionHandler)
    errorsHandled
  }

  def fetchCommand(args: ~[String, String])(client: Client, router: Router): Task[Unit] = args match {
    case site ~ url =>
      implicit val C = client
      val uriParseResult = Uri.fromString(url)
      uriParseResult.fold(
        l = _ => Task.fail(new ParseException("Invalid URI")),
        r = uriParsed => Sites.forceFetchRoute(site, uriParsed, Map()).run)
  }

  def listCommand(client: Client, router: Router): Task[Unit] = {

    import Tree._

    val sites = router.settings.mirroredSites
    val siteFolders = sites.map(site => (site, Sites.siteToFileName(site)))
    val contained = siteFolders.map {
      case (site, fileName) =>
        Task.delay {
          val path = Paths.get(fileName)
          val files = Files.list(path).iterator().asScala.toStream.map(f => leaf(f.getFileName.toString))
          (site, files)
        }
    }.sequenceU
    val tree: Task[Tree[String]] = contained.map {
      list =>
        val subtrees = list.map {
          site => node(site._1, site._2)
        }
        node("Sites", subtrees.toStream)
    }
    tree.map(t => println(t.drawTree))
  }

  def createCommand(site: String)(client: Client, router: Router): Task[Unit] = {
    val sites = router.settings.mirroredSites
    if (sites contains site) {
      Task.delay(logger.error(s"Site $site already exists!"))
    } else {
      Task.delay(???)
    }
  }

  def usageMessage = """Usage: 
                       |("f" | "fetch") siteName url ->
                       |     fetches url, saving it under site siteName
                       |("c" | "create") siteName ->
                       |     creates a new site with name siteName
                       |("quit" | "exit" | "q") ->
                       |     quits Twin
                       |"delete" siteName ->
                       |     deletes the site named siteName
                       | "deleteAll" ->
                       |     deletes all sites
                       |"list" | "ls" | "l" ->
                       |     lists the sites you have, as well as the urls under them (the site directory structure)
                       |"help" | "usage" | "?" ->
                       |      shows this message
                     """.stripMargin

  def usageCommand(client: Client, router: Router): Task[Unit] = {
    Task.delay(println(usageMessage))
  }

  def commandExceptionHandler: PartialFunction[Throwable, Task[Unit]] = {
    case ex: Exception =>
      Task.delay {
        logger.error(ex)("an uncaught exception has occurred: ")
      }
    case t: Throwable =>
      Task.delay {
        logger.error(t)("a serious error has occurred; the program will exit"); sys.exit(1)
      }
  }

  def deleteAll: Parser[Cmd] = uncased("deleteall") ^^ (_ => deleteAllCommand)
  def delete: Parser[Cmd] = uncased("delete") ~> siteRegex ^^ deleteCommand
  def fetch: Parser[Cmd] = (uncased("fetch") ~> siteRegex) ~ url ^^ fetchCommand
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
