package io.evolutionary.twin

import java.nio.file.{Paths, Files}

import org.http4s.{Request, Uri}
import org.http4s.client.Client

import scalaz.concurrent.Task
import scalaz._
import Scalaz._

import scala.collection.JavaConverters._

object Command extends TwinLogging {

  case class CommandException(reason: String) extends RuntimeException(reason)

  type Cmd = (Client, Router) => Task[Unit]

  // empty command
  def ignore(client: Client, router: Router): Task[Unit] = Task.delay(())

  def exitCommand(client: Client, router: Router): Task[Unit] = Task.delay(System.exit(0))

  def fileExceptionHandler: PartialFunction[Throwable, Task[Unit]] = {
    case ex: Exception =>
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

  def deleteCommand(site: String)(client: Client, router: Router): Task[Unit] = Task.suspend {
    val path = Paths.get(Sites.siteToFileName(site))
    if (Files.exists(path)) {
      logger.warn(s"Deleting site $site")
      FileUtils.deleteFolder(path).handleWith(fileExceptionHandler)
    } else {
      Task.delay {
        logger.error(s"Site $site does not exist!")
      }
    }
  }

  def createCommand(site: String)(client: Client, router: Router): Task[Unit] = Task.delay {
    val path = Paths.get(Sites.siteToFileName(site))
    if (Files.exists(path)) {
      logger.error(s"Site $site already exists!")
    } else {
      Files.createDirectory(path)
      println(s"Site $site created!")
    }
  }

  def fetchCommand(site: String, url: String)(client: Client, router: Router): Task[Unit] = {
    implicit val C = client
    val uriParseResult = Uri.fromString(url)
    uriParseResult.fold(
      l = _ => Task.fail(new CommandException("Invalid URI")),
      r = uriParsed => Sites.forceFetchRoute(site, uriParsed, Request()).run)
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

}
