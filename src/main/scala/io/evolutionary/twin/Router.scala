package io.evolutionary.twin

import com.typesafe.config.{ConfigFactory, Config}
import org.http4s.client.Client
import org.http4s.{Uri, Response, Request}
import org.http4s.dsl._
import scala.language.postfixOps
import scala.util.Try
import scalaz.Scalaz._
import scalaz._
import effectful._

import scalaz.concurrent.Task
import scalaz.stream._

import scala.collection.JavaConverters._

import net.ceedubs.ficus.Ficus._

class RouterSettings(config: Config) {
  val routerCfg = config.getConfig("io.evolutionary.twin.router")
  val mirroredConfigs = routerCfg.as[List[Config]]("mirrored")
  val mirroredSites = mirroredConfigs map { _.getString("name") }
}

object Router {
  def make(settings: RouterSettings)(implicit client: Client): Router = new Router(settings)
}

class Router(val settings: RouterSettings)(implicit client: Client) extends PartialFunction[Request, Task[Response]] {

  import Scalaz._
  import Sites._

  def allSites = settings.mirroredSites

  override def isDefinedAt(req: Request): Boolean = req match {
    case _ -> Root / (site: Site) =>
      true//settings.mirroredSites.contains(site)
    case _ => false
  }

  override def apply(req: Request): Task[Response] = req match {
    case _ -> Root / (site: Site) =>
      val url = req.params.get("url")
      val refresh = req.params.get("__twin_refresh") == Some("true")
      if (refresh) println("Refreshing!")
      val task: Option[Process[Task, String]] = if (refresh) effectfully {
        Sites.forceFetchRoute(
          site, Uri.fromString(url !).toOption !, req)
      } else effectfully {
        Sites.fetchRoute(
          site, Uri.fromString(url !).toOption !, req)
      }
      task.fold(NotFound())(Ok(_))
  }
}