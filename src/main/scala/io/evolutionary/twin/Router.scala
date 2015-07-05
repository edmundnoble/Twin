package io.evolutionary.twin

import com.typesafe.config.{ConfigFactory, Config}
import org.http4s.client.Client
import org.http4s.{Uri, Response, Request}
import org.http4s.dsl._
import scala.language.postfixOps
import scalaz.Scalaz._
import scalaz._
import effectful._

import scalaz.concurrent.Task
import scalaz.stream._

import scala.collection.JavaConverters._

class RouterSettings(config: Config) {

  import Scalaz._

  val routerCfg = config.getConfig("io.evolutionary.twin.router")
  val mirroredConfigs = routerCfg.getConfigList("mirrored").asScala
  val mirroredSites = mirroredConfigs map { _.getString("name") }
}

object Router {
  def make(implicit client: Client): Router = new Router(new RouterSettings(ConfigFactory.load()))
}

class Router(settings: RouterSettings)(implicit client: Client) extends PartialFunction[Request, Task[Response]] {

  import Scalaz._
  import Sites._

  override def isDefinedAt(req: Request): Boolean = req match {
    case GET -> Root / (site: Site) =>
      settings.mirroredSites.contains(site)
    case _ => false
  }

  override def apply(req: Request): Task[Response] = req match {
    case GET -> Root / (site: Site) =>
      val url = req.params.get("url")
      println(req.params)
      val task: Option[Process[Task, String]] = effectfully {
        getRoute(site, Uri.fromString(url !).toOption !)
      }
      task.fold(NotFound())(Ok(_))
  }
}
