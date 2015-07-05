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

import scala.collection.JavaConverters._

class RouterSettings(config: Config) {

  import Scalaz._

  val routerCfg = config.getConfig("io.evolutionary.twin.router")
  val mirroredConfigs = routerCfg.getConfigList("mirrored").asScala
  val mirroredSites = mirroredConfigs map { cfg =>
    val urls = cfg.getStringList("urls").asScala.map(url => Uri.fromString(url).fold((err) => throw new RuntimeException(s"Invalid URL: $url. Error: $err"), identity))
    Site(cfg.getString("name"), urls)
  }
}

object Router {
  def make(implicit client: Client): Router = new Router(new RouterSettings(ConfigFactory.load()))
}

class Router(settings: RouterSettings)(implicit client: Client) extends PartialFunction[Request, Task[Response]] {

  import Scalaz._

  override def isDefinedAt(req: Request): Boolean = req match {
    case GET -> Root / (site: String) =>
      settings.mirroredSites.exists(_.name == site)
    case _ => false
  }

  override def apply(req: Request): Task[Response] = req match {
    case GET -> Root / (site: String) =>
      val targetSite = settings.mirroredSites.find(_.name == site)
      val url = req.params.get("url")
      println(req.params)
      val task: Option[Process[Task, String]] = effectfully {
        Sites.getRoute(targetSite !, Uri.fromString(url !).toOption !)
      }
      task.fold(NotFound())(Ok(_))
  }
}
