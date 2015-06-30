package io.evolutionary.twin

import com.typesafe.config.{ConfigFactory, Config}
import org.http4s.{Uri, Response, Request}
import org.http4s.dsl._

import scalaz.concurrent.Task

import scala.collection.JavaConverters._

class RouterSettings(config: Config) {
  val routerCfg = config.getConfig("io.evolutionary.twin.router")
  val mirroredConfigs = routerCfg.getConfigList("mirrored").asScala
  val mirroredSites = for {
    cfg <- mirroredConfigs
    siteName = cfg.getString("site")
    urls = cfg.getStringList("urls").asScala.map(url => Uri.fromString(url).fold((err) => throw new RuntimeException(s"Invalid URL: $url. Error: $err"), identity))
  } yield Site(siteName, urls)
}

object Router {
  def make: Router = new Router(new RouterSettings(ConfigFactory.load()))
}

class Router(settings: RouterSettings) extends PartialFunction[Request, Task[Response]] {
  override def isDefinedAt(x: Request): Boolean = x match {
    case GET -> Root / (site: String) =>
      val targetSite = settings.mirroredSites.find(_.name == site)
      targetSite.isDefined
  }
  override def apply(v1: Request): Task[Response] = v1 match {
    case GET -> Root / (site: String) =>
      val targetSite = settings.mirroredSites.find(_.name == site)
      val url = v1.params.get("url")
      val task = for {
        site <- targetSite
        urlRaw <- url
        urlParsed <- Uri.fromString(urlRaw).toOption
      } yield Sites.getRoute(site, urlParsed)
      task.fold(NotFound())(Ok(_))
  }
}
