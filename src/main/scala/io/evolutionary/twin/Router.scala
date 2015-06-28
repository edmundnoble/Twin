package io.evolutionary.twin

import com.typesafe.config.{ConfigFactory, Config}
import org.http4s.{Response, Request}
import org.http4s.dsl._

import scalaz.concurrent.Task

import scala.collection.JavaConverters._

class RouterSettings(config: Config)  {
  val routerCfg = config.getConfig("io.evolutionary.twin.router")
  val mirroredConfigs = routerCfg.getList("mirrored").asScala
  val
}

object Router {
  def make: Router = new Router(new RouterSettings(ConfigFactory.load()))
}

class Router(settings: RouterSettings) extends PartialFunction[Request, Task[Response]] {
  override def isDefinedAt(x: Request): Boolean = x match {
    case GET -> Root / (site: String) if settings.mirroredSites.contains(site) =>
      Sites.getRoute(site, x.body)
  }
  override def apply(v1: Request): Task[Response] = ???
}
