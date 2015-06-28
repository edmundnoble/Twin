package io.evolutionary.twin

import com.typesafe.config.{ConfigFactory, Config}
import org.http4s.server.blaze.BlazeBuilder

import scalaz.concurrent.{Task, TaskApp}
import org.http4s.dsl._
import org.http4s.server._

object Main extends App {
  class ServerSettings(config: Config) {
    config.checkValid(ConfigFactory.defaultReference())
    config.getConfig("io.evolutionary.twin.server")

    val port = config.getInt("port")
    val host = config.getString("host")
    val route = config.getString("route")
  }
  val httpService = HttpService(Router.make)

  val settings = new ServerSettings(ConfigFactory.load())

  BlazeBuilder
    .bindHttp(settings.port, settings.host)
    .mountService(httpService, settings.route)
    .run
    .awaitShutdown()

}
