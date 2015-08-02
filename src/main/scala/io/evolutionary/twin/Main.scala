package io.evolutionary.twin

import java.util.concurrent.TimeUnit

import com.codahale.metrics.MetricRegistry
import com.codahale.metrics.json.MetricsModule
import com.fasterxml.jackson.databind.ObjectMapper
import com.typesafe.config.{ConfigFactory, Config}
import org.apache.log4j.BasicConfigurator
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.middleware.Metrics
import org.http4s.dsl._
import com.codahale.metrics._

import scalaz._
import Scalaz._
import scalaz.concurrent.{Task, TaskApp}
import scalaz.stream._
import org.http4s.dsl._
import org.http4s.server._
import scala.collection.JavaConverters._
import language.postfixOps

import net.ceedubs.ficus.Ficus._

class ServerSettings(config: Config) {
  val serverCfg = config.getConfig("io.evolutionary.twin.server")

  val port = serverCfg.as[Int]("port")
  val host = serverCfg.as[String]("host")
  val route = serverCfg.as[String]("route")
}

object Main extends App {

  BasicConfigurator.configure()

  val settings = new ServerSettings(ConfigFactory.load())

  implicit val client = org.http4s.client.blaze.defaultClient

  val metrics = new MetricRegistry()
  val mapper = new ObjectMapper()
    .registerModule(new MetricsModule(TimeUnit.SECONDS, TimeUnit.SECONDS, true))

  val routerSettings = new RouterSettings(ConfigFactory.load())
  val router = Router.make(routerSettings)

  val httpService = HttpService(router)
  val metricsService = HttpService {
    case GET -> Root / "metrics" =>
      val writer = mapper.writerWithDefaultPrettyPrinter()
      Ok(writer.writeValueAsString(metrics))
  }

  val svc = Metrics.meter(metrics, "Twin")(httpService orElse metricsService)

  val server = BlazeBuilder
    .bindHttp(settings.port, settings.host)
    .mountService(svc, settings.route)
    .withNio2(true)
    .run

  println("Server started!")

  val commandParsing = (io stdInLines) flatMap { cmd => Command.parseCommand(cmd, router) }

  commandParsing.run.run

}
