package io.evolutionary.twin

import java.util.concurrent.TimeUnit

import com.codahale.metrics.MetricRegistry
import com.codahale.metrics.json.MetricsModule
import com.fasterxml.jackson.databind.ObjectMapper
import com.typesafe.config.{ConfigFactory, Config}
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.middleware.Metrics
import org.http4s.dsl._
import com.codahale.metrics._

import scalaz.concurrent.{Task, TaskApp}
import scalaz.stream._
import org.http4s.dsl._
import org.http4s.server._
import scala.collection.JavaConverters._
import language.postfixOps

object Main extends App {
  class ServerSettings(config: Config) {
    val serverCfg = config.getConfig("io.evolutionary.twin.server")

    val port = serverCfg.getInt("port")
    val host = serverCfg.getString("host")
    val route = serverCfg.getString("route")
  }

  val settings = new ServerSettings(ConfigFactory.load())

  implicit val client = org.http4s.client.blaze.defaultClient

  val metrics = new MetricRegistry()
  val mapper = new ObjectMapper()
    .registerModule(new MetricsModule(TimeUnit.SECONDS, TimeUnit.SECONDS, true))

  val httpService = HttpService(Router.make)
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

  ((io stdInLines) map Command.parseCommand to (io stdOutLines)).onComplete(Process.eval(Task { server.awaitShutdown() })).run.run

}
