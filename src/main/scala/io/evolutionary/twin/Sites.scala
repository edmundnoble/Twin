package io.evolutionary.twin

import org.http4s.client.Client
import org.http4s.dsl._
import org.http4s._

import scalaz.concurrent.Task

case class Site(name: String, urls: List[Uri])

object Sites {
  def getRoute(url: Uri): Task[String] = {
    
  }

  def fetchSite(site: Site)(implicit client: Client): Seq[Task[String]] = {
    site.urls.map(url => client(url).as[String])
  }
}
