package io.evolutionary.twin

import java.io.File
import java.nio.file.Files

import org.http4s.client.Client
import org.http4s.dsl._
import org.http4s._

import scala.io.Codec
import scalaz.concurrent.Task
import scalaz.stream._

case class Site(name: String, urls: Seq[Uri])

object Sites {
  val validFilenameCharacters = ('0' to '9') ++ ('A' to 'Z')

  def getRoute(site: Site, url: Uri)(implicit client: Client): Process[Task, String] = {
    val cachedAlready = isCached(site, url)
    if (cachedAlready) {
      retrieveFile(site, url)
    } else {
      fetchSite(url)
    }
  }

  implicit val fileCodec = Codec.UTF8

  def retrieveFile(site: Site, url: Uri): Process[Task, String] = {
    val fileName = routeToFileName(site, url)
    io linesR fileName
  }

  def isCached(site: Site, url: Uri): Boolean = {
    new File(routeToFileName(site, url)).exists()
  }

  def routeToFileName(site: Site, url: Uri): String = {
    def escapeFileName(name: String): String = name.filter(validFilenameCharacters.contains)
    Vector(site.name.toUpperCase, url.toString().toUpperCase).map(escapeFileName).mkString(File.separator)
  }

  def fetchSite(url: Uri)(implicit client: Client): Process[Task, String] = {
    Process.eval(client(url).as[String])
  }
}
