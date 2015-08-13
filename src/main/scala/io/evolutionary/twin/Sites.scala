package io.evolutionary.twin

import java.io.{FileOutputStream, File}
import java.nio.file.{FileAlreadyExistsException, Paths, Files}

import org.http4s.Uri.{RegName, Authority}

import scalaz.stream.text._
import org.http4s.client.Client
import org.http4s.dsl._
import org.http4s._
import scodec.bits.ByteVector

import scala.io.Codec
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz._
import Scalaz._

object Sites {
  type Site = String

  val allSiteFolder = "sites"
  val validFilenameCharacters = ('0' to '9') ++ ('A' to 'Z')
  val fileBufferSize = 4096

  def fetchRoute(site: Site, url: Uri, req: Request)(implicit client: Client): Process[Task, String] = {
    val fileName = routeToFileName(site, url)
    val urlWithAuthority = url.copy(authority = Some(url.authority.getOrElse(Authority(host = RegName(url.path)))))
    val cachedAlready = Process.eval(isCached(fileName))
    cachedAlready.ifM(retrieveFile(fileName), fetchAndSaveToFile(site, fileName, urlWithAuthority, req))
  }

  def forceFetchRoute(site: Site, url: Uri, req: Request)(implicit client: Client): Process[Task, String] = {
    fetchAndSaveToFile(site, routeToFileName(site, url), url, req)
  }

  def fetchAndSaveToFile(site: Site, fileName: String, url: Uri, req: Request)(implicit client: Client): Process[Task, String] = {
    getSite(url, req)
      .pipe(utf8Encode)
      .observe(saveFile(site, fileName))
      .pipe(utf8Decode)
  }

  implicit val fileCodec = Codec.UTF8

  def retrieveFile(fileName: String): Process[Task, String] = {
    Process.constant(fileBufferSize)
      .toSource
      .through(io fileChunkR(fileName, fileBufferSize))
      .pipe(utf8Decode)
  }

  def saveFile(siteName: Site, fileName: String): Sink[Task, ByteVector] = {
    import scala.util.control.Exception._
    val task =
      Task.delay {
        try {
          val filePath = Paths.get(fileName)
          val site = siteName.toUpperCase
          def ct(f: => Unit) = catching(classOf[FileAlreadyExistsException]).withTry(f)
          ct(Files.createDirectory(Paths.get(allSiteFolder)))
          ct(Files.createDirectory(Paths.get(allSiteFolder, site)))
          ct(Files.createFile(filePath))
          println("Making outputstream!")
          Files.newOutputStream(filePath)
        } catch {
          case ex: Throwable =>
            ex.printStackTrace()
            sys.exit(1)
        }
      }

    val res = Process.eval(task).flatMap((fs) => io.chunkW(fs))
    res
  }

  def isCached(fileName: String): Task[Boolean] = Task.delay {
    Files.exists(Paths.get(fileName))
  }

  def translateRequest(url: Uri, req: Request): Request = {
    val sanitizedParams = req.params.filterKeys(!_.startsWith("__twin"))
    Request(req.method, url, req.httpVersion, req.headers, req.body, req.attributes)
  }

  def escapeFileName(name: String): String = name.toUpperCase.filter(validFilenameCharacters.contains)

  def routeToFileName(site: Site, url: Uri): String = {
    val sep = File.separator
    val fileName = siteToFileName(site) + sep + escapeFileName(url.toString)
    fileName
  }

  def siteToFileName(site: Site): String = allSiteFolder + File.separator + escapeFileName(site)

  private def getSite(url: Uri, origRequest: Request)(implicit client: Client): Process[Task, String] = {
    Process.eval(client(translateRequest(url, origRequest)).map { c => println(s"Request status: ${c.status}"); c }.as[String])
  }
}
