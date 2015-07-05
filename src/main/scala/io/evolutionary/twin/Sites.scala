package io.evolutionary.twin

import java.io.{FileOutputStream, File}
import java.nio.file.{FileAlreadyExistsException, Paths, Files}

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
import effectful._

object Sites {
  type Site = String

  val siteFolder = "sites"
  val validFilenameCharacters = ('0' to '9') ++ ('A' to 'Z')
  val fileBufferSize = 4096

  def getRoute(site: Site, url: Uri)(implicit client: Client): Process[Task, String] = {
    val fileName = routeToFileName(site, url)
    val cachedAlready = isCached(fileName)
    if (cachedAlready) {
      retrieveFile(fileName)
    } else {
      fetchAndSaveFile(site, fileName, url)
    }
  }

  def fetchAndSaveFile(site: Site, fileName: String, url: Uri)(implicit client: Client): Process[Task, String] = {
    fetchSite(url)
      .pipe(utf8Encode)
      .observe(saveFile(site, fileName))
      .pipe(utf8Decode)
  }

  implicit val fileCodec = Codec.UTF8

  def retrieveFile(fileName: String): Process[Task, String] = {
    Process.constant(100)
      .toSource
      .through(io fileChunkR(fileName, fileBufferSize))
      .pipe(utf8Decode)
  }

  def saveFile(siteName: Site, fileName: String): Sink[Task, ByteVector] = {
    import scala.util.control.Exception._
    val process =
      Task.delay {
        try {
          val filePath = Paths.get(fileName)
          val site = siteName.toUpperCase
          def ct(f: => Unit) = catching(classOf[FileAlreadyExistsException]).withTry(f)
          ct(Files.createDirectory(Paths.get(siteFolder)))
          ct(Files.createDirectory(Paths.get(siteFolder, site)))
          ct(Files.createFile(filePath))
          println("Making outputstream!")
          Files.newOutputStream(filePath)
        } catch {
          case ex: Throwable =>
            ex.printStackTrace
            sys.exit(1)
        }
      }

    val res = Process.eval(process).flatMap((fs) => io.chunkW(fs))
    res
  }

  def isCached(fileName: String): Boolean = {
    Files.exists(Paths.get(fileName))
  }

  def routeToFileName(site: Site, url: Uri): String = {
    def escapeFileName(name: String): String = name.toUpperCase.filter(validFilenameCharacters.contains)
    val sep = File.separator
    val escapedInnerFile = Vector(site, url.toString).map(escapeFileName).mkString(sep)
    val fileName = siteFolder + sep + escapedInnerFile
    println(s"File: $fileName")
    fileName
  }

  def fetchSite(url: Uri)(implicit client: Client): Process[Task, String] = {
    Process.eval(client(url).as[String])
  }
}
