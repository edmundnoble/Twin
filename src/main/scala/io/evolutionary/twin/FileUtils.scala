package io.evolutionary.twin

import java.nio.file.{Files, Path}

import scalaz.concurrent.Task
import scala.collection.JavaConverters._

object FileUtils {
  def deleteFolder(path: Path): Task[Unit] = Task.suspend {
    val (dirs, files) = Files.list(path).iterator().asScala.toVector.partition(Files.isDirectory(_))
    def combineTasks(t1: Task[Unit], t2: Task[Unit]): Task[Unit] = t1.flatMap(_ => t2)
    def combineAll(t: Seq[Task[Unit]]): Task[Unit] = t.foldLeft(Task.delay(()))(combineTasks)
    val dirDelete = combineAll(dirs.map(deleteFolder))
    val fileDelete = combineAll(files.map(file => Task.delay {
      Files.delete(file)
    }))
    combineTasks(dirDelete, fileDelete).map(_ => Files.delete(path))
  }
}
