package io.evolutionary.twin

import java.nio.file.{Files, Path}
import java.util.function.Predicate

import scalaz.concurrent.Task
import scala.collection.JavaConverters._

object FileUtils {
  implicit def javaStreamToScala[T](s: java.util.stream.Stream[T]): Iterator[T] = {
    s.iterator().asScala
  }

  def deleteFolder(path: Path): Task[Unit] = Task.delay {
    val (dirs, files) = Files.list(path).toVector.partition(Files.isDirectory(_))
    def combineTasks(t1: Task[Unit], t2: Task[Unit]): Task[Unit] = t1.onFinish(_ => t2)
    def combineAll(t: Seq[Task[Unit]]): Task[Unit] = t.foldLeft(Task.delay(()))(combineTasks)
    val dirDelete = combineAll(dirs.map(dir => Task.suspend(deleteFolder(dir))))
    val fileDelete = combineAll(files.map(file => Task.delay(Files.delete(file))))
    combineTasks(dirDelete, fileDelete)
  }
}
