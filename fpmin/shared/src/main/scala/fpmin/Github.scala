package fpmin

import scala.io.Source

import java.io.IOException
import zio._
import zio.blocking._

class Github(blocking: Blocking.Service) {
  import blocking.effectBlocking

  def download(slug: String, file: String): ZIO[Any, IOException, String] =
    open(slug, file).use { source =>
      effectBlocking(source.getLines().mkString("\n")).refineToOrDie[IOException]
    }

  def open(slug: String, file: String): Managed[IOException, Source] = {
    val acquire = effectBlocking(unsafeOpen(slug, file)).refineToOrDie[IOException]
    val release = (source: Source) => effectBlocking(source.close()).orDie

    Managed.make(acquire)(release)
  }

  /**
   * Downloads a file from Github.
   */
  def unsafeDownload(slug: String, file: String): String =
    unsafeOpen(slug, file).getLines().mkString("\n")

  /**
   * Opens a file from Github, so that it can be streamed or downloaded.
   */
  def unsafeOpen(slug: String, file: String): Source =
    Source.fromURL(Github.downloadUrl(slug, file))
}
object Github {
  val live: ZLayer[Blocking, Nothing, Has[Github]] =
    ZLayer.fromFunction[Blocking, Github](env => new Github(env.get))

  private def downloadUrl(slug: String, file: String): String =
    s"https://github.com/${slug}/raw/master/${file}"
}
