package fpmin

import scala.io.Source

class Github() {

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
  private def downloadUrl(slug: String, file: String): String =
    s"https://github.com/${slug}/raw/master/${file}"
}
