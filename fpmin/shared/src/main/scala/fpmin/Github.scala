package fpmin

import scala.io.Source

class Github() {

  /**
   * Downloads a file from Github.
   */
  def unsafeDownload(slug: String, file: String): String =
    Source.fromURL(Github.downloadUrl(slug, file)).getLines().mkString("\n")
}
object Github {
  private def downloadUrl(slug: String, file: String): String =
    s"https://github.com/${slug}/raw/master/${file}"
}
