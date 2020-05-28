package fpmin

import zio.App
import zio.console._

object StatsCollector2 extends App {
  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic =
    for {
      _    <- putStrLn("Hello! What is your name?")
      name <- getStrLn
      _    <- putStrLn(s"Hello, $name, welcome to ZIO!")
    } yield ()
}
