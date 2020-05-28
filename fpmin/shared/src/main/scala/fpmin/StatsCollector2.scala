package fpmin

import fpmin.csv._

import java.time._

import zio._
import zio.duration._
import zio.console._

object StatsCollector2 extends App {
  def aggregate(month: Month) = {
    val daysInMonth = YearMonth.of(2020, month.getValue()).lengthOfMonth()

    ZIO
      .foreachPar(1 to daysInMonth) { dayInMonth =>
        val load = Covid19.load(month.getValue(), dayInMonth)

        val schedule = Schedule.exponential(1.millis) && Schedule.recurs(100)

        load
          .retry(schedule)
          .map { csv =>
            val retained = csv.retain("country_region", "confirmed", "deaths", "recovered", "active", "province_state")

            val grouped = retained.groupBy("province_state", "country_region")

            grouped.sortByReverse(grouped("deaths").int)
          }
          .timeoutFail(new Error)(30.seconds) orElse ZIO.succeed(Csv.empty)
      }
      .map(_.reduce(_ + _))
  }

  def printSummary(csv: Csv) =
    for {
      _ <- putStrLn("-- SUMMARY --")
      _ <- putStrLn(csv.toString())
    } yield ()

  def aggregateAndSummarize(month: Month) =
    for {
      result <- aggregate(month)
      _      <- printSummary(result.truncate(10))
      _      <- putStrLn("Press [Enter] to stop downloading...")
    } yield ()

  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic = {
    val agg = aggregateAndSummarize(Month.APRIL)

    (for {
      - <- agg.repeat(Schedule.spaced(1.minute)).fork
      _ <- getStrLn
    } yield ()).provideCustomLayer(Github.live >>> Covid19.live)
  }
}
