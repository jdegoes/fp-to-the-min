package fpmin

import java.time.Month
import java.time.YearMonth
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

import fpmin.csv._
import fpmin.scheduler._

object StatsCollector1 {
  def aggregate(covid19: Covid19, month: Month): Csv = {
    val daysInMonth = YearMonth.of(2020, month.getValue()).lengthOfMonth()

    (1 to daysInMonth).map { dayInMonth =>
      val csv = covid19.unsafeLoad(month.getValue(), dayInMonth)

      val retained = csv.retain("country_region", "confirmed", "deaths", "recovered", "active", "province_state")

      val grouped = retained.groupBy("province_state", "country_region")

      grouped.sortByReverse(grouped("deaths").int)
    }.reduce(_ + _)
  }

  def printSummary(csv: Csv): Unit = {
    println("-- SUMMARY --")
    println(csv)
  }

  def aggregateAndSummarize(covid19: Covid19, month: Month): Unit = {
    val result = aggregate(covid19, month)

    printSummary(result.truncate(10))

    println("Press [Enter] to stop downloading...")
  }

  def main(args: Array[String]): Unit = {
    val github    = new Github(zio.blocking.Blocking.Service.live)
    val scheduler = new Scheduler.Live(Executors.newScheduledThreadPool(1))
    val covid19   = new Covid19.Live(github)

    val f = scheduler.schedule(0L, 1L, TimeUnit.MINUTES) {
      aggregateAndSummarize(covid19, Month.APRIL)
    }

    val _ = scala.io.StdIn.readLine()

    f.cancel(true)

    scheduler.shutdown()

    ()
  }
}
