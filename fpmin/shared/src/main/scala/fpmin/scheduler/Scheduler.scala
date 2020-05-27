package fpmin.scheduler

import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.ScheduledFuture

trait Scheduler {

  /**
   * Schedules the action to execute regularly, on the specified schedule,
   * with the specified starting delay, and increment between time units.
   */
  def schedule[A](initial: Long, increment: Long, unit: TimeUnit)(run0: => A): ScheduledFuture[_]

  /**
   * Shuts down the scheduler.
   */
  def shutdown(): Unit
}
object Scheduler {
  class Live(scheduledExecutor: ScheduledExecutorService) extends Scheduler {
    def schedule[A](initial: Long, increment: Long, unit: TimeUnit)(run0: => A): ScheduledFuture[_] =
      scheduledExecutor.scheduleAtFixedRate(new Runnable {
        def run() = { val _ = run0 }
      }, initial, increment, unit)

    def shutdown(): Unit = { val _ = scheduledExecutor.shutdownNow() }
  }
}
