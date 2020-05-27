package fpmin.csv

/**
 * Represents a column in a CSV file.
 */
sealed trait Column[+A] { self =>
  import Column._

  /**
   * Appends two columns together.
   */
  def ++[A1 >: A](that: Column[A1]): Column[A1] =
    (self, that) match {
      case (Data(l), Data(r)) => Data(l ++ r)
      case (x, NA)            => x
      case (NA, y)            => y
      case _                  => NA
    }

  /**
   * Maps over the value with a partial function, translating exceptions into
   * Column.NA.
   */
  def as[B](f: A => B): Column[B] = self match {
    case NA          => NA
    case Data(value) => collectAll(value.map(a => scala.util.Try(f(a)))).map(Data(_)).getOrElse(NA)
  }

  /**
   * Maps over the value with a total function.
   */
  def map[B](f: A => B): Column[B] = self match {
    case NA          => NA
    case Data(value) => Data(value.map(f))
  }

  /**
   * Returns the size of the column.
   */
  def size: Int

  /**
   * Zips two columns together.
   */
  def zipWith[B, C](that: Column[B])(f: (A, B) => C): Column[C] =
    (self, that) match {
      case (Data(l), Data(r)) => Data(l.zip(r).map(f.tupled))
      case _                  => NA
    }
}
object Column {
  implicit class ColumnStringSyntax(self: Column[String]) {
    import java.time.Instant

    /**
     * Converts the string column to integers.
     */
    def int: Column[Int] = self.as(_.toInt)

    /**
     * Converts the string column to instants.
     */
    def dateTime: Column[Instant] = self.as(s => Instant.parse(s))
  }

  /**
   * A column in error state or not existing.
   */
  case object NA extends Column[Nothing] {
    def size: Int = 0
  }

  /**
   * A column with data.
   */
  final case class Data[+A](value: Vector[A]) extends Column[A] {
    def size: Int = value.length
  }
}
