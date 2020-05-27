package fpmin.csv

/**
 * A simple representation of CSV files.
 */
final case class Csv(rows: Vector[Vector[String]], header: Map[String, Int]) { self =>

  /**
   * Adds two CSV files together, inserting blanks for columns present in one,
   * but missing in the other. Attempts to preserve ordering.
   */
  def +(that: Csv): Csv = {
    val selfHeaders = self.header.toVector.sortBy(_._2).map(_._1)

    val jointHeaders = that.header.keySet.foldLeft(selfHeaders) {
      case (vector, key) => if (vector.contains(key)) vector else vector :+ key
    }

    val newToSelf = jointHeaders.filter(header => !self.header.contains(header))
    val newToThat = jointHeaders.filter(header => !that.header.contains(header))

    val enlargedSelf = self.add(newToSelf)
    val enlargedThat = that.add(newToThat)

    val newHeader = jointHeaders.zipWithIndex.toMap

    Csv(
      rows = jointHeaders.map { header =>
        val left  = enlargedSelf(header)
        val right = enlargedThat(header)

        (left ++ right) match {
          case Column.NA           => Vector()
          case Column.Data(values) => values
        }
      }.transpose,
      newHeader
    )
  }

  /**
   * Retrieves the index of the specified column name.
   */
  def index(name: String): Option[Int] = header.get(name.toLowerCase())

  /**
   * Retrieves the column with the specified name.
   */
  def apply(name: String): Column[String] =
    index(name).map(index => Column.Data(rows.map(_.apply(index)))).getOrElse(Column.NA)

  /**
   * Adds a number of columns to the CSV file, with empty string values.
   */
  def add(columns0: Iterable[String], blank: String = ""): Csv = {
    val columns = columns0.filter(c => !header.contains(c)).toVector

    val newIndices = (0 to columns.length).map(_ + header.size)
    val padding    = columns.map(_ => blank)

    Csv(
      rows = rows.map(_ ++ padding),
      header = columns.zip(newIndices).toMap ++ header
    )
  }

  /**
   * Retrieves the number of columns in the CSV file. If the data is good, then
   * this will be a single integer.
   */
  def columns: Set[Int] = rows.map(_.length).toSet

  /**
   * Filters the CSV file by a boolean column.
   */
  def filter(keep: Column[Boolean]): Csv =
    keep match {
      case Column.NA           => self
      case Column.Data(values) => copy(rows = rows.zip(values).collect { case (row, true) => row })
    }

  /**
   * Filters the CSV file by a string predicate that operates on the column
   * of the specified name.
   */
  def filter(name: String)(predicate: String => Boolean): Csv =
    filter(self(name).map(predicate))

  /**
   * Groups the CSV file by the specified columns, attempting to aggregate
   * all others.
   */
  def groupBy(g: String, gs: String*): Csv = {
    val groupIndices = (gs.toSet + g).flatMap(name => header.get(name).fold(Set.empty[Int])(Set(_)))

    def groupKey(row: Vector[String]): Set[String] = groupIndices.map(row(_))

    val aggIndices = header.values.toSet -- groupIndices

    def reduce(row: Vector[String]): String = {
      import scala.util._

      def tryAgg[A](to: String => A, from: A => String, agg: (A, A) => A): Option[String] =
        collectAll(row.map(s => Try(to(s)))).toOption.flatMap(_.reduceOption(agg)).map(from)

      (tryAgg[Int](_.toInt, _.toString, _ + _) orElse
        tryAgg[Double](_.toDouble, _.toString, _ + _) orElse
        tryAgg[Set[String]](Set(_), _.mkString(", "), _ union _)).getOrElse("")
    }

    val aggregator: Vector[(Int, Vector[String] => String)] =
      (groupIndices.map(index => (index, (data: Vector[String]) => data.headOption.getOrElse(""))) ++
        aggIndices.map(index => (index, (data: Vector[String]) => reduce(data)))).toVector.sortBy(_._1)

    Csv(rows.groupBy(groupKey(_)).toVector.map {
      case (key, rows) =>
        aggregator.map {
          case (index, f) => f(rows.map(row => row(index)))
        }
    }, header)
  }

  /**
   * Retains the specified columns, and deletes others, preserving order if
   * possible.
   */
  def retain(column: Int, columns0: Int*): Csv = {
    val columns = Vector(column) ++ Vector(columns0: _*)

    val headersFull = columns.zipWithIndex.map {
      case (old, new0) => (self.header.find(_._2 == old).fold("NA")(_._1), (old, new0))
    }

    val headers2 = headersFull.map { case (key, (old, new0)) => (key, new0) }.toMap

    val oldIndices = headersFull.map { case (_, (old, _)) => old }

    Csv(
      rows.map(row => oldIndices.map(old => row.lift(old).getOrElse(""))),
      headers2
    )
  }

  /**
   * Retains the specified columns, and deletes others, preserving order if
   * possible.
   */
  def retain(name: String, names0: String*): Csv = {
    val names = (Vector(name) ++ Vector(names0: _*)).filter(n => header.contains(n))

    val columns = names.map(header(_))

    if (columns.isEmpty) Csv(Vector(), Map())
    else retain(columns.head, columns.tail: _*)
  }

  /**
   * Sorts by the specified column, in reverse order.
   */
  def sortByReverse(column: Column[Int]): Csv = sortBy(column, true)

  /**
   * Sorts by the specified column, in regular order.
   */
  def sortBy(column: Column[Int]): Csv = sortBy(column, false)

  /**
   * Sorts by the specified column, in the specified order.
   */
  def sortBy(column: Column[Int], reverse: Boolean): Csv = {
    val ordering0 = implicitly[Ordering[Int]]

    val ordering = if (reverse) ordering0.reverse else ordering0

    column match {
      case Column.NA           => self
      case Column.Data(values) => copy(rows = rows.zip(values).sortBy(_._2)(ordering).map(_._1))
    }
  }

  /**
   * Transforms a column.
   */
  def transform(name: String)(f: String => String): Csv =
    self.apply(name) match {
      case Column.NA => self
      case Column.Data(values) =>
        val index = header(name)

        copy(rows = rows.zip(values.map(f)).map { case (row, c) => row.updated(index, c) })
    }

  /**
   * Takes only the specified number of rows.
   */
  def truncate(n: Int): Csv = copy(rows = rows.take(n))

  /**
   * Renders the CSV file as a string.
   */
  override def toString(): String = {
    def escape(s0: String): String = {
      val s1 = s0.replaceAllLiterally("\"", "\"\"")

      if (s1.contains(",")) "\"" + s1 + "\"" else s1
    }

    header.toList.sortBy(_._2).map(_._1).mkString(", ") + "\n" +
      rows.map(_.map(escape(_)).mkString(", ")).mkString("\n")
  }
}
object Csv {

  /**
   * Constructs a CSV file from a string.
   */
  def fromString(string: String): Csv = {
    def process(in: String): String = in.replaceAll("^\"|\"$", "").trim

    val data0 =
      string.split("\r?\n").toVector.map { row =>
        row.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)").toVector
      }

    val header = data0.headOption.map(_.map(_.toLowerCase).zipWithIndex.toMap).getOrElse(Map())

    Csv(data0.drop(1).map(_.map(process(_))), header)
  }
}
