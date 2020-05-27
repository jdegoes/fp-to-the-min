package fpmin

package object csv {
  import scala.util.{ Success, Try }

  def collectAll[A](tries: Iterable[Try[A]]): Try[Vector[A]] =
    tries.foldLeft[Try[Vector[A]]](Success(Vector.empty[A])) {
      case (acc, t) => acc.flatMap(acc => t.map(t => acc :+ t))
    }
}
