package fpmin.fpmax

import scala.util.Try
import scala.io.StdIn.readLine

object AppLow {
  def main(args: Array[String]): Unit = {
    println("What is your name?")

    val name = readLine()

    println("Hello, " + name + ", welcome to the game!")

    var exec = true

    while (exec) {
      val num = scala.util.Random.nextInt(5) + 1

      println("Dear " + name + ", please guess a number from 1 to 5:")

      val guess = readLine().toInt

      if (guess == num) println("You guessed right, " + name + "!")
      else println("You guessed wrong, " + name + "! The number was: " + num)

      println("Do you want to continue, " + name + "?")

      readLine() match {
        case "y" => exec = true
        case "n" => exec = false
      }
    }
  }
}

object AppMedium {
  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  trait Program[F[_]] {
    def finish[A](a: => A): F[A]

    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

    def map[A, B](fa: F[A], ab: A => B): F[B]
  }
  object Program {
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }
  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B]          = F.map(fa, f)
    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
  }
  def finish[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)

  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]
    def getStrLn: F[String]
  }
  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }
  def putStrLn[F[_]: Console](line: String): F[Unit] = Console[F].putStrLn(line)
  def getStrLn[F[_]: Console]: F[String]             = Console[F].getStrLn

  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }
  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F
  }
  def nextInt[F[_]](upper: Int)(implicit F: Random[F]): F[Int] = Random[F].nextInt(upper)

  final case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun()).unsafeRun())
  }
  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)

    implicit val ProgramIO = new Program[IO] {
      def finish[A](a: => A): IO[A] = IO.point(a)

      def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)

      def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
    }
    implicit val ConsoleIO = new Console[IO] {
      def putStrLn(line: String): IO[Unit] = IO(() => println(line))
      def getStrLn: IO[String]             = IO(() => readLine())
    }
    implicit val RandomIO = new Random[IO] {
      def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
    }
  }

  final case class TestData(input: List[String], output: List[String], nums: List[Int]) {
    def putStrLn(line: String): (TestData, Unit) =
      (copy(output = line :: output), ())

    def getStrLn: (TestData, String) =
      (copy(input = input.drop(1)), input.head)

    def nextInt(upper: Int): (TestData, Int) =
      (copy(nums = nums.drop(1)), nums.head % upper)

    def showResults = output.reverse.mkString("\n")
  }

  final case class TestIO[A](run: TestData => (TestData, A)) { self =>
    def map[B](ab: A => B): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => (t, ab(a)) })

    def flatMap[B](afb: A => TestIO[B]): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => afb(a).run(t) })

    def eval(t: TestData): TestData = run(t)._1
  }
  object TestIO {
    def point[A](a: => A): TestIO[A] = TestIO(t => (t, a))

    implicit val ProgramTestIO = new Program[TestIO] {
      def finish[A](a: => A): TestIO[A] = TestIO.point(a)

      def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] = fa.flatMap(afb)

      def map[A, B](fa: TestIO[A], ab: A => B): TestIO[B] = fa.map(ab)
    }
    implicit val ConsoleTestIO = new Console[TestIO] {
      def putStrLn(line: String): TestIO[Unit] = TestIO(t => t.putStrLn(line))
      def getStrLn: TestIO[String]             = TestIO(t => t.getStrLn)
    }
    implicit val RandomTestIO = new Random[TestIO] {
      def nextInt(upper: Int): TestIO[Int] = TestIO(t => t.nextInt(upper))
    }
  }

  def checkContinue[F[_]: Program: Console](name: String): F[Boolean] =
    for {
      _     <- putStrLn("Do you want to continue, " + name + "?")
      input <- getStrLn.map(_.toLowerCase)
      cont <- input match {
               case "y" => finish(true)
               case "n" => finish(false)
               case _   => checkContinue(name)
             }
    } yield cont

  def printResults[F[_]: Console](input: String, num: Int, name: String): F[Unit] =
    parseInt(input).fold(
      putStrLn("You did not enter a number")
    )(guess =>
      if (guess == num) putStrLn("You guessed right, " + name + "!")
      else putStrLn("You guessed wrong, " + name + "! The number was: " + num)
    )

  def gameLoop[F[_]: Program: Random: Console](name: String): F[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      input <- getStrLn
      _     <- printResults(input, num, name)
      cont  <- checkContinue(name)
      _     <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def main[F[_]: Program: Random: Console]: F[Unit] =
    for {
      _    <- putStrLn("What is your name?")
      name <- getStrLn
      _    <- putStrLn("Hello, " + name + ", welcome to the game!")
      _    <- gameLoop(name)
    } yield ()

  def mainIO: IO[Unit] = main[IO]

  def mainTestIO: TestIO[Unit] = main[TestIO]

  val TestExample =
    TestData(
      input = "John" :: "1" :: "n" :: Nil,
      output = Nil,
      nums = 0 :: Nil
    )

  def runTest = mainTestIO.eval(TestExample).showResults
  /*
    What is your name?
    Hello, John, welcome to the game!
    Dear John, please guess a number from 1 to 5:
    You guessed right, John!
    Do you want to continue, John?
 */
}

object AppHigh {
  object stdlib {
    trait Program[F[_]] {
      def finish[A](a: A): F[A]

      def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

      def map[A, B](fa: F[A], ab: A => B): F[B]
    }
    object Program {
      def apply[F[_]](implicit F: Program[F]): Program[F] = F
    }
    implicit class ProgramSyntax[F[_], A](fa: F[A]) {
      def map[B](ab: A => B)(implicit F: Program[F]): F[B] = F.map(fa, ab)

      def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
    }
    def finish[F[_], A](a: A)(implicit F: Program[F]): F[A] = F.finish(a)

    final case class IO[A](unsafeRun: () => A) { self =>
      final def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

      final def flatMap[B](f: A => IO[B]): IO[B] =
        IO(() => f(self.unsafeRun()).unsafeRun())
    }
    object IO {
      def point[A](a: => A): IO[A] = IO(() => a)

      implicit val ProgramIO = new Program[IO] {
        def finish[A](a: A): IO[A] = IO.point(a)

        def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)

        def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
      }
    }

    trait Console[F[_]] {
      def putStrLn(line: String): F[Unit]
      def getStrLn: F[String]
    }
    object Console {
      def apply[F[_]](implicit F: Console[F]): Console[F] = F

      implicit val ConsoleIO = new Console[IO] {
        def putStrLn(line: String): IO[Unit] = IO(() => println(line))
        def getStrLn: IO[String]             = IO(() => readLine())
      }
    }
    def putStrLn[F[_]: Console](line: String): F[Unit] = Console[F].putStrLn(line)
    def getStrLn[F[_]: Console]: F[String]             = Console[F].getStrLn

    trait Random[F[_]] {
      def nextInt(upper: Int): F[Int]
    }
    object Random {
      def apply[F[_]](implicit F: Random[F]): Random[F] = F

      implicit val RandomIO = new Random[IO] {
        def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
      }
    }
    def nextInt[F[_]: Random](upper: Int): F[Int] = Random[F].nextInt(upper)
  }

  import stdlib._

  final case class TestData(input: List[String], output: List[String], nums: List[Int]) {
    def showResults = output.reverse.mkString("\n")

    def nextInt: (TestData, Int) = (copy(nums = nums.drop(1)), nums.head)

    def putStrLn(line: String): (TestData, Unit) = (copy(output = line :: output), ())

    def getStrLn: (TestData, String) = (copy(input = input.drop(1)), input.head)
  }

  final case class TestIO[A](run: TestData => (TestData, A)) { self =>
    def map[B](f: A => B): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => (t, f(a)) })

    def flatMap[B](f: A => TestIO[B]): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => f(a).run(t) })

    def eval(t: TestData): TestData = self.run(t)._1
  }
  object TestIO {
    def point[A](a: => A): TestIO[A] = TestIO(t => (t, a))

    implicit val RandomTestIO = new Random[TestIO] {
      def nextInt(upper: Int): TestIO[Int] =
        TestIO(t => t.nextInt)
    }
    implicit val ProgramTestIO = new Program[TestIO] {
      def finish[A](a: A): TestIO[A] = TestIO.point(a)

      def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] = fa.flatMap(afb)

      def map[A, B](fa: TestIO[A], ab: A => B): TestIO[B] = fa.map(ab)
    }
    implicit val ConsoleTestIO = new Console[TestIO] {
      def putStrLn(line: String): TestIO[Unit] =
        TestIO(t => t.putStrLn(line))
      def getStrLn: TestIO[String] =
        TestIO(t => t.getStrLn)
    }
  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def checkAnswer[F[_]: Console](name: String, num: Int, guess: Int): F[Unit] =
    if (num == guess) putStrLn("You guessed right, " + name + "!")
    else putStrLn("You guessed wrong, " + name + "! The number was: " + num)

  def checkContinue[F[_]: Program: Console](name: String): F[Boolean] =
    for {
      _      <- putStrLn("Do you want to continue, " + name + "?")
      choice <- getStrLn.map(_.toLowerCase)
      cont <- if (choice == "y") finish(true)
             else if (choice == "n") finish(false)
             else checkContinue(name)
    } yield cont

  def gameLoop[F[_]: Program: Console: Random](name: String): F[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      guess <- getStrLn
      _ <- parseInt(guess).fold(
            putStrLn("That is not a valid selection, " + name + "!")
          )((guess: Int) => checkAnswer(name, num, guess))
      cont <- checkContinue(name)
      _    <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def main[F[_]: Program: Console: Random]: F[Unit] =
    for {
      _    <- putStrLn("What is your name?")
      name <- getStrLn
      _    <- putStrLn("Hello, " + name + ", welcome to the game!")
      _    <- gameLoop(name)
    } yield ()

  def mainIO: IO[Unit] = main[IO]

  def mainTestIO: TestIO[Unit] = main[TestIO]

  val TestExample = TestData(input = "john" :: "1" :: "n" :: Nil, output = Nil, nums = 0 :: Nil)
}

object AppMax {
  object stdlib {
    trait Program[F[_]] {
      def finish[A](a: A): F[A]

      def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

      def map[A, B](fa: F[A], ab: A => B): F[B]
    }
    object Program {
      def apply[F[_]](implicit F: Program[F]): Program[F] = F
    }
    implicit class ProgramSyntax[F[_], A](fa: F[A]) {
      def map[B](ab: A => B)(implicit F: Program[F]): F[B] = F.map(fa, ab)

      def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
    }
    def finish[F[_], A](a: A)(implicit F: Program[F]): F[A] = F.finish(a)

    final case class IO[A](unsafeRun: () => A) { self =>
      final def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

      final def flatMap[B](f: A => IO[B]): IO[B] =
        IO(() => f(self.unsafeRun()).unsafeRun())
    }
    object IO {
      def point[A](a: => A): IO[A] = IO(() => a)

      implicit val ProgramIO = new Program[IO] {
        def finish[A](a: A): IO[A] = IO.point(a)

        def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)

        def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
      }
    }

    sealed trait ConsoleOut {
      def en: String
    }
    object ConsoleOut {
      case class YouGuessedRight(name: String) extends ConsoleOut {
        def en = "You guessed right, " + name + "!"
      }
      case class YouGuessedWrong(name: String, num: Int) extends ConsoleOut {
        def en = "You guessed wrong, " + name + "! The number was: " + num
      }
      case class DoYouWantToContinue(name: String) extends ConsoleOut {
        def en = "Do you want to continue, " + name + "?"
      }
      case class PleaseGuess(name: String) extends ConsoleOut {
        def en = "Dear " + name + ", please guess a number from 1 to 5:"
      }
      case class ThatIsNotValid(name: String) extends ConsoleOut {
        def en = "That is not a valid selection, " + name + "!"
      }
      case object WhatIsYourName extends ConsoleOut {
        def en = "What is your name?"
      }
      case class WelcomeToGame(name: String) extends ConsoleOut {
        def en = "Hello, " + name + ", welcome to the game!"
      }
    }

    trait Console[F[_]] {
      def putStrLn(line: ConsoleOut): F[Unit]
      def getStrLn: F[String]
    }
    object Console {
      def apply[F[_]](implicit F: Console[F]): Console[F] = F

      implicit val ConsoleIO = new Console[IO] {
        def putStrLn(line: ConsoleOut): IO[Unit] = IO(() => println(line.en))
        def getStrLn: IO[String]                 = IO(() => readLine())
      }
    }
    def putStrLn[F[_]: Console](line: ConsoleOut): F[Unit] = Console[F].putStrLn(line)
    def getStrLn[F[_]: Console]: F[String]                 = Console[F].getStrLn

    trait Random[F[_]] {
      def nextInt(upper: Int): F[Int]
    }
    object Random {
      def apply[F[_]](implicit F: Random[F]): Random[F] = F

      implicit val RandomIO = new Random[IO] {
        def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
      }
    }
    def nextInt[F[_]: Random](upper: Int): F[Int] = Random[F].nextInt(upper)
  }

  import stdlib._

  case class TestData(input: List[String], output: List[ConsoleOut], nums: List[Int]) {
    def showResults = output.reverse.map(_.en).mkString("\n")

    def nextInt: (TestData, Int) = (copy(nums = nums.drop(1)), nums.head)

    def putStrLn(line: ConsoleOut): (TestData, Unit) = (copy(output = line :: output), ())

    def getStrLn: (TestData, String) = (copy(input = input.drop(1)), input.head)
  }

  case class TestIO[A](run: TestData => (TestData, A)) { self =>
    def map[B](f: A => B): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => (t, f(a)) })

    def flatMap[B](f: A => TestIO[B]): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => f(a).run(t) })

    def eval(t: TestData): TestData = self.run(t)._1
  }
  object TestIO {
    def point[A](a: => A): TestIO[A] = TestIO(t => (t, a))

    implicit val RandomTestIO = new Random[TestIO] {
      def nextInt(upper: Int): TestIO[Int] =
        TestIO(t => t.nextInt)
    }
    implicit val ProgramTestIO = new Program[TestIO] {
      def finish[A](a: A): TestIO[A] = TestIO.point(a)

      def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] = fa.flatMap(afb)

      def map[A, B](fa: TestIO[A], ab: A => B): TestIO[B] = fa.map(ab)
    }
    implicit val ConsoleTestIO = new Console[TestIO] {
      def putStrLn(line: ConsoleOut): TestIO[Unit] =
        TestIO(t => t.putStrLn(line))
      def getStrLn: TestIO[String] =
        TestIO(t => t.getStrLn)
    }
  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def checkAnswer[F[_]: Console](name: String, num: Int, guess: Int): F[Unit] =
    if (num == guess) putStrLn(ConsoleOut.YouGuessedRight(name))
    else putStrLn(ConsoleOut.YouGuessedWrong(name, num))

  def checkContinue[F[_]: Program: Console](name: String): F[Boolean] =
    for {
      _      <- putStrLn(ConsoleOut.DoYouWantToContinue(name))
      choice <- getStrLn.map(_.toLowerCase)
      cont <- if (choice == "y") finish(true)
             else if (choice == "n") finish(false)
             else checkContinue(name)
    } yield cont

  def gameLoop[F[_]: Program: Console: Random](name: String): F[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn(ConsoleOut.PleaseGuess(name))
      guess <- getStrLn
      _ <- parseInt(guess).fold(
            putStrLn(ConsoleOut.ThatIsNotValid(name))
          )((guess: Int) => checkAnswer(name, num, guess))
      cont <- checkContinue(name)
      _    <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def main[F[_]: Program: Console: Random]: F[Unit] =
    for {
      _    <- putStrLn(ConsoleOut.WhatIsYourName)
      name <- getStrLn
      _    <- putStrLn(ConsoleOut.WelcomeToGame(name))
      _    <- gameLoop(name)
    } yield ()

  def mainIO: IO[Unit] = main[IO]

  def mainTestIO: TestIO[Unit] = main[TestIO]

  val TestExample = TestData(input = "john" :: "1" :: "n" :: Nil, output = Nil, nums = 0 :: Nil)
}
