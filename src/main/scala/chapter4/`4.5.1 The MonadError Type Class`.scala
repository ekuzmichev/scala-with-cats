package chapter4

object `4.5.1 The MonadError Type Class` {}

object TheMonadErrorTypeClass extends App {
  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)
  println(success)

  val failure = monadError.raiseError("Fail!")
  println(failure)

  val handled =
    monadError.handleError(failure) {
      case "Fail!" => monadError.pure("It's ok")
      case _       => monadError.raiseError("It's not ok")
    }
  println(handled)

  val ensured = monadError.ensure(success)("Not good")(_ > 100)
  println(ensured)

  {
    import cats.syntax.applicative._
    import cats.syntax.applicativeError._
    import cats.syntax.monadError._

    val success: ErrorOr[Int] = 42.pure[ErrorOr]
    println(success)
    val failure: ErrorOr[Int] = "Fail!".raiseError[ErrorOr, Int]
    println(failure)
    val ensured = success.ensure("Number to low!")(_ > 1000)
    println(ensured)
  }

  {
    import scala.util.Try
    import cats.syntax.applicativeError._
    import cats.instances.try_._
    val exn: Throwable = new RuntimeException("It's all gone wrong")
    val error: Try[Int] = exn.raiseError[Try, Int]
    println(error)
  }
}
