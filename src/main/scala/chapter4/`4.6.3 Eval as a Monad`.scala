package chapter4

object `4.6.3 Eval as a Monad` {}

object EvalAsMonadTest extends App {
  import cats.Eval
  val greeting =
    Eval.always { println("Step 1"); "Hello" }.map { str => println("Step 2"); s"$str, Bob" }

  println(greeting.value)
  println(greeting.value)

  val ans = for {
    a <- Eval.now { println("Calculating A"); 40 }
    b <- Eval.always { println("Calculating B"); 2 }
  } yield {
    println("Adding A and B"); a + b
  }

  println(ans.value)
  println(ans.value)

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }
  println(factorial(50000).value)
}
