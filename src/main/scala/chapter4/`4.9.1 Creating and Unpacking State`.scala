package chapter4

object `4.9.1 Creating and Unpacking State` {
  import cats.data.State
  import cats.data.State._
  val a: State[Int, String] = State[Int, String](state => (state, s"The state is $state"))
  val (state, result)       = a.run(10).value
  val justTheState: Int     = a.runS(10).value
  val justTheResult: String = a.runA(10).value

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
}

object CreatingAndUnpackingStateTest extends App {
  import chapter4.`4.9.1 Creating and Unpacking State`.program

  val (state, result) = program.run(1).value
  assert(state == 3)
  assert(result == (1, 2, 3000))
}
