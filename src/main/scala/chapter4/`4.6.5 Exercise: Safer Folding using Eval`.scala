package chapter4

object `4.6.5 Exercise: Safer Folding using Eval` {
  import cats.Eval
  def foldRightNotStackSafe[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
    case head :: tail =>
      fn(head, foldRightNotStackSafe(tail, acc)(fn))
    case Nil =>
      acc
  }

  def foldRightStackSafe[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case head :: tail =>
      Eval.defer(fn(head, foldRightStackSafe(tail, acc)(fn)))
    case Nil =>
      acc
  }
}

object SaferFoldingUsingEvalTest extends App {
  import `4.6.5 Exercise: Safer Folding using Eval`._
  import cats.Eval

  try {
    foldRightNotStackSafe((1 to 50000).toList, 0)(_ + _)
    assert(false)
  } catch {
    case _: StackOverflowError =>
      println("StackOverflowError catched")
  }

  try {
    foldRightStackSafe((1 to 50000).toList, Eval.now(0)) { case (a, b) => b.map(a + _) }.value
    println("no StackOverflowError")
    assert(true)
  } catch {
    case _: StackOverflowError =>
      assert(false)
  }
}
