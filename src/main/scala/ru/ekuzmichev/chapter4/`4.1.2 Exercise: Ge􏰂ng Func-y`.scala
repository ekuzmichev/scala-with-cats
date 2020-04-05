package ru.ekuzmichev.chapter4

import scala.language.higherKinds

object `4.1.2 Exercise: Ge􏰂ng Func-y` {
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

    def map[A, B](value: F[A])(f: A => B): F[B] = flatMap(value)(a => pure(f(a)))
  }
}
