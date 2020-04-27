package ru.ekuzmichev.chapter7

object `7.1.2 Exercise: Reflecting on Folds`

object ReflectingOnFondsTest extends App {
  println(List(1, 2, 3).foldLeft(List.empty[Int])((accum, elem) => elem :: accum))
  println(List(1, 2, 3).foldRight(List.empty[Int])((elem, accum) => elem :: accum))
}
