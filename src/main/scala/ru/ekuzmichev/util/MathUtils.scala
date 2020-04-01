package ru.ekuzmichev.util

object MathUtils {
  def ~=(d1: Double, d2: Double, precision: Double): Boolean = (d1 - d2).abs < precision
}

object MathOps {
  implicit class DoubleOps(d: Double) {
    def ~=(other: Double): Boolean = MathUtils.~=(d, other, 0.001)
  }
}
