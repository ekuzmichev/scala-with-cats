package ru.ekuzmichev.chapter3

object `3.6.2.1 Transforma􏰀ve Thinking with imap` {
  trait Codec[A] {
    self =>

    def encode(value: A): String

    def decode(s: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(s: String): B = dec(self.decode(s))
    }
  }

  object Codec {
    def apply[A](implicit instance: Codec[A]): Codec[A] = instance

    implicit val stringCodec: Codec[String] =
      new Codec[String] {
        def encode(value: String): String = value
        def decode(value: String): String = value
      }
    implicit val intCodec: Codec[Int]              = Codec[String].imap(_.toInt, _.toString)
    implicit val doubleCodec: Codec[Double]        = Codec[String].imap(_.toDouble, _.toString)
    implicit val booleanCodec: Codec[Boolean]      = stringCodec.imap(_.toBoolean, _.toString)
    implicit def boxCodec[A: Codec]: Codec[Box[A]] = Codec[A].imap(Box(_), _.value)
  }

  case class Box[A](value: A)
}

object TransformativeThinkingWithImapTest extends App {
  import `3.6.2.1 Transforma􏰀ve Thinking with imap`._

  assert {
    val codec = Codec[Int]
    codec.decode(codec.encode(42)) == 42
  }
  assert {
    val codec = Codec[Double]
    codec.decode(codec.encode(1.0)) == 1.0
  }

  assert {
    val codec = Codec[Box[Int]]
    codec.decode(codec.encode(Box(42))) == Box(42)
  }
}
