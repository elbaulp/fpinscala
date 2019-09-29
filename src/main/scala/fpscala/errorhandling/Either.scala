package fpscala.errorhandling

import scala.annotation.switch
import scala.{
  Either ⇒ _,
  Left ⇒ _,
  Option ⇒ _,
  Right ⇒ _
} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A ⇒ B): Either[E, B] = this match {
    case Right(a) ⇒ Right(f(a))
    case Left(e)  ⇒ Left(e)
  }

  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  ⇒ Left(e)
    case Right(a) ⇒ f(a)
  }

  def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Right(a) ⇒ Right(a)
    case Left(_)  ⇒ b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] =
    flatMap(a ⇒ b map (bb ⇒ f(a, bb)))
  //for { a <- this; b1 <- b } yield f(a,b1) // Equivalent
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A ⇒ Either[E, B]): Either[E, List[B]] =
    (es: @switch) match {
      case Nil    ⇒ Right(Nil)
      case h :: t ⇒ (f(h) map2 traverse(t)(f))(_ +: _)
    }

  def traverse_1[E, A, B](
      es: List[A]
  )(f: A ⇒ Either[E, B]): Either[E, List[B]] =
    es.:\[Either[E, List[B]]](Right(Nil))((t, z) ⇒ f(t).map2(z)(_ +: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception ⇒ Left(e)
    }

  def Try[A](a: ⇒ A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception ⇒ Left(e)
    }

  def main(args: Array[String]): Unit = {
    val right1                  = Right(1)
    val right2                  = Right(2)
    val left1: Either[Int, Int] = Left(1)
    val left2: Either[Int, Int] = Left(2)

    println(s"map: ${Right("a").map(x ⇒ Try { x.toInt })}")
    println(s"map: ${Right("1").map(x ⇒ Try { x.toInt })}")
    println(s"flatMap: ${Right("hello").flatMap(x ⇒ Try { x.toInt })}")
    println(s"map2: ${right1.map2(right2)(_ + _)}")
    println(s"map2: ${right1.map2(left1)(_ + _)}")
    println(s"map2: ${left2.map2(right2)(_ + _)}")

    println(s"Traverse: ${Either
      .traverse(List("1", "1", "1", "1", "1", "a"))(x ⇒ Try { x.toInt })}")
    println(s"Traverse: ${Either
      .traverse(List("1", "1", "1", "1", "1", "1"))(x ⇒ Try { x.toInt })}")
  }
}
