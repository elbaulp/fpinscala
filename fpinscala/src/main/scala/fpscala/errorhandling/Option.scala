package fpscala.errorhandling

import scala.annotation.{ switch, tailrec }
import scala.{ Option ⇒ _, Some ⇒ _, Either ⇒ _, _ } // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A ⇒ B): Option[B] = this match {
    case None ⇒ None
    case Some(x) ⇒ Some(f(x))
  }

  def getOrElse[B >: A](default: ⇒ B): B = this match {
    case None ⇒ default
    case Some(x) ⇒ x
  }

  def flatMap[B](f: A ⇒ Option[B]): Option[B] =
    map(f) getOrElse None
  //def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
  //  case None => None
  //  case Some(a) => f(a)
  //}

  def orElse[B >: A](ob: ⇒ Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob
  /*
  Again, we can implement this with explicit pattern matching.
  */
  //def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
  //  case None => ob
  //  case _ => this
  //}

  def filter(f: A ⇒ Boolean): Option[A] = this match {
    case Some(x) if f(x) ⇒ this
    case _ ⇒ None
  }

  def filter_1(f: A ⇒ Boolean): Option[A] =
    flatMap(x ⇒ if (f(x)) Some(x) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] =
    a flatMap (aa ⇒ b map (bb ⇒ f(aa, bb)))

  //  @tailrec
  def sequence[A](a: List[Option[A]]): Option[List[A]] = (a: @switch) match {
    case Nil ⇒ Some(Nil)
    case h :: t ⇒  h flatMap (hh ⇒ sequence(t) map (hh +: _))
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ +: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = (a: @switch) match {
    case Nil => Some(Nil)
    case h :: t =>
      map2(f(h), traverse(t)(f))(_ :: _)
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}

  def main(args: Array[String]): Unit = {
    val a = List("1", "2", "3", "a")
    val b = List("1", "2", "3")
    val c = List(Some(1), Some(2))
    val d = List(None, Some(2))

//    println(s"${Option.traverse(a)(x => Try{x.toInt})}")
//    println(s"${Option.traverse(b)(x => Try{x.toInt})}")
    println(s"${Option.sequenceViaTraverse(c)}")
    println(s"${Option.sequenceViaTraverse(d)}")

  }
}


