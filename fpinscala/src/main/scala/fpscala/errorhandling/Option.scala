package fpscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map (f) getOrElse None
  //def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
  //  case None => None
  //  case Some(a) => f(a)
  //}


  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob
  /*
  Again, we can implement this with explicit pattern matching.
  */
  //def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
  //  case None => ob
  //  case _ => this
  //}

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(x => if (f(x)) Some(x) else None)
}



case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
