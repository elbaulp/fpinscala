package fpscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil ⇒ 0 // The sum of the empty list is 0.
      case Cons(x, xs) ⇒
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x, xs)  ⇒ x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          ⇒ x
    case Nil                                   ⇒ 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
    case Cons(h, t)                            ⇒ h + sum(t)
    case _                                     ⇒ 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        ⇒ a2
      case Cons(h, t) ⇒ Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = // Utility functions
    as match {
      case Nil         ⇒ z
      case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) ⇒ x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Better to through an exception for empty tail
  def tail[A](l: List[A]): List[A] = l match {
    case Nil        ⇒ sys.error("Tail of list is empty")
    case Cons(_, t) ⇒ t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil        ⇒ sys.error("setHead on empty list")
    case Cons(_, t) ⇒ Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil        ⇒ Nil
        case Cons(_, t) ⇒ drop(t, n - 1)
      }

  def dropWhile[A](l: List[A], f: A ⇒ Boolean): List[A] = l match {
    case Cons(h, t) if f(h) ⇒ dropWhile(t, f)
    case _                  ⇒ l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          ⇒ sys.error("init on empty list")
    case Cons(_, Nil) ⇒ Nil
    case Cons(h, t)   ⇒ Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, z) ⇒ z + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) ⇒ B): B = l match {
    case Nil        ⇒ z
    case Cons(h, t) ⇒ foldLeft(t, f(z, h))(f)
  }

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((z, _) ⇒ z + 1)

  def reverse[A](l: List[A]) = foldLeft(l, List[A]())((acc, h) ⇒ Cons(h, acc))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B =
    foldLeft(reverse(l), z)((b, a) ⇒ f(a, b))

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  // Ex 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  // Ex 3.16
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) ⇒ Cons(h + 1, t))

  // Ex 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) ⇒ Cons(h.toString, t))

  // Ex 3.18
  def map[A, B](l: List[A])(f: A ⇒ B): List[B] =
    foldRight(l, Nil: List[B])((h, t) ⇒ Cons(f(h), t))

  // Ex 3.19
  def filter[A](as: List[A])(f: A ⇒ Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) ⇒ if (f(h)) Cons(h, t) else t)

  // Ex 3.20
  def flatMap[A, B](as: List[A])(f: A ⇒ List[B]): List[B] =
    concat(map(as)(f))

  // Ex 3.21
  def filter2[A](l: List[A])(f: A ⇒ Boolean): List[A] =
    flatMap(l)(a ⇒ if (f(a)) List(a) else Nil)

  // Ex 3.22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _)                   ⇒ Nil
    case (_, Nil)                   ⇒ Nil
    case (Cons(x, xs), Cons(y, ys)) ⇒ Cons(x + y, addPairwise(xs, ys))
  }

  // Ex 3.23
  def zipWith[A](a: List[A], b: List[A])(f: (A, A) ⇒ A): List[A] =
    (a, b) match {
      case (Nil, _)                   ⇒ Nil
      case (_, Nil)                   ⇒ Nil
      case (Cons(x, xs), Cons(y, ys)) ⇒ Cons(f(x, y), zipWith(xs, ys)(f))
    }

}
