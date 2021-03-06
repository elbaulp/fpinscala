/*
 * Copyright (C) 2019  Alejandro Alcalde
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fpscala.monoids

import fpscala.parallelism.Nonblocking._
import fpscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero                       = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero                         = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero                      = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero                      = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2
    val zero                                  = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 & a2
    val zero                                  = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val zero                                        = None
  }

  def endoMonoid[A]: Monoid[A ⇒ A] = new Monoid[A ⇒ A] {
    def op(f: A ⇒ A, h: A ⇒ A): A ⇒ A = f compose h
    def zero                          = identity
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  //trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(for {
      x ← gen
      y ← gen
      z ← gen
    } yield (x, y, z))(
      v ⇒
        // Assoc law
        m.op(v._1, m.op(v._2, v._3)) == m.op(m.op(v._1, v._2), v._3)
    ) &&
      // Idendity law
      forAll(gen)(x ⇒ m.op(x, m.zero) == x && m.op(m.zero, x) == x)

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A ⇒ B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) ⇒ B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) ⇒ B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero              = m.zero
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A ⇒ B): B =
    if (as.length == 0)
      m.zero
    else if (as.length == 1)
      f(as head)
    else {
      val (a, b) = as.splitAt(as.length / 2)
      m.op(foldMapV(a, m)(f), foldMapV(b, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val orderMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
      val zero = None
      def op(a: Option[(Int, Int, Boolean)], b: Option[(Int, Int, Boolean)]) =
        (a, b) match {
          case (Some((x1, y1, b1)), Some((x2, y2, b2))) =>
            Some((x1 min x2, y1 max y2, b1 & b2 && y1 <= x2))
          case (None, x) => x
          case (x, None) => x
        }
    }
    foldMapV(ints, orderMonoid)(i => Some((i, i, true)))
      .map(_._3)
      .getOrElse(true)
  }

//  sealed trait WC
//  case class Stub(chars: String) extends WC
//  case class Part(lStub: String, words: Int, rStub: String) extends WC
//
//  def par[A](m: Monoid[A]): Monoid[Par[A]] =
//    ???
//
//  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A ⇒ B): Par[B] =
//    ???
//
//  val wcMonoid: Monoid[WC] = ???
//
//  def count(s: String): Int = ???
//
//  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
//    ???
//
//  def functionMonoid[A, B](B: Monoid[B]): Monoid[A ⇒ B] =
//    ???
//
//  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
//    ???
//
//  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
//    ???
//
  def main(args: Array[String]): Unit = {
    println("Exercise 10.8. FoldMapV")
    println(
      s"${Monoid.foldMapV(IndexedSeq("Lorem", "Ipsum"), Monoid.stringMonoid)(x => x + " ")}"
    )

    println("Exercise 10.9. Ordered")
    println(
      s"${Monoid.ordered(IndexedSeq(1, 2, 3, 4, 5, 6))}"
    )
    println(
      s"${Monoid.ordered(IndexedSeq(1, 2, 19, 4, 5, 6))}"
    )
  }
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) ⇒ B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) ⇒ B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A ⇒ B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    ???
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) ⇒ B) =
    ???
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) ⇒ B) =
    ???
  override def foldMap[A, B](as: List[A])(f: A ⇒ B)(mb: Monoid[B]): B =
    ???
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) ⇒ B) =
    ???
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) ⇒ B) =
    ???
  override def foldMap[A, B](as: IndexedSeq[A])(f: A ⇒ B)(mb: Monoid[B]): B =
    ???
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) ⇒ B) =
    ???
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) ⇒ B) =
    ???
}

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A ⇒ B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) ⇒ B) =
    ???
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) ⇒ B) =
    ???
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A ⇒ B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) ⇒ B) =
    ???
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) ⇒ B) =
    ???
}
