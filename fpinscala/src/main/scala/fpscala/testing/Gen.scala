/*
 * Copyright (C) 2018  Alejandro Alcalde
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

package fpscala.testing

import scala.language.{ implicitConversions, postfixOps }

import Gen._
import Prop._
import fpscala.laziness.Stream
import fpscala.state._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
case class Prop(run: (MaxSize, TestCases, RNG) ⇒ Result) {

  def &&(p: Prop) = Prop { (m, t, rng) ⇒
    run(m, t, rng) match {
      case Passed | Proved ⇒ p run (m, t, rng)
      case o ⇒ o
    }
  }

  def ||(p: Prop): Prop = Prop { (m, t, rng) ⇒
    run(m, t, rng) match {
      case Falsified(msg, _) ⇒ p.tag(msg) run (m, t, rng)
      case x ⇒ x
    }
  }

  def tag(msg: FailedCase) = Prop {
    (m, t, rng) ⇒
      run(m, t, rng) match {
        case Falsified(e, c) ⇒ Falsified(s"$msg\n$e", c)
        case x ⇒ x
      }
  }

}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(
    failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  def check(p: ⇒ Boolean): Prop = Prop { (_, _, _) ⇒
    if (p) Proved else Falsified("()", 0)
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng ⇒ Some(g.sample.run(rng)))

  def apply(f: (TestCases, RNG) ⇒ Result): Prop =
    Prop { (_, n, rng) ⇒ f(n, rng) }

  def forAll[A](g: SGen[A])(f: A ⇒ Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int ⇒ Gen[A])(f: A ⇒ Boolean): Prop = Prop {
    (max, n, rng) ⇒
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i ⇒ forAll(g(i))(f))
      val prop: Prop =
        props.map(p ⇒ Prop { (max, n, rng) ⇒
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A ⇒ Boolean): Prop = Prop {
    (n, rng) ⇒
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) ⇒ try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception ⇒ Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) ⇒
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed ⇒
        println(s"+ OK, passed $testCases tests")
      case Proved ⇒
        println("+ OK, proved property")
    }
}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A ⇒ B): Gen[B] =
    Gen(sample map f)

  def map2[B, C](g: Gen[B])(f: (A, B) ⇒ C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A ⇒ Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_) sample))

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (Gen listOfN (_, this))

  def unsized: SGen[A] = SGen(_ ⇒ this)
}

object Gen {
  def unit[A](a: ⇒ A): Gen[A] =
    Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n ⇒ start + n % (stopExclusive - start)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeInt).map(n ⇒ n % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    //listOfN(1, g)
    SGen(n ⇒ g.listOfN(n max 1))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen { g listOfN }
  //    SGen{listOfN(_, g)}

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap (if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val thr = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(p ⇒ if (p < thr) g1._1.sample else g2._1.sample))
  }

}

case class SGen[+A](forSize: Int ⇒ Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A ⇒ B): SGen[B] = SGen {
    forSize(_) map f
  }

  def flatMap[B](f: A ⇒ SGen[B]): SGen[B] = SGen {
    n ⇒
      {
        forSize(n) flatMap { f(_) forSize n }
      }
  }
}

object App {
  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf1(smallInt)) { ns ⇒
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)

    val sortedProp = forAll(listOf(smallInt)) { ns ⇒
      val nss = ns.sorted
      (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
        case (a, b) ⇒ a > b
      }) &&
        !ns.exists(!nss.contains(_)) &&
        !nss.exists(!ns.contains(_))
    }
    Prop.run(sortedProp)
  }
}
