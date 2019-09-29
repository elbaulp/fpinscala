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

package fpscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService ⇒ Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] =
    (es: ExecutorService) ⇒
      UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: ⇒ A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone                                  = true
    def get(timeout: Long, units: TimeUnit)     = get
    def isCancelled                             = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) ⇒ C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) ⇒ {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: ⇒ Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es ⇒
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def map[A, B](pa: Par[A])(f: A ⇒ B): Par[B] =
    map2(pa, unit(()))((a, _) ⇒ f(a))

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(
      f: (A, B, C) ⇒ D
  ): Par[D] =
    //val c = f.curried
    //map2(map2(pa, pb)((a, b) ⇒ c(a)(b)), pc)(_(_))
    map2(map2(pa, pb)((a, b) ⇒ (c: C) ⇒ f(a, b, c)), pc)(_(_))

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(
      f: (A, B, C, D) ⇒ E
  ): Par[E] = {
    val m3 = map3(pa, pb, pc)((a, b, c) ⇒ (d: D) ⇒ f(a, b, c, d))
    map2(m3, pd)(_(_))
  }

  // map5 would be similar

  def parMap[A, B](ps: List[A])(f: A ⇒ B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A ⇒ Boolean): Par[List[A]] = {
    val pars = as map (asyncF(a ⇒ if (f(a)) List(a) else List.empty))
    map(sequence(pars))(_.flatten)
  }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: ⇒ Par[A]): Par[A] =
    es ⇒ fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(a ⇒ if (a) t else f)
  //    es ⇒
  //      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
  //      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(i ⇒ choices(i))
  //    es ⇒ run(es)(choices(run(es)(n).get))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key)(k ⇒ choices(k))
  //    es ⇒ {
  //      val k = run(es)(key).get
  //      run(es)(choices(k))
  //    }

  // This was chooser. FlatMap can be though as a two steps process:
  // 1. Mappping f: A => Par[B] over our Par[A], this generates a Par[Par[B]],
  // 2. Flattening the result to obtain a Par[B]
  def flatMap[A, B](pa: Par[A])(f: A ⇒ Par[B]): Par[B] =
    es ⇒ {
      val a = run(es)(pa).get
      run(es)(f(a))
    }

  def join[A](a: Par[Par[A]]): Par[A] =
    es ⇒ {
      val pa = run(es)(a).get
      run(es)(pa)
    }

  // We have a Par[Par[A]]
  // 1. Apply f (Identity) to the value, we get a Par[Par[A]]
  // 2. Flattern the result, get a Par[A]
  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  // Simiraly.
  // 1. Apply f to p, to get a Par[Par[B]]
  // 2. Fattern it
  def flatMapViaJoin[A, B](p: Par[A])(f: A ⇒ Par[B]): Par[B] =
    join(map(p)(f))

  def asyncF[A, B](f: A ⇒ B): A ⇒ Par[B] =
    a ⇒ lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List.empty))((h, t) ⇒ map2(h, t)(_ +: _))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {}
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def maxi(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      math max (maxi(l), maxi(r))
    }

  def wordCount(w: List[String]): Int =
    if (w.isEmpty) 0
    else {
      ???
    }

  def main(args: Array[String]): Unit = ???
}
