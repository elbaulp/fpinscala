package fpscala.laziness

import fpscala.laziness.Stream._

import scala.annotation.{switch, tailrec}

trait Stream[+A] {

  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) ⇒
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ ⇒ z
    }

  def exists(p: A ⇒ Boolean): Boolean =
    foldRight(false)((a, b) ⇒ p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def exists_1(p: A ⇒ Boolean): Boolean = this match {
    case Cons(h, t) ⇒ p(h()) || t().exists_1(p)
    case _          ⇒ false
  }

  @tailrec
  final def find(f: A ⇒ Boolean): Option[A] = this match {
    case Empty      ⇒ None
    case Cons(h, t) ⇒ if (f(h())) Some(h()) else t().find(f)
  }

  // Reuse filter to define find, althought filter transform the
  // whole stream, that transformation is done lazily, so find
  // terminates as soon as a match is found.
  def find_1(f: A ⇒ Boolean): Option[A] =
    filter(f).headOption

  def toListRecursive: List[A] = this match {
    case Empty      ⇒ Nil
    case Cons(h, t) ⇒ h() +: t().toListRecursive
  }

  def toList: List[A] = {
    @tailrec
    def go(acc: List[A])(s: Stream[A]): List[A] = (s: @switch) match {
      case Empty      ⇒ acc
      case Cons(h, t) ⇒ go(h() +: acc)(t())
    }

    go(Nil)(this).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
   */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) ⇒
        buf += h()
        go(t())
      case _ ⇒ buf.toList
    }

    go(this)
  }

  // My version
  def take2(n: Int): Stream[A] = (this: @switch) match {
    case Cons(h, t) if n > 1  ⇒ cons(h(), t() take2 n - 1)
    case Cons(h, _) if n == 1 ⇒ cons(h(), empty)
    case _                    ⇒ empty
  }

  // Book's version
  /*
      Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
      calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
      we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
      at the stream at all.
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  ⇒ cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 ⇒ cons(h(), empty)
    case _                    ⇒ empty
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(a, b), n) if n > 1 ⇒ Some((a(), (b(), n - 1)))
      case (Cons(a, _), 1)          ⇒ Some((a(), (empty, 0)))
      case _                        ⇒ None
    }

  @tailrec
  final def drop(n: Int): Stream[A] = (this: @switch) match {
    case Cons(_, t) if n > 1  ⇒ t() drop n - 1
    case Cons(_, t) if n == 1 ⇒ t()
    case _                    ⇒ this
  }

  @tailrec
  final def drop2(n: Int): Stream[A] = (this: @switch) match {
    case Cons(_, t) if n > 0 ⇒ t() drop2 n - 1
    case _                   ⇒ this
  }

  def takeWhile(p: A ⇒ Boolean): Stream[A] = (this: @switch) match {
    case Cons(h, t) if p(h()) ⇒ cons(h(), t() takeWhile p)
    case _                    ⇒ empty
  }

  def takeWhile_1(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) ⇒
      if (p(a)) cons(a, b)
      else empty
    }

  def takeWhileViaUnfold(p: A ⇒ Boolean): Stream[A] =
    unfold(this) {
      case Cons(a, b) if p(a()) ⇒ Some((a(), b()))
      case _                    ⇒ None
    }

  def forAll(p: A ⇒ Boolean): Boolean = this match {
    // more general implementation, althought not tail-recursive:
    //   foldRight(true){(a, b) => p(a) && b}
    case Cons(h, t) ⇒ p(h()) && t().forAll(p)
    case _          ⇒ true
  }

  def headOption: Option[A] = this match {
    case Empty      ⇒ None
    case Cons(h, _) ⇒ Some(h())
  }

  def headOption_1: Option[A] =
    foldRight(None: Option[A])((a, _) ⇒ Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A ⇒ B): Stream[B] =
    foldRight(empty[B])((a, b) ⇒ cons(f(a), b))

  def mapViaUnfold[B](f: A ⇒ B): Stream[B] =
    unfold(this) {
      case Cons(h, t) ⇒ Some((f(h()), t()))
      case _          ⇒ None
    }

  def filter(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, b) ⇒ if (p(a)) cons(a, b) else b)

  def append[B >: A](s: ⇒ Stream[B]): Stream[B] =
    foldRight(s)((a, b) ⇒ cons(a, b))

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) ⇒ f(a) append b)

  def zipWith[B, C](s2: Stream[B])(f: (A, B) ⇒ C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(a, b), Cons(c, d)) ⇒
        Some((f(a(), c()), (b(), d())))
      case _ ⇒ None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](
      s2: Stream[B]
  )(f: (Option[A], Option[B]) ⇒ C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) ⇒ None
      case (Cons(h, t), Empty) ⇒
        Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) ⇒
        Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) ⇒
        Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /*
  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted, and the termination if a nonmatching element is found or the first stream is exhausted.
   */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) ⇒ h == h2
    }

  /*
  The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
   */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty ⇒ None
      case s     ⇒ Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
   */
  def scanRight[B](z: B)(f: (A, ⇒ B) ⇒ B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) ⇒ {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2      = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () ⇒ A, t: () ⇒ Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() ⇒ head, () ⇒ tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant_1[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() ⇒ a, () ⇒ tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  // cons(0, go(1, 0 + 1)), cons(0, cons(1, cons(1, 1 + 1))

  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(z) match {
    case None         ⇒ empty
    case Some((a, s)) ⇒ cons(a, unfold(s)(f))
  }

  val fibsViaUnfold =
    unfold((0, 1)) { case (f0, f1) ⇒ Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int) =
    unfold(n)(n ⇒ Some((n, n + 1)))

  def constantViaUnfold(c: Int) =
    unfold(c)(_ ⇒ Some((c, c)))

  val onesViaUnfold =
    constantViaUnfold(1)

  def main(args: Array[String]): Unit = {
    val s = cons(1, cons(2, cons(3, empty))) //cons(4, cons(5, empty)))))

    println(s"Tolist ${s.toListRecursive}")
    println(s"Tolist ${s.toList}")
    println(s"Take 0 ${s.take(0).toListFast}")
    println(s"Take 1 ${s.take(1).toListFast}")
    println(s"Take 2 ${s.take(2).toListFast}")

    println(s"Take2 0 ${s.take2(0).toListFast}")
    println(s"Take2 1 ${s.take2(1).toListFast}")
    println(s"Take2 2 ${s.take2(2).toListFast}")

    println(s"Drop 0 ${s.drop(0).toListFast}")
    println(s"Drop 1 ${s.drop(1).toListFast}")
    println(s"Drop 2 ${s.drop(2).toListFast}")
    println(s"Drop 20 ${s.drop(20).toListFast}")

    println(s"takeWhile ${s.takeWhile(_ > 0).toListFast}")
    println(s"takeWhile ${s.takeWhile(_ < 0).toListFast}")
    println(s"takeWhile ${s.takeWhile(_ == 1).toListFast}")

    println(s"takeWhile_1 ${s.takeWhile_1(_ > 0).toListFast}")
    println(s"takeWhile_1 ${s.takeWhile_1(_ < 0).toListFast}")
    println(s"takeWhile_1 ${s.takeWhile_1(_ == 1).toListFast}")

    println(s"forAll ${s.forAll(_ == 1)}")
    println(s"forAll ${s.forAll(_ > 0)}")

    println(s"map ${s.map(_ * 10).toListFast}")
    println(s"map ${empty[Int].map(_ * 10).toListFast}")

    println(s"filter ${s.filter(_ % 2 == 0).toListFast}")
    println(s"filter ${s.filter(_ == -1).toListFast}")

    println(s"append ${s.append(cons("s", empty)).toListFast}")
    println(s"append ${s.append(empty).toListFast}")

    println(s"flatmap ${s.flatMap(x ⇒ cons(x, empty)).toListFast}")

    println(s"constant ${constant(1).take(10).toListFast}")
    println(s"From ${from(100).take(10).toListFast}")

    println(s"fibs ${fibs.take(10).toListFast}")

    println(
      s"HHHH ${unfold[Int, Int](0)(x ⇒ Some(x + x, x + x)).take(10).toListFast}"
    )

    println(s"tails ${s.tails.take(10).map(_.toListFast).toListFast}")

    println(s"scanRight ${s.scanRight(0)(_ + _).toListFast}")
  }
}
