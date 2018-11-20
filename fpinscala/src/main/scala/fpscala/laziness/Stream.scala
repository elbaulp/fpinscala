package fpscala.laziness

import Stream._

import scala.annotation.{switch, tailrec}

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def exists_1(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists_1(p)
    case _          => false
  }

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() +: t().toListRecursive
  }

  def toList: List[A] = {
    @tailrec
    def go(acc: List[A])(s: Stream[A]): List[A] = (s: @switch) match {
      case Empty => acc
      case Cons(h, t) => go(h() +: acc)(t())
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
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  // My version
  def take2(n: Int): Stream[A] = (this: @switch) match {
    case Cons(h, t) if n > 1  => cons(h(), t() take2 n - 1)
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }
  // Book's version
  /*
      Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
      calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
      we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
      at the stream at all.
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = (this: @switch) match {
    case Cons(_, t) if n > 1  => t() drop n - 1
    case Cons(_, t) if n == 1 => t()
    case _                    => this
  }

  @tailrec
  final def drop2(n: Int): Stream[A] = (this: @switch) match {
    case Cons(_, t) if n > 0 => t() drop2 n - 1
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = (this: @switch) match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _                    => empty
  }

  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons(a, b)
      else empty
    }

  def forAll(p: A => Boolean): Boolean = this match {
  // more general implementation, althought not tail-recursive:
  //   foldRight(true){(a, b) => p(a) && b}
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _          => true
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOption_1: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  def main(args: Array[String]): Unit = {
    val s = Stream.cons(1, Stream.cons(2, Empty))
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
  }
}

