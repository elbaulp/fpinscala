package fpscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  //  def size[A](t: Tree[A]): Int = {
  //    @tailrec
  //    def go(z: Int)(l: Tree[A], r: Tree[A]): Int = t match {
  //      case Branch(ll, rr) => go(z + 2)(ll, rr)
  //      case Leaf(_) => z + 1
  //    }
  //    t match {
  //      case _: Leaf[A] => 1
  //      case x: Branch[A] => go(1)(x.left, x.right)
  //    }
  //  }

  // EX 3.25
  def size2[T](t: Tree[T]): Int = t match {
    case Leaf(_) ⇒ 1
    case Branch(l, r) ⇒ 1 + size2(l) + size2(r)
  }

  // ex 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) ⇒ n
    case Branch(l, r) ⇒ maximum(l) max maximum(r)
  }

  // 3.27
  def depth[T](t: Tree[T]): Int = t match {
    case Leaf(_) ⇒ 0
    case Branch(l, r) ⇒
      1 + (depth(l) max depth(r))
  }

  // 3.28
  def map[T, R](t: Tree[T])(f: T ⇒ R): Tree[R] = t match {
    case Leaf(x) ⇒ Leaf(f(x))
    case Branch(l, r) ⇒ Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  def fold[T, R](t: Tree[T])(f: T ⇒ R)(g: (R, R) ⇒ R): R = t match {
    case Leaf(x) ⇒ f(x)
    case Branch(l, r) ⇒ g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[T](t: Tree[T]): Int =
    fold(t)(_ ⇒ 1)(1 + _ + _)

  def maxViaFold(t: Tree[Int]): Int =
    fold(t)(n ⇒ n)(_ max _)

  def depthViaFold[T](t: Tree[T]): Int =
    fold(t)(_ ⇒ 0)(1 + _ max _)

  def main(args: Array[String]): Unit = {
    val a = Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))
    val b = Branch(Leaf(1), Leaf(2))

    println(Tree.size2(a))
    println(Tree.maximum(a))
    println(Tree.depth(a))
    println(Tree.map(a)(x ⇒ x + 2))

    println(Tree.sizeViaFold(a))
    println(Tree.maxViaFold(a))
    println(Tree.depthViaFold(a))
  }
}
