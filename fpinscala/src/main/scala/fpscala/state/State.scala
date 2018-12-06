package fpscala.state

import scala.annotation.{ switch, tailrec }

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // State action data type
  type Rand[+A] = RNG ⇒ (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng ⇒ (a, rng)

  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    rng ⇒ {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] = rng ⇒ {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)

    (f(a, b), r2)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    flatMap(s)(i ⇒ unit(f(i)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    flatMap(ra)(a ⇒ flatMap(rb)(b ⇒ unit(f(a, b))))

  def map2ViaFlatMapBook[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    flatMap(ra)(a ⇒ map(rb)(b ⇒ f(a, b)))

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Wrongapproach
  def nonNegativeInt_(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (i: @switch) match {
      case x if x == Int.MinValue ⇒ (0, rng2)
      case x if x < 0 ⇒ (-x, rng2)
      case _ ⇒ (i, rng2)
    }
  }

  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)

    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int)(acc: List[Int], r: RNG): (List[Int], RNG) = (c: @switch) match {
      case 0 ⇒ (acc, r)
      case x if x > 0 ⇒
        val (n, rn) = r.nextInt
        go(c - 1)(n +: acc, rn)
    }

    go(count)(Nil, rng)
  }

  def ints_(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(int))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.:\(unit(List.empty[A]))((r, l) ⇒ map2(r, l)(_ +: _))

  def flatMap[A, B](f: Rand[A])(g: A ⇒ Rand[B]): Rand[B] = rng ⇒ {
    val (a, r1) = f(rng)
    val (b, r2) = g(a)(r1)

    (b, r2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i ⇒
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  //  Book Solution
  //  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  //    rng => {
  //      val (a, r1) = f(rng)
  //      g(a)(r1) // We pass the new state along
  //    }

  def main(args: Array[String]): Unit = {

    val s1 = Simple(1)
    val s2 = Simple(2)

    println(s"IntDouble: ${intDouble(Simple(123))}")
    println(s"IntDouble: ${intDouble(Simple(124))}")

    println(s"DoubleInt: ${doubleInt(Simple(124))}")
    println(s"DoubleInt: ${doubleInt(Simple(123))}")

    println(s"Ints: ${ints(100)(Simple(123))}")
    val ints1 = ints(100)(Simple(123))
    val ints2 = ints(100)(Simple(123))
    println(s"Ints are equal for same seed: ${ints1.equals(ints2)}")

    println(s"int val ${int(s1)}")

    println(s"double ${double(s1)}")
    println(s"doubleviaMap ${doubleViaMap(s1)}")

    println(s"unit ${unit(2)(s1)}")
    println(s"unit ${unit(1 :: 2 :: Nil)(s1)}")
    println(s"map2 ${map2(int, int)(_ + _)(s1)}")
    println(s"map2 ${map2(int, int)(_ + _)(s1)}")
    println(s"map2 ${map2(int, int)(_ + _)(s2)}")

    println(s"Both ${both(int, int)(s1)}")
    println(s"Both ${both(int, int)(s1)}")
    println(s"Both ${both(int, int)(s2)}")

    println(s"IntDouble ${randIntDouble(s1)}")
    println(s"IntDouble ${randIntDouble(s1)}")
    println(s"IntDouble ${randIntDouble(s2)}")

    println(s"DoubleInt ${randDoubleInt(s2)}")
    println(s"DoubleInt ${randDoubleInt(s2)}")
    println(s"DoubleInt ${randDoubleInt(s1)}")

    println(s"Sequence ${sequence(int :: int :: int :: Nil)(s1)}")
    println(s"Sequence ${sequence(int :: int :: int :: Nil)(s1)}")
    println(s"Sequence ${sequence(int :: int :: int :: Nil)(s2)}")

    println(s"ints via Sequence ${ints_(10)(s1)}")
    println(s"ints via Sequence ${ints_(10)(s1)}")
    println(s"ints via Sequence ${ints_(10)(s2)}")

    println(s"FlatMap ${nonNegativeLessThan(10)(s1)}")
    println(s"FlatMap ${map(int)(_ * 2)(s1)}")
    println(s"FlatMap ${mapViaFlatMap(int)(_ * 2)(s1)}")

    println(s"map2 ${map2(int, int)(_ + _)(s2)}")
    println(s"map2viaFlatMap ${map2ViaFlatMap(int, int)(_ + _)(s2)}")
    println(s"map2viaFlatMapBook ${map2ViaFlatMapBook(int, int)(_ + _)(s2)}")

    val s = State[String, Int](s ⇒ (0, s))
    val units = State.unit[String, String]("S1")

    println(s"UNIT: ${units.run("UNIT")}, unit map ${units.map(x ⇒ x + x).run("STATE")}")
    println(s"State ${s.flatMap(i ⇒ State(s ⇒ (i + 1, s + s))).run("GO!")}")

    val sequences = State[String, Int](s ⇒ (0, s)) :: State[String, Int](s ⇒ (1, s)) :: Nil
    println(s"Sequence ${State.sequence(sequences).run("GO")}")
    println(s"SequenceR ${State.sequenceViaFoldRight(sequences).run("GO")}")
    println(s"SequenceL ${State.sequenceViaFoldLeft(sequences).run("GO")}")

    val machine = Machine(true, 5, 10)
    val usedMachine = Candy.simulateMachine(Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Nil)
    println(s"Final Machine ${usedMachine.run(machine)}")

  }
}

case class State[S, +A](run: S ⇒ (A, S)) {
  import State._
  def map[B](f: A ⇒ B): State[S, B] =
    flatMap(a ⇒ unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] =
    flatMap(a ⇒ sb.map(b ⇒ f(a, b)))

  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] =
    State(s ⇒ {
      val (a, ns) = run(s)
      f(a) run ns
    })
}

import State._

object State {
  type Rand[A] = State[RNG, A]

  def modify[S](f: S ⇒ S): State[S, Unit] = for {
    s ← get // Gets the current state and assigns it to `s`.
    _ ← set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s ⇒ (s, s))

  def set[S](s: S): State[S, Unit] = State(_ ⇒ ((), s))

  def unit[S, A](a: A): State[S, A] =
    State(s ⇒ (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    @tailrec
    def go(s: S, l: List[State[S, A]])(acc: List[A]): (List[A], S) = (l: @switch) match {
      case Nil ⇒ (acc.reverse, s)
      case h +: t ⇒ h run s match { case (a, ns) ⇒ go(ns, t)(a +: acc) }
    }

    State(s ⇒ go(s, fs)(List.empty[A]))
  }

  def sequenceViaFoldRight[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.:\(unit[S, List[A]](List.empty[A]))((f, z) ⇒ f.map2(z)(_ +: _))

  def sequenceViaFoldLeft[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse./:(unit[S, List[A]](List.empty[A]))((z, f) ⇒ f.map2(z)(_ +: _))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) ⇒ (s: Machine) ⇒
    (i, s) match {
      case (_, Machine(_, 0, _)) ⇒ s
      case (Coin, Machine(false, _, _)) ⇒ s
      case (Turn, Machine(true, _, _)) ⇒ s
      case (Coin, Machine(true, candy, coin)) ⇒
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) ⇒
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ ← sequence(inputs map (modify[Machine] _ compose update))
    s ← get
  } yield (s.coins, s.candies)

}
