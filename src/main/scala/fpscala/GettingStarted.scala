package fpscala

import scala.annotation.tailrec

object GettingStarted extends App {

  /*************************************************************************************
    * Exercise 2.2.                                                                   *
    *  Implement isSorted, which checks whether an Array[A] is sorted according to a *
    *  given comparison function:                                                        *
    *  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean                 *
 *************************************************************************************/
  def isSorted[A](as: Array[A], ordered: (A, A) ⇒ Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean =
      if (n >= as.size - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else go(n + 1)
    go(0)
  }

  val array = Array(1, 2, 3, 4, 5)
  val ord   = (a: Int, b: Int) ⇒ if (a > b) true else false
  print(s"Array ${array.foreach(print)} is ordered?: ${isSorted(array, ord)}")

  /*****************************************************************************
    * HoF  called Partial Application that can only be implemented in one way.  *
    *   Takes a value and a function of two args, return a function of one arg. *
   *****************************************************************************/
  def partial1[A, B, C](a: A, f: (A, B) ⇒ C): B ⇒ C =
    // (b: B) => ??? We meed to return a function B => C.
    // As the required return type for the function is C,
    // and the only way you can obtain C is applying f,
    // RHS of the function must be f(a,b)
    //(b: B) => f(a, b)
    // as b is in scope:
    b ⇒ f(a, b)

  /******************************************************************************************
    * EXERCISE 2.3                                                                           *
    * Let’s look at another example, currying,9 which converts a function f of two arguments *
    * into a function of one argument that partially applies f. Here again there’s only one  *
    * implementation that compiles. Write this implementation.                               *
   ******************************************************************************************/
  def curry[A, B, C](f: (A, B) ⇒ C): A ⇒ (B ⇒ C) =
    a ⇒ b ⇒ f(a, b)

  /*************************************************************************************
    * EXERCISE 2.4                                                                      *
    * Implement uncurry, which reverses the transformation of curry. Note that since => *
    * associates to the right, A => (B => C) can be written as A => B => C.             *
   *************************************************************************************/
  def uncurry[A, B, C](f: A ⇒ B ⇒ C): (A, B) ⇒ C =
    (a, b) ⇒ f(a)(b)

  /********************************************************************
    * EXERCISE 2.5                                                     *
    * Implement the higher-order function that composes two functions. *
   ********************************************************************/
  def compose[A, B, C](f: B ⇒ C, g: A ⇒ B): A ⇒ C =
    a ⇒ f(g(a))
}
