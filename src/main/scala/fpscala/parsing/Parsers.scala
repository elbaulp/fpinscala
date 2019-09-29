// Copyright (C) 2018-2018  Alejandro Alcalde.
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package fpscala.parsing

import java.util.regex.Pattern

import fpscala.testing.Prop._
import fpscala.testing._

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self ⇒ // so inner classes may call methods of trait

  // recognizes and returns a single String
  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(
      implicit f: A ⇒ Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Chooses between two parsers, first attempting p1, and then p2 if p1 fails
  def or[A](p1: Parser[A], p2: ⇒ Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    //map2(p, succeed(n))((a, b) => List.fill(b)(a))
    if (n <= 0) {
      succeed(List.empty)
    } else {
      map2(p, listOfN(n - 1, p))(_ +: _)
    }

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def attempt[A](p: Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  /**
    * Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result.
    */
  def skipL[B](p: Parser[Any], p2: ⇒ Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) ⇒ b)

  /**
    * Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result.
    */
  def skipR[A](p: Parser[A], p2: ⇒ Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) ⇒ a)

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p, p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  // unit. Always succeeds with the value a
  def succeed[A](a: A): Parser[A] = string("") map (_ ⇒ a)

  // Primitives
  def flatMap[A, B](p: Parser[A])(f: A ⇒ Parser[B]): Parser[B]

  // Applies the function f to the result of p, if successful
  def map[A, B](p: Parser[A])(f: A ⇒ B): Parser[B] =
    flatMap(p)(a ⇒ succeed(f(a)))

  def map2[A, B, C](p: Parser[A], p2: ⇒ Parser[B])(f: (A, B) ⇒ C): Parser[C] =
    for { a ← p; b ← p2 } yield f(a, b)

  //    p.fatMap(a => map(p2)(f(a, _)))
  //flatMap(p)(a ⇒ map(p2)(f(a, _)))

  //    product(p, p2) map (f.tupled)
  // product(p, p2).map { case (a, b) ⇒ f(a, b) }
  //map(product(p, p2))(f.tupled)

  // Returns the portion of input inspected by p if successful
  def slice[A](p: Parser[A]): Parser[String]

  // Aditional parsing task
  // Parser[Int] that recognizes zero or more 'a' chars.
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ +: _) | succeed(List.empty)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ +: _)

  // Sequences two parsers, running p1 and then p2, and returns the pair of their results if both succeed
  def product[A, B](p: Parser[A], p2: ⇒ Parser[B]): Parser[(A, B)] =
    flatMap(p)(a ⇒ map(p2)((a, _)))

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Parser which consumes 1 or more digits. */
  def digits: Parser[String] = "\\d+".r

  /**
    * C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(
      p: ⇒ Parser[A]
  ): Parser[A] =
    start *> p <* stop

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
    // rather annoying to write, left as an exercise
    // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A ⇒ B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def flatMap[B](f: A ⇒ Parser[B]): Parser[B] = self.flatMap(p)(f)

    def token: Parser[A] = self.token(p)

    def *>[B](p2: ⇒ Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*(p2: ⇒ Parser[Any]): Parser[A] = self.skipR(p, p2)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def sep(separator: Parser[Any]): Parser[List[A]] = self.sep(p, separator)

    def sep1(separator: Parser[Any]): Parser[List[A]] = self.sep1(p, separator)

    def label(msg: String): Parser[A] = self.label(msg)(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s ⇒ run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s ⇒ p == p.map(identity))

    def charLaw[A](p: Parser[A])(in: Gen[Char]): Prop =
      forAll(in)(c ⇒ run(p)(c.toString) == Right(c))

    def stringLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s ⇒ run(p)(in.toString) == Right(s))

    def unitLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s ⇒ run(succeed(s))(s) == Right(s))

    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

    def productAssocLaw[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C])(
        in: Gen[String]
    ): Prop =
      forAll(in)(s ⇒ ((a ** b) ** c map unbiasL) == (a ** (b ** c) map unbiasR))
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col  = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(
    stack: List[(Location, String)] = List(),
    otherFailures: List[ParseError] = List()
) {}
