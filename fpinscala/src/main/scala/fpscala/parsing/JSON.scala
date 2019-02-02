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

package fpscala.parsing

import language.higherKinds
import language.implicitConversions

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{ string ⇒ _, _ }
    ???
    //    implicit def tok(s: String): Parser[String] = token(P.string(s))
    //
    //    def array = surround("[", "]")(
    //      value sep "," map (vs ⇒ JArray(vs.toIndexedSeq))) scope "array"
    //    def obj = surround("{", "}")(
    //      keyval sep "," map (kvs ⇒ JObject(kvs.toMap))) scope "object"
    //    def keyval = escapedQuoted ** (":" *> value)
    //    def lit = scope("literal") {
    //      "null".as(JNull) |
    //        double.map(JNumber(_)) |
    //        escapedQuoted.map(JString(_)) |
    //        "true".as(JBool(true)) |
    //        "false".as(JBool(false))
    //    }
    //    def value: Parser[JSON] = lit | obj | array
    //    root(whitespace *> (obj | array))
    //  }
  }
}

///**
//  * JSON parsing example.
//  */
//object JSONExample extends App {
//  val jsonTxt = """
//{
//  "Company name" : "Microsoft Corporation",
//  "Ticker"  : "MSFT",
//  "Active"  : true,
//  "Price"   : 30.66,
//  "Shares outstanding" : 8.38e9,
//  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
//}
//"""
//
//  val malformedJson1 = """
//{
//  "Company name" ; "Microsoft Corporation"
//}
//"""
//
//  val malformedJson2 = """
//[
//  [ "HPQ", "IBM",
//  "YHOO", "DELL" ++
//  "GOOG"
//  ]
//]
//"""
//
//  val P = fpscala.parsing.Reference
//  import fpscala.parsing.ReferenceTypes.Parser
//
//  def printResult[E](e: Either[E,JSON]) =
//    e.fold(println, println)
//
//  val json: Parser[JSON] = JSON.jsonParser(P)
//  printResult { P.run(json)(jsonTxt) }
//  println("--")
//  printResult { P.run(json)(malformedJson1) }
//  println("--")
//  printResult { P.run(json)(malformedJson2) }
//}
