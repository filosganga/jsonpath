/*
 * Copyright 2023 Filippo De Luca
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.filippodeluca.jsonpath
package parser

import munit.Location

import com.filippodeluca.jsonpath.ast.*

class JsonPathParserSuite extends munit.FunSuite {

  def shouldParse(exp: String, expected: Exp)(implicit loc: Location) = {
    test(s"should parse $exp") {
      val result = JsonPathParser.parse(exp)
      assertEquals(result, Right(expected))
    }
  }

  def shouldNotParse(exp: String)(implicit loc: Location) = {
    test(s"should not parse $exp") {
      val result = JsonPathParser.parse(exp)
      assert(result.isLeft, s"Expected failure, got $result")
    }
  }

  shouldParse(
    "$",
    Root
  )

  shouldParse(
    "($)",
    Root
  )

  shouldParse(
    "@",
    This
  )

  shouldParse(
    "true",
    b(true)
  )

  shouldParse(
    "54.67",
    n(54.67)
  )

  shouldParse(
    "'123'",
    s("123")
  )

  shouldParse(
    "$.test",
    Property(s("test"), Root)
  )

  shouldParse(
    "$['$test']",
    Property(s("$test"), Root)
  )

  shouldParse(
    "$['@test']",
    Property(s("@test"), Root)
  )

  shouldParse(
    "$.*",
    Wildcard(Root)
  )

  shouldParse(
    "($.*)",
    Wildcard(Root)
  )

  shouldParse(
    "$[*]",
    Wildcard(Root)
  )

  shouldParse(
    "@.foo.bar.baz",
    Property(s("baz"), Property(s("bar"), thisProp("foo")))
  )

  shouldParse(
    "@.foo['bar'].baz",
    Property(s("baz"), Property(s("bar"), thisProp("foo")))
  )

  shouldParse(
    """@.foo["bar"].baz""",
    Property(s("baz"), Property(s("bar"), thisProp("foo")))
  )

  shouldParse(
    "@.foo[('bar')].baz",
    Property(s("baz"), Property(s("bar"), thisProp("foo")))
  )

  shouldParse(
    "@.foo[(@.bar)].baz",
    Property(s("baz"), Property(thisProp("bar"), thisProp("foo")))
  )

  shouldParse(
    "! true",
    Not(b(true))
  )

  shouldParse(
    "!@.foo",
    Not(thisProp("foo"))
  )

  shouldParse(
    "!(@.foo)",
    Not(thisProp("foo"))
  )

  shouldParse(
    "true && false",
    And(b(true), b(false))
  )

  shouldParse(
    "true||false",
    Or(b(true), b(false))
  )

  shouldParse(
    "1 > 2.5",
    Gt(n(1), n(2.5))
  )

  shouldParse(
    "1.9 >=1.9",
    Gte(n(1.9), n(1.9))
  )

  shouldParse(
    "1.93 <=1",
    Lte(n(1.93), n(1))
  )

  shouldParse(
    "1 < 100",
    Lt(n(1), n(100))
  )

  shouldParse(
    "0.23 == 0.23",
    Eq(n(0.23), n(0.23))
  )

  shouldParse(
    "0.23 != 0.23",
    Not(Eq(n(0.23), n(0.23)))
  )

  shouldParse(
    "$[1]",
    ArrayIndex(n(1), Root)
  )

  shouldParse(
    "$[-1]",
    ArrayIndex(n(-1), Root)
  )

  shouldParse(
    "$[1:1:1]",
    ArraySlice(n(1), n(1), n(1), Root)
  )

  shouldParse(
    "$[:]",
    ArraySlice(NullLiteral, NullLiteral, NullLiteral, Root)
  )

  shouldParse(
    "$[::]",
    ArraySlice(NullLiteral, NullLiteral, NullLiteral, Root)
  )

  shouldParse(
    "$[:3]",
    ArraySlice(NullLiteral, n(3), NullLiteral, Root)
  )

  shouldParse(
    "$[1:3]",
    ArraySlice(n(1), n(3), NullLiteral, Root)
  )

  shouldParse(
    "$[::1]",
    ArraySlice(NullLiteral, NullLiteral, n(1), Root)
  )

  shouldParse(
    "$[?(@.this == @.that)]",
    Filter(Eq(thisProp("this"), thisProp("that")), Root)
  )

  shouldParse(
    "$[?(@.big > @.small || @.first == @.second && @.first != @.third)]",
    Filter(
      Or(
        Gt(thisProp("big"), thisProp("small")),
        And(
          Eq(thisProp("first"), thisProp("second")),
          Not(Eq(thisProp("first"), thisProp("third")))
        )
      ),
      Root
    )
  )

  shouldParse(
    "1 + 2",
    Plus(n(1), n(2))
  )

  shouldParse(
    "-1 + 2",
    Plus(n(-1), n(2))
  )

  shouldParse(
    "-1 + -2",
    Plus(n(-1), n(-2))
  )

  shouldParse(
    "1 - 2",
    Minus(n(1), n(2))
  )

  shouldParse(
    "-1 - 2",
    Minus(n(-1), n(2))
  )

  shouldParse(
    "-1 - -2",
    Minus(n(-1), n(-2))
  )

  shouldParse(
    "1 * 2",
    Times(n(1), n(2))
  )

  shouldParse(
    "-1 * 2",
    Times(n(-1), n(2))
  )

  shouldParse(
    "-1 * -2",
    Times(n(-1), n(-2))
  )

  shouldParse(
    "1 / 2",
    DividedBy(n(1), n(2))
  )

  shouldParse(
    "-1 / 2",
    DividedBy(n(-1), n(2))
  )

  shouldParse(
    "-1 / -2",
    DividedBy(n(-1), n(-2))
  )

  shouldParse(
    "1 % 2",
    Modulo(n(1), n(2))
  )

  shouldParse(
    "-1 % 2",
    Modulo(n(-1), n(2))
  )

  shouldParse(
    "-1 % -2",
    Modulo(n(-1), n(-2))
  )

  shouldParse(
    "1 + 2 * 3",
    Plus(n(1), Times(n(2), n(3)))
  )

  shouldParse(
    "1 * 2 + 3",
    Plus(Times(n(1), n(2)), n(3))
  )

  shouldParse(
    "1 - 2 / 3",
    Minus(n(1), DividedBy(n(2), n(3)))
  )

  shouldParse(
    "1 + 2 % 3",
    Plus(n(1), Modulo(n(2), n(3)))
  )

  shouldParse(
    "1 - 2 - 3",
    Minus(Minus(n(1), n(2)), n(3))
  )

  shouldParse(
    "1 - 2 + 3",
    Plus(Minus(n(1), n(2)), n(3))
  )

  shouldParse(
    "8 / 4 / 2",
    DividedBy(DividedBy(n(8), n(4)), n(2))
  )

  shouldParse(
    "6 % 4 * 2",
    Times(Modulo(n(6), n(4)), n(2))
  )

  shouldParse(
    "1-2-3",
    Minus(Minus(n(1), n(2)), n(3))
  )

  shouldParse(
    "(1 + 2) * 3",
    Times(Plus(n(1), n(2)), n(3))
  )

  shouldParse(
    "( 1 + 2 ) * 3",
    Times(Plus(n(1), n(2)), n(3))
  )

  shouldParse(
    "1 - (2 - 3)",
    Minus(n(1), Minus(n(2), n(3)))
  )

  shouldParse(
    "1 + 2 > 3 * 4",
    Gt(Plus(n(1), n(2)), Times(n(3), n(4)))
  )

  shouldParse(
    "@.a - 1 == @.b",
    Eq(Minus(thisProp("a"), n(1)), thisProp("b"))
  )

  shouldParse(
    "1 <= 2 + 3",
    Lte(n(1), Plus(n(2), n(3)))
  )

  shouldNotParse("1 < 2 == true")

  shouldNotParse("1 == 1 == true")

  shouldParse(
    "(1 < 2) == true",
    Eq(Lt(n(1), n(2)), b(true))
  )

  shouldParse(
    "1 == 1 && 2 != 3",
    And(Eq(n(1), n(1)), Not(Eq(n(2), n(3))))
  )

  shouldParse(
    "1 <= 2 || 3 >= 4",
    Or(Lte(n(1), n(2)), Gte(n(3), n(4)))
  )

  shouldParse(
    "true || false && true",
    Or(b(true), And(b(false), b(true)))
  )

  shouldParse(
    "true && false || true",
    Or(And(b(true), b(false)), b(true))
  )

  shouldParse(
    "true || false || true",
    Or(Or(b(true), b(false)), b(true))
  )

  shouldParse(
    "true && false && true",
    And(And(b(true), b(false)), b(true))
  )

  shouldParse(
    "(true || false) && true",
    And(Or(b(true), b(false)), b(true))
  )

  shouldParse(
    "!true && false",
    And(Not(b(true)), b(false))
  )

  shouldParse(
    "!(true && false)",
    Not(And(b(true), b(false)))
  )

  shouldParse(
    "!!true",
    Not(Not(b(true)))
  )

  shouldParse(
    "!@.a == true",
    Eq(Not(thisProp("a")), b(true))
  )

  shouldParse(
    "1 + 2 * 3 > 4 && @.a == 'x' || !@.b",
    Or(
      And(Gt(Plus(n(1), Times(n(2), n(3))), n(4)), Eq(thisProp("a"), s("x"))),
      Not(thisProp("b"))
    )
  )

  shouldParse(
    "$[?(@.price * @.qty > 100 && @.inStock)]",
    Filter(And(Gt(Times(thisProp("price"), thisProp("qty")), n(100)), thisProp("inStock")), Root)
  )

  def n(value: Double) = NumberLiteral(value)
  def b(value: Boolean) = BooleanLiteral(value)
  def s(value: String) = StringLiteral(value)
  def thisProp(name: String) = Property(s(name), This)
}
