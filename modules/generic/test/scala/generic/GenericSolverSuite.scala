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

package com.filippodeluca.jsonpath.generic

import cats.syntax.all.*

import org.scalacheck.Prop.*

import com.filippodeluca.jsonpath.ast.*

class GenericSolverSuite extends munit.ScalaCheckSuite {

  import GenericSolver.*

  def testSolve(exp: Exp, source: Any, expecteds: Any*)(implicit loc: munit.Location) = {
    test(show"""solve ${exp} on ${source.noSpaces} should return ${expecteds
        .map(_.noSpaces)
        .mkString("[", ",", "]")}""") {
      assertEquals(solve(exp, source), expecteds.toVector)
    }
  }

  testSolve(
    StringLiteral("foo"),
    Map(
      "foo" -> "bar"
    ),
    "foo"
  )

  testSolve(
    BooleanLiteral(true),
    Map(
      "foo" -> "bar"
    ),
    true
  )

  testSolve(
    BooleanLiteral(false),
    Map(
      "foo" -> "bar"
    ),
    false
  )

  testSolve(
    NumberLiteral(10),
    Map(
      "foo" -> "bar"
    ),
    10
  )

  testSolve(
    This,
    Map(
      "foo" -> "bar"
    ),
    Map(
      "foo" -> "bar"
    )
  )

  testSolve(
    Root,
    Map(
      "foo" -> "bar"
    ),
    Map(
      "foo" -> "bar"
    )
  )

  testSolve(
    Property(StringLiteral("foo"), Root),
    Map(
      "bar" -> "value"
    )
  )

  testSolve(
    Property(StringLiteral("bar"), Property(StringLiteral("foo"), Root)),
    Map(
      "foo" ->
        Map(
          "bar" -> "value"
        )
    ),
    "value"
  )

  testSolve(
    Property(StringLiteral("bar"), Property(StringLiteral("foo"), Root)),
    Map(
      "foo" ->
        Map(
          "baz" -> "value"
        )
    )
  )

  testSolve(
    ArrayIndex(NumberLiteral(1), This),
    List(
      0,
      1,
      2
    ),
    1
  )

  testSolve(
    ArrayIndex(NumberLiteral(-1), This),
    List(
      0,
      1,
      2
    ),
    2
  )

  testSolve(
    ArrayIndex(NumberLiteral(-2), This),
    List(
      0,
      1,
      2
    ),
    1
  )

  testSolve(
    ArrayIndex(NumberLiteral(-4), This),
    List(
      0,
      1,
      2
    )
  )

  testSolve(
    ArrayIndex(NumberLiteral(3), This),
    List(
      0,
      1
    )
  )

  testSolve(
    ArrayIndex(NumberLiteral(1), Property(StringLiteral("foo"), This)),
    Map(
      "foo" -> List(
        1,
        2,
        3
      )
    ),
    2
  )

  testSolve(
    ArrayIndex(
      NumberLiteral(1),
      Property(StringLiteral("bar"), Property(StringLiteral("foo"), This))
    ),
    Map(
      "foo" -> Map(
        "bar" -> List(
          1,
          2,
          3
        )
      )
    ),
    2
  )

  testSolve(
    Property(
      StringLiteral("bar"),
      ArrayIndex(NumberLiteral(0), Property(StringLiteral("foo"), This))
    ),
    Map(
      "foo" -> List(
        Map("bar" -> 1),
        Map("bar" -> 2),
        Map("bar" -> 3)
      )
    ),
    1
  )

  testSolve(
    Property(
      StringLiteral("bar"),
      ArrayIndex(NumberLiteral(0), Property(StringLiteral("foo"), This))
    ),
    Map(
      "foo" -> List(
        Map("bar" -> 1),
        Map("bar" -> 2),
        Map("bar" -> 3)
      )
    ),
    1
  )

  // Wildcard ***

  testSolve(
    Wildcard(Root),
    List(
      1,
      2,
      3
    ),
    1,
    2,
    3
  )

  testSolve(
    Wildcard(Root),
    List(
      Map("a" -> 1),
      Map("b" -> 2),
      Map("c" -> 3)
    ),
    Map("a" -> 1),
    Map("b" -> 2),
    Map("c" -> 3)
  )

  testSolve(
    Wildcard(Root),
    Map(
      "foo" -> "fooValue",
      "bar" -> "barValue",
      "baz" -> "bazValue"
    ),
    "fooValue",
    "barValue",
    "bazValue"
  )

  testSolve(
    Wildcard(Root),
    Map(
      "foo" -> Map("a" -> 1),
      "bar" -> Map("b" -> 2),
      "baz" -> Map("c" -> 3)
    ),
    Map("a" -> 1),
    Map("b" -> 2),
    Map("c" -> 3)
  )

  // ArrayIndex ***

  testSolve(
    ArrayIndex(
      NumberLiteral(1),
      Property(Property(StringLiteral("arrName"), This), Property(StringLiteral("foo"), This))
    ),
    Map(
      "foo" -> Map(
        "arrName" -> "bar",
        "bar" -> List(
          1,
          2,
          3
        )
      )
    ),
    2
  )

  testSolve(
    ArrayIndex(
      Property(StringLiteral("barIndex"), Property(StringLiteral("foo"), Root)),
      Property(StringLiteral("bar"), Property(StringLiteral("foo"), Root))
    ),
    Map(
      "foo" -> Map(
        "barIndex" -> 2,
        "bar" -> List(
          1,
          2,
          3
        )
      )
    ),
    3
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Map(
      "foo" -> Map(
        "enabled" -> false,
        "bar" -> "Baz"
      )
    )
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Map(
      "foo" -> Map(
        "enabled" -> true,
        "bar" -> "Baz"
      )
    ),
    Map(
      "enabled" -> true,
      "bar" -> "Baz"
    )
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Map(
      "foo" -> Map(
        "enabled" -> false,
        "bar" -> "Baz"
      )
    )
  )

  testSolve(
    Filter(
      Eq(Property(StringLiteral("enabled"), This), BooleanLiteral(true)),
      Property(StringLiteral("foo"), Root)
    ),
    Map(
      "foo" -> Map(
        "enabled" -> false,
        "bar" -> "Baz"
      )
    )
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Map(
      "foo" -> List(
        Map(
          "enabled" -> false,
          "bar" -> "Baz"
        ),
        Map(
          "enabled" -> true,
          "bar" -> "Bazzinga"
        )
      )
    ),
    Map(
      "enabled" -> true,
      "bar" -> "Bazzinga"
    )
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Map(
      "foo" -> List(
        Map(
          "enabled" -> false,
          "bar" -> "Baz"
        ),
        Map(
          "enabled" -> true,
          "bar" -> "Bazzinga"
        )
      )
    ),
    Map(
      "enabled" -> true,
      "bar" -> "Bazzinga"
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(1), NullLiteral, NullLiteral, Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      1, 2, 3, 4, 5
    )
  )

  testSolve(
    ArraySlice(NullLiteral, NumberLiteral(3), NullLiteral, Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      0,
      1,
      2
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(1), NumberLiteral(3), NullLiteral, Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      1,
      2
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(1), NumberLiteral(4), NumberLiteral(2), Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      1,
      3
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(-1), NullLiteral, NullLiteral, Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      5
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(-3), NumberLiteral(-1), NullLiteral, Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      3,
      4
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(3), NullLiteral, NumberLiteral(-1), Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      3,
      2,
      1,
      0
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(-2), NullLiteral, NumberLiteral(-1), Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      4, 3, 2, 1, 0
    )
  )

  testSolve(
    ArraySlice(NullLiteral, NumberLiteral(-4), NumberLiteral(-1), Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      5,
      4,
      3
    )
  )

  testSolve(
    ArraySlice(NullLiteral, NumberLiteral(3), NumberLiteral(-1), Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      5,
      4
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(4), NumberLiteral(3), NumberLiteral(-1), Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      4
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(-2), NumberLiteral(-4), NumberLiteral(-1), Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      4,
      3
    )
  )

  testSolve(
    ArraySlice(NullLiteral, NullLiteral, NumberLiteral(-2), Root),
    List(
      0, 1, 2, 3, 4, 5
    ),
    List(
      4,
      2,
      0
    )
  )

  // Eq ***

  testSolve(
    Eq(StringLiteral("foo"), StringLiteral("foo")),
    None,
    true
  )

  testSolve(
    Eq(NumberLiteral(3), NumberLiteral(3)),
    None,
    true
  )

  testSolve(
    Eq(NumberLiteral(3), NumberLiteral(5)),
    None,
    false
  )

  testSolve(
    Eq(StringLiteral("3"), NumberLiteral(3)),
    None,
    false
  )

  testSolve(
    Eq(This, NumberLiteral(3)),
    3,
    true
  )

  testSolve(
    Eq(This, StringLiteral("foo")),
    "foo",
    true
  )

  testSolve(
    Eq(This, StringLiteral("bar")),
    "foo",
    false
  )

  testSolve(
    Eq(Property(StringLiteral("foo"), Root), Property(StringLiteral("bar"), Root)),
    Map("foo" -> 8),
    "bar" -> 9,
    false
  )

  testSolve(
    Eq(Property(StringLiteral("foo"), Root), Property(StringLiteral("bar"), Root)),
    Map("foo" -> 8),
    "bar" -> 8,
    true
  )

  // Gt ***

  testSolve(
    Gt(NumberLiteral(9), NumberLiteral(8)),
    None,
    true
  )

  testSolve(
    Gt(NumberLiteral(9), NumberLiteral(9)),
    None,
    false
  )

  testSolve(
    Gt(NumberLiteral(8), NumberLiteral(9)),
    None,
    false
  )

  testSolve(
    Gt(StringLiteral("9"), NumberLiteral(8)),
    None,
    false
  )

  testSolve(
    Gt(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Map("left" -> 9),
    "right" -> 8,
    true
  )

  testSolve(
    Gt(
      NumberLiteral(9),
      Property(StringLiteral("right"), Root)
    ),
    Map("right" -> 8),
    true
  )

  // Lt ***

  testSolve(
    Lt(NumberLiteral(9), NumberLiteral(8)),
    None,
    false
  )

  testSolve(
    Lt(NumberLiteral(9), NumberLiteral(9)),
    None,
    false
  )

  testSolve(
    Lt(NumberLiteral(8), NumberLiteral(9)),
    None,
    true
  )

  testSolve(
    Lt(StringLiteral("9"), NumberLiteral(8)),
    None,
    false
  )

  testSolve(
    Lt(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Map("left" -> 8),
    "right" -> 9,
    true
  )

  // ** Gte

  testSolve(
    Gte(NumberLiteral(9), NumberLiteral(8)),
    None,
    true
  )

  testSolve(
    Gte(NumberLiteral(9), NumberLiteral(9)),
    None,
    true
  )

  testSolve(
    Gte(NumberLiteral(8), NumberLiteral(9)),
    None,
    false
  )

  testSolve(
    Gte(StringLiteral("9"), NumberLiteral(8)),
    None,
    false
  )

  testSolve(
    Gte(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Map("left" -> 8),
    "right" -> 8,
    true
  )

  testSolve(
    Gte(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Map("left" -> 9),
    "right" -> 8,
    true
  )

  // Lte ***

  testSolve(
    Lte(NumberLiteral(8), NumberLiteral(9)),
    None,
    true
  )

  testSolve(
    Lte(NumberLiteral(9), NumberLiteral(9)),
    None,
    true
  )

  testSolve(
    Lte(NumberLiteral(9), NumberLiteral(8)),
    None,
    false
  )

  testSolve(
    Lte(StringLiteral("8"), NumberLiteral(8)),
    None,
    false
  )

  testSolve(
    Lte(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Map("left" -> 8),
    "right" -> 8,
    true
  )

  testSolve(
    Lte(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Map("left" -> 8),
    "right" -> 9,
    true
  )

  // Not ***

  testSolve(
    Not(BooleanLiteral(true)),
    None,
    false
  )

  testSolve(
    Not(Root),
    false,
    true
  )

  testSolve(
    Not(This),
    true,
    false
  )

  // Or ***

  testSolve(
    Or(BooleanLiteral(false), BooleanLiteral(false)),
    None,
    false
  )

  testSolve(
    Or(BooleanLiteral(true), BooleanLiteral(false)),
    None,
    true
  )

  testSolve(
    Or(BooleanLiteral(false), BooleanLiteral(true)),
    None,
    true
  )

  testSolve(
    Or(BooleanLiteral(true), BooleanLiteral(true)),
    None,
    true
  )

  // And ***

  testSolve(
    And(BooleanLiteral(false), BooleanLiteral(false)),
    None,
    false
  )

  testSolve(
    And(BooleanLiteral(true), BooleanLiteral(false)),
    None,
    false
  )

  testSolve(
    And(BooleanLiteral(false), BooleanLiteral(true)),
    None,
    false
  )

  testSolve(
    And(BooleanLiteral(true), BooleanLiteral(true)),
    None,
    true
  )

  // In ***

  testSolve(
    In(StringLiteral("bar"), Root),
    List("foo"),
    "bar",
    "baz",
    true
  )

  testSolve(
    In(StringLiteral("foo"), Root),
    Map("foo" -> "bar"),
    true
  )

  testSolve(
    In(StringLiteral("foo"), Root),
    List("bar"),
    "baz",
    false
  )

  testSolve(
    In(StringLiteral("foo"), Root),
    Map("bar" -> "baz"),
    false
  )

  // Plus ***

  testSolve(
    Plus(NumberLiteral(1), Root),
    2,
    3
  )

  testSolve(
    Plus(NumberLiteral(1), Root),
    -2,
    -1
  )

  testSolve(
    Minus(NumberLiteral(5), Root),
    2,
    3
  )

  testSolve(
    Minus(NumberLiteral(5), Root),
    -2,
    7
  )

  testSolve(
    Minus(Minus(NumberLiteral(5), NumberLiteral(1)), Root),
    -2,
    6
  )

  testSolve(
    Times(NumberLiteral(10), Root),
    2,
    20
  )

  testSolve(
    Times(NumberLiteral(10), Root),
    -1,
    -10
  )

  testSolve(
    DividedBy(NumberLiteral(5), Root),
    -2,
    BigDecimal(-2.5f)
  )

  testSolve(
    Modulo(NumberLiteral(5), Root),
    -2,
    1
  )

  testSolve(
    Modulo(NumberLiteral(5), Root),
    5,
    0
  )

  testSolve(
    Modulo(NumberLiteral(5), Root),
    1,
    0
  )

  testSolve(
    Modulo(NumberLiteral(6), Root),
    3,
    0
  )

  // Union ***

  testSolve(
    Union(Vector(Root)),
    8,
    8
  )

  testSolve(
    Union(Vector(Root, Root)),
    8,
    8,
    8
  )

  testSolve(
    Union(Vector(Property(StringLiteral("foo"), Root), Property(StringLiteral("bar"), This))),
    Map(
      "foo" -> 1,
      "bar" -> 2
    ),
    1,
    2
  )

  testSolve(
    Union(
      Vector(
        Property(StringLiteral("foo"), Root),
        Union(
          Vector(
            Property(StringLiteral("bar"), This),
            Property(StringLiteral("baz"), This)
          )
        )
      )
    ),
    Map(
      "foo" -> 1,
      "bar" -> 2,
      "baz" -> 3
    ),
    1,
    2,
    3
  )
}
