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
package circe

import cats.syntax.all.*

import io.circe.Json
import io.circe.literal.*
import io.circe.testing.instances.*

import org.scalacheck.Prop.*

import com.filippodeluca.jsonpath.ast.*

class CirceSolverSuite extends munit.ScalaCheckSuite {

  import CirceSolver.*

  def testSolve(exp: Exp, source: Json, expecteds: Json*)(implicit loc: munit.Location) = {
    test(show"""solve ${exp} on ${source.noSpaces} should return ${expecteds
        .map(_.noSpaces)
        .mkString("[", ",", "]")}""") {
      assertEquals(solve(exp, source), expecteds.toVector)
    }
  }

  testSolve(
    StringLiteral("foo"),
    Json.obj(
      "foo" -> Json.fromString("bar")
    ),
    Json.fromString("foo")
  )

  testSolve(
    BooleanLiteral(true),
    Json.obj(
      "foo" -> Json.fromString("bar")
    ),
    Json.fromBoolean(true)
  )

  testSolve(
    BooleanLiteral(false),
    Json.obj(
      "foo" -> Json.fromString("bar")
    ),
    Json.fromBoolean(false)
  )

  testSolve(
    NumberLiteral(10),
    Json.obj(
      "foo" -> Json.fromString("bar")
    ),
    Json.fromInt(10)
  )

  testSolve(
    This,
    Json.obj(
      "foo" -> Json.fromString("bar")
    ),
    Json.obj(
      "foo" -> Json.fromString("bar")
    )
  )

  testSolve(
    Root,
    Json.obj(
      "foo" -> Json.fromString("bar")
    ),
    Json.obj(
      "foo" -> Json.fromString("bar")
    )
  )

  testSolve(
    Property(StringLiteral("foo"), Root),
    Json.obj(
      "bar" -> Json.fromString("value")
    )
  )

  testSolve(
    Property(StringLiteral("bar"), Property(StringLiteral("foo"), Root)),
    Json.obj(
      "foo" ->
        Json.obj(
          "bar" -> Json.fromString("value")
        )
    ),
    Json.fromString("value")
  )

  testSolve(
    Property(StringLiteral("bar"), Property(StringLiteral("foo"), Root)),
    Json.obj(
      "foo" ->
        Json.obj(
          "baz" -> Json.fromString("value")
        )
    )
  )

  testSolve(
    ArrayIndex(NumberLiteral(1), This),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2)
    ),
    Json.fromInt(1)
  )

  testSolve(
    ArrayIndex(NumberLiteral(-1), This),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2)
    ),
    Json.fromInt(2)
  )

  testSolve(
    ArrayIndex(NumberLiteral(-2), This),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2)
    ),
    Json.fromInt(1)
  )

  testSolve(
    ArrayIndex(NumberLiteral(-4), This),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2)
    )
  )

  testSolve(
    ArrayIndex(NumberLiteral(3), This),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1)
    )
  )

  testSolve(
    ArrayIndex(NumberLiteral(1), Property(StringLiteral("foo"), This)),
    Json.obj(
      "foo" -> Json.arr(
        Json.fromInt(1),
        Json.fromInt(2),
        Json.fromInt(3)
      )
    ),
    Json.fromInt(2)
  )

  testSolve(
    ArrayIndex(
      NumberLiteral(1),
      Property(StringLiteral("bar"), Property(StringLiteral("foo"), This))
    ),
    Json.obj(
      "foo" -> Json.obj(
        "bar" -> Json.arr(
          Json.fromInt(1),
          Json.fromInt(2),
          Json.fromInt(3)
        )
      )
    ),
    Json.fromInt(2)
  )

  testSolve(
    Property(
      StringLiteral("bar"),
      ArrayIndex(NumberLiteral(0), Property(StringLiteral("foo"), This))
    ),
    Json.obj(
      "foo" -> Json.arr(
        Json.obj("bar" -> Json.fromInt(1)),
        Json.obj("bar" -> Json.fromInt(2)),
        Json.obj("bar" -> Json.fromInt(3))
      )
    ),
    Json.fromInt(1)
  )

  testSolve(
    Property(
      StringLiteral("bar"),
      ArrayIndex(NumberLiteral(0), Property(StringLiteral("foo"), This))
    ),
    Json.obj(
      "foo" -> Json.arr(
        Json.obj("bar" -> Json.fromInt(1)),
        Json.obj("bar" -> Json.fromInt(2)),
        Json.obj("bar" -> Json.fromInt(3))
      )
    ),
    Json.fromInt(1)
  )

  // Wildcard ***

  testSolve(
    Wildcard(Root),
    Json.arr(
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3)
    ),
    Json.fromInt(1),
    Json.fromInt(2),
    Json.fromInt(3)
  )

  testSolve(
    Wildcard(Root),
    Json.arr(
      Json.obj("a" -> Json.fromInt(1)),
      Json.obj("b" -> Json.fromInt(2)),
      Json.obj("c" -> Json.fromInt(3))
    ),
    Json.obj("a" -> Json.fromInt(1)),
    Json.obj("b" -> Json.fromInt(2)),
    Json.obj("c" -> Json.fromInt(3))
  )

  testSolve(
    Wildcard(Root),
    Json.obj(
      "foo" -> Json.fromString("fooValue"),
      "bar" -> Json.fromString("barValue"),
      "baz" -> Json.fromString("bazValue")
    ),
    Json.fromString("fooValue"),
    Json.fromString("barValue"),
    Json.fromString("bazValue")
  )

  testSolve(
    Wildcard(Root),
    Json.obj(
      "foo" -> Json.obj("a" -> Json.fromInt(1)),
      "bar" -> Json.obj("b" -> Json.fromInt(2)),
      "baz" -> Json.obj("c" -> Json.fromInt(3))
    ),
    Json.obj("a" -> Json.fromInt(1)),
    Json.obj("b" -> Json.fromInt(2)),
    Json.obj("c" -> Json.fromInt(3))
  )

  // ArrayIndex ***

  testSolve(
    ArrayIndex(
      NumberLiteral(1),
      Property(Property(StringLiteral("arrName"), This), Property(StringLiteral("foo"), This))
    ),
    Json.obj(
      "foo" -> Json.obj(
        "arrName" -> Json.fromString("bar"),
        "bar" -> Json.arr(
          Json.fromInt(1),
          Json.fromInt(2),
          Json.fromInt(3)
        )
      )
    ),
    Json.fromInt(2)
  )

  testSolve(
    ArrayIndex(
      Property(StringLiteral("barIndex"), Property(StringLiteral("foo"), Root)),
      Property(StringLiteral("bar"), Property(StringLiteral("foo"), Root))
    ),
    Json.obj(
      "foo" -> Json.obj(
        "barIndex" -> Json.fromInt(2),
        "bar" -> Json.arr(
          Json.fromInt(1),
          Json.fromInt(2),
          Json.fromInt(3)
        )
      )
    ),
    Json.fromInt(3)
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Json.obj(
      "foo" -> Json.obj(
        "enabled" -> Json.fromBoolean(false),
        "bar" -> Json.fromString("Baz")
      )
    )
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Json.obj(
      "foo" -> Json.obj(
        "enabled" -> Json.fromBoolean(true),
        "bar" -> Json.fromString("Baz")
      )
    ),
    Json.obj(
      "enabled" -> Json.fromBoolean(true),
      "bar" -> Json.fromString("Baz")
    )
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Json.obj(
      "foo" -> Json.obj(
        "enabled" -> Json.fromBoolean(false),
        "bar" -> Json.fromString("Baz")
      )
    )
  )

  testSolve(
    Filter(
      Eq(Property(StringLiteral("enabled"), This), BooleanLiteral(true)),
      Property(StringLiteral("foo"), Root)
    ),
    Json.obj(
      "foo" -> Json.obj(
        "enabled" -> Json.fromBoolean(false),
        "bar" -> Json.fromString("Baz")
      )
    )
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Json.obj(
      "foo" -> Json.arr(
        Json.obj(
          "enabled" -> Json.fromBoolean(false),
          "bar" -> Json.fromString("Baz")
        ),
        Json.obj(
          "enabled" -> Json.fromBoolean(true),
          "bar" -> Json.fromString("Bazzinga")
        )
      )
    ),
    Json.obj(
      "enabled" -> Json.fromBoolean(true),
      "bar" -> Json.fromString("Bazzinga")
    )
  )

  testSolve(
    Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root)),
    Json.obj(
      "foo" -> Json.arr(
        Json.obj(
          "enabled" -> Json.fromBoolean(false),
          "bar" -> Json.fromString("Baz")
        ),
        Json.obj(
          "enabled" -> Json.fromBoolean(true),
          "bar" -> Json.fromString("Bazzinga")
        )
      )
    ),
    Json.obj(
      "enabled" -> Json.fromBoolean(true),
      "bar" -> Json.fromString("Bazzinga")
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(1), NullLiteral, NullLiteral, Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )
  )

  testSolve(
    ArraySlice(NullLiteral, NumberLiteral(3), NullLiteral, Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2)
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(1), NumberLiteral(3), NullLiteral, Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(1),
      Json.fromInt(2)
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(1), NumberLiteral(4), NumberLiteral(2), Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(1),
      Json.fromInt(3)
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(-1), NullLiteral, NullLiteral, Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(5)
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(-3), NumberLiteral(-1), NullLiteral, Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(3),
      Json.fromInt(4)
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(3), NullLiteral, NumberLiteral(-1), Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(3),
      Json.fromInt(2),
      Json.fromInt(1),
      Json.fromInt(0)
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(-2), NullLiteral, NumberLiteral(-1), Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(4),
      Json.fromInt(3),
      Json.fromInt(2),
      Json.fromInt(1),
      Json.fromInt(0)
    )
  )

  testSolve(
    ArraySlice(NullLiteral, NumberLiteral(-4), NumberLiteral(-1), Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(5),
      Json.fromInt(4),
      Json.fromInt(3)
    )
  )

  testSolve(
    ArraySlice(NullLiteral, NumberLiteral(3), NumberLiteral(-1), Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(5),
      Json.fromInt(4)
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(4), NumberLiteral(3), NumberLiteral(-1), Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(4)
    )
  )

  testSolve(
    ArraySlice(NumberLiteral(-2), NumberLiteral(-4), NumberLiteral(-1), Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(4),
      Json.fromInt(3)
    )
  )

  testSolve(
    ArraySlice(NullLiteral, NullLiteral, NumberLiteral(-2), Root),
    Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    ),
    Json.arr(
      Json.fromInt(4),
      Json.fromInt(2),
      Json.fromInt(0)
    )
  )

  // Eq ***

  testSolve(
    Eq(StringLiteral("foo"), StringLiteral("foo")),
    Json.Null,
    Json.True
  )

  testSolve(
    Eq(NumberLiteral(3), NumberLiteral(3)),
    Json.Null,
    Json.True
  )

  testSolve(
    Eq(NumberLiteral(3), NumberLiteral(5)),
    Json.Null,
    Json.False
  )

  testSolve(
    Eq(StringLiteral("3"), NumberLiteral(3)),
    Json.Null,
    Json.False
  )

  testSolve(
    Eq(This, NumberLiteral(3)),
    Json.fromInt(3),
    Json.True
  )

  testSolve(
    Eq(This, StringLiteral("foo")),
    Json.fromString("foo"),
    Json.True
  )

  testSolve(
    Eq(This, StringLiteral("bar")),
    Json.fromString("foo"),
    Json.False
  )

  testSolve(
    Eq(Property(StringLiteral("foo"), Root), Property(StringLiteral("bar"), Root)),
    Json.obj("foo" -> Json.fromInt(8), "bar" -> Json.fromInt(9)),
    Json.False
  )

  testSolve(
    Eq(Property(StringLiteral("foo"), Root), Property(StringLiteral("bar"), Root)),
    Json.obj("foo" -> Json.fromInt(8), "bar" -> Json.fromInt(8)),
    Json.True
  )

  // Gt ***

  testSolve(
    Gt(NumberLiteral(9), NumberLiteral(8)),
    Json.Null,
    Json.True
  )

  testSolve(
    Gt(NumberLiteral(9), NumberLiteral(9)),
    Json.Null,
    Json.False
  )

  testSolve(
    Gt(NumberLiteral(8), NumberLiteral(9)),
    Json.Null,
    Json.False
  )

  testSolve(
    Gt(StringLiteral("9"), NumberLiteral(8)),
    Json.Null,
    Json.False
  )

  testSolve(
    Gt(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Json.obj("left" -> Json.fromInt(9), "right" -> Json.fromInt(8)),
    Json.True
  )

  testSolve(
    Gt(
      NumberLiteral(9),
      Property(StringLiteral("right"), Root)
    ),
    Json.obj("right" -> Json.fromInt(8)),
    Json.True
  )

  // Lt ***

  testSolve(
    Lt(NumberLiteral(9), NumberLiteral(8)),
    Json.Null,
    Json.False
  )

  testSolve(
    Lt(NumberLiteral(9), NumberLiteral(9)),
    Json.Null,
    Json.False
  )

  testSolve(
    Lt(NumberLiteral(8), NumberLiteral(9)),
    Json.Null,
    Json.True
  )

  testSolve(
    Lt(StringLiteral("9"), NumberLiteral(8)),
    Json.Null,
    Json.False
  )

  testSolve(
    Lt(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Json.obj("left" -> Json.fromInt(8), "right" -> Json.fromInt(9)),
    Json.True
  )

  // ** Gte

  testSolve(
    Gte(NumberLiteral(9), NumberLiteral(8)),
    Json.Null,
    Json.True
  )

  testSolve(
    Gte(NumberLiteral(9), NumberLiteral(9)),
    Json.Null,
    Json.True
  )

  testSolve(
    Gte(NumberLiteral(8), NumberLiteral(9)),
    Json.Null,
    Json.False
  )

  testSolve(
    Gte(StringLiteral("9"), NumberLiteral(8)),
    Json.Null,
    Json.False
  )

  testSolve(
    Gte(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Json.obj("left" -> Json.fromInt(8), "right" -> Json.fromInt(8)),
    Json.True
  )

  testSolve(
    Gte(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Json.obj("left" -> Json.fromInt(9), "right" -> Json.fromInt(8)),
    Json.True
  )

  // Lte ***

  testSolve(
    Lte(NumberLiteral(8), NumberLiteral(9)),
    Json.Null,
    Json.True
  )

  testSolve(
    Lte(NumberLiteral(9), NumberLiteral(9)),
    Json.Null,
    Json.True
  )

  testSolve(
    Lte(NumberLiteral(9), NumberLiteral(8)),
    Json.Null,
    Json.False
  )

  testSolve(
    Lte(StringLiteral("8"), NumberLiteral(8)),
    Json.Null,
    Json.False
  )

  testSolve(
    Lte(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Json.obj("left" -> Json.fromInt(8), "right" -> Json.fromInt(8)),
    Json.True
  )

  testSolve(
    Lte(
      Property(StringLiteral("left"), Root),
      Property(StringLiteral("right"), Root)
    ),
    Json.obj("left" -> Json.fromInt(8), "right" -> Json.fromInt(9)),
    Json.True
  )

  // Not ***

  testSolve(
    Not(BooleanLiteral(true)),
    Json.Null,
    Json.False
  )

  testSolve(
    Not(Root),
    Json.False,
    Json.True
  )

  testSolve(
    Not(This),
    Json.True,
    Json.False
  )

  // Or ***

  testSolve(
    Or(BooleanLiteral(false), BooleanLiteral(false)),
    Json.Null,
    Json.False
  )

  testSolve(
    Or(BooleanLiteral(true), BooleanLiteral(false)),
    Json.Null,
    Json.True
  )

  testSolve(
    Or(BooleanLiteral(false), BooleanLiteral(true)),
    Json.Null,
    Json.True
  )

  testSolve(
    Or(BooleanLiteral(true), BooleanLiteral(true)),
    Json.Null,
    Json.True
  )

  // And ***

  testSolve(
    And(BooleanLiteral(false), BooleanLiteral(false)),
    Json.Null,
    Json.False
  )

  testSolve(
    And(BooleanLiteral(true), BooleanLiteral(false)),
    Json.Null,
    Json.False
  )

  testSolve(
    And(BooleanLiteral(false), BooleanLiteral(true)),
    Json.Null,
    Json.False
  )

  testSolve(
    And(BooleanLiteral(true), BooleanLiteral(true)),
    Json.Null,
    Json.True
  )

  // In ***

  testSolve(
    In(StringLiteral("bar"), Root),
    Json.arr(Json.fromString("foo"), Json.fromString("bar"), Json.fromString("baz")),
    Json.True
  )

  testSolve(
    In(StringLiteral("foo"), Root),
    Json.obj("foo" -> Json.fromString("bar")),
    Json.True
  )

  testSolve(
    In(StringLiteral("foo"), Root),
    Json.arr(Json.fromString("bar"), Json.fromString("baz")),
    Json.False
  )

  testSolve(
    In(StringLiteral("foo"), Root),
    Json.obj("bar" -> Json.fromString("baz")),
    Json.False
  )

  // Plus ***

  testSolve(
    Plus(NumberLiteral(1), Root),
    Json.fromInt(2),
    Json.fromInt(3)
  )

  testSolve(
    Plus(NumberLiteral(1), Root),
    Json.fromInt(-2),
    Json.fromInt(-1)
  )

  testSolve(
    Minus(NumberLiteral(5), Root),
    Json.fromInt(2),
    Json.fromInt(3)
  )

  testSolve(
    Minus(NumberLiteral(5), Root),
    Json.fromInt(-2),
    Json.fromInt(7)
  )

  testSolve(
    Minus(Minus(NumberLiteral(5), NumberLiteral(1)), Root),
    Json.fromInt(-2),
    Json.fromInt(6)
  )

  testSolve(
    Times(NumberLiteral(10), Root),
    Json.fromInt(2),
    Json.fromInt(20)
  )

  testSolve(
    Times(NumberLiteral(10), Root),
    Json.fromInt(-1),
    Json.fromInt(-10)
  )

  testSolve(
    DividedBy(NumberLiteral(5), Root),
    Json.fromInt(-2),
    Json.fromBigDecimal(BigDecimal(-2.5f))
  )

  testSolve(
    Modulo(NumberLiteral(5), Root),
    Json.fromInt(-2),
    Json.fromInt(1)
  )

  testSolve(
    Modulo(NumberLiteral(5), Root),
    Json.fromInt(5),
    Json.fromInt(0)
  )

  testSolve(
    Modulo(NumberLiteral(5), Root),
    Json.fromInt(1),
    Json.fromInt(0)
  )

  testSolve(
    Modulo(NumberLiteral(6), Root),
    Json.fromInt(3),
    Json.fromInt(0)
  )

  // Union ***

  testSolve(
    Union(Vector(Root)),
    Json.fromInt(8),
    Json.fromInt(8)
  )

  testSolve(
    Union(Vector(Root, Root)),
    Json.fromInt(8),
    Json.fromInt(8),
    Json.fromInt(8)
  )

  testSolve(
    Union(Vector(Property(StringLiteral("foo"), Root), Property(StringLiteral("bar"), This))),
    json"""{"foo": 1, "bar": 2}""",
    Json.fromInt(1),
    Json.fromInt(2)
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
    json"""{"foo": 1, "bar": 2, "baz": 3}""",
    Json.fromInt(1),
    Json.fromInt(2),
    Json.fromInt(3)
  )
}
