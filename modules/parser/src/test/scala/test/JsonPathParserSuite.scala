package com.filippodeluca.jsonpath
package parser

import cats.parse.*
import cats.syntax.all.*

import munit.Location

import com.filippodeluca.jsonpath.ast.*

class JsonPathParserSuite extends munit.FunSuite {

  def shouldParse(exp: String, expected: Exp)(implicit loc: Location) = {
    test(s"should parse $exp") {
      val result = JsonPathParser.parse(exp)
      assertEquals(result, Right(expected))
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
    BooleanLiteral(true)
  )

  shouldParse(
    "54.67",
    NumberLiteral(54.67)
  )

  shouldParse(
    "'123'",
    StringLiteral("123")
  )

  shouldParse(
    "$.test",
    Property(StringLiteral("test"), Root)
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
    Property(
      StringLiteral("baz"),
      Property(
        StringLiteral("bar"),
        Property(
          StringLiteral("foo"),
          This
        )
      )
    )
  )

  shouldParse(
    "@.foo['bar'].baz",
    Property(
      StringLiteral("baz"),
      Property(
        StringLiteral("bar"),
        Property(
          StringLiteral("foo"),
          This
        )
      )
    )
  )

  shouldParse(
    "@.foo[('bar')].baz",
    Property(
      StringLiteral("baz"),
      Property(
        StringLiteral("bar"),
        Property(
          StringLiteral("foo"),
          This
        )
      )
    )
  )

  shouldParse(
    "@.foo[(@.bar)].baz",
    Property(
      StringLiteral("baz"),
      Property(
        Property(
          StringLiteral("bar"),
          This
        ),
        Property(
          StringLiteral("foo"),
          This
        )
      )
    )
  )

  shouldParse(
    "! true",
    Not(BooleanLiteral(true))
  )

  shouldParse(
    "!@.foo",
    Not(Property(StringLiteral("foo"), This))
  )

  shouldParse(
    "!(@.foo)",
    Not(Property(StringLiteral("foo"), This))
  )

  shouldParse(
    "true && false",
    And(
      BooleanLiteral(true),
      BooleanLiteral(false)
    )
  )

  shouldParse(
    "true||false",
    Or(
      BooleanLiteral(true),
      BooleanLiteral(false)
    )
  )

  shouldParse(
    "1 > 2.5",
    Gt(
      NumberLiteral(1),
      NumberLiteral(2.5)
    )
  )

  shouldParse(
    "1.9 >=1.9",
    Gte(
      NumberLiteral(1.9),
      NumberLiteral(1.9)
    )
  )

  shouldParse(
    "1.93 <=1",
    Lte(
      NumberLiteral(1.93),
      NumberLiteral(1)
    )
  )

  shouldParse(
    "1 < 100",
    Lt(
      NumberLiteral(1),
      NumberLiteral(100)
    )
  )

  shouldParse(
    "0.23 == 0.23",
    Eq(
      NumberLiteral(0.23),
      NumberLiteral(0.23)
    )
  )

  shouldParse(
    "0.23 != 0.23",
    Not(
      Eq(
        NumberLiteral(0.23),
        NumberLiteral(0.23)
      )
    )
  )

  shouldParse(
    "$[1]",
    ArrayIndex(NumberLiteral(1), Root)
  )

  shouldParse(
    "$[-1]",
    ArrayIndex(NumberLiteral(-1), Root)
  )

  shouldParse(
    "$[1:1:1]",
    ArraySlice(NumberLiteral(1), NumberLiteral(1), NumberLiteral(1), Root)
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
    ArraySlice(NullLiteral, NumberLiteral(3), NullLiteral, Root)
  )

  shouldParse(
    "$[1:3]",
    ArraySlice(NumberLiteral(1), NumberLiteral(3), NullLiteral, Root)
  )

  shouldParse(
    "$[::1]",
    ArraySlice(NullLiteral, NullLiteral, NumberLiteral(1), Root)
  )

}
