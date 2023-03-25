package com.filippodeluca.jsonpath
package circe

import io.circe.Json

class CirceJsonPathSuite extends munit.FunSuite {

  case class Case(
      expression: String,
      source: Json,
      expectedResult: Json
  )

  List(
    Case(
      expression = "$",
      source = Json.fromString("it is me"),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$.foo",
      source = Json.obj(
        "foo" -> Json.fromString("it is me")
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$['foo']",
      source = Json.obj(
        "foo" -> Json.fromString("it is me")
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$[2]",
      source = Json.arr(
        Json.fromString("it isn't me"),
        Json.fromString("it isn't me"),
        Json.fromString("it is me"),
        Json.fromString("it isn't me")
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$.foo.bar['baz']",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.obj("baz" -> Json.fromString("it is me"))
        )
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$['foo']",
      source = Json.obj(
        "foo" -> Json.fromString("it is me")
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$.foo.bar[2]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.fromString("it isn't me"),
            Json.fromString("it isn't me"),
            Json.fromString("it is me"),
            Json.fromString("it isn't me")
          )
        )
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$.foo.bar[1].baz",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.obj(
              "baz" -> Json.fromString("it isn't me")
            ),
            Json.obj(
              "baz" -> Json.fromString("it is me")
            ),
            Json.obj(
              "baz" -> Json.fromString("it isn't me")
            )
          )
        )
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$['foo']['bar'][2]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.fromString("it isn't me"),
            Json.fromString("it isn't me"),
            Json.fromString("it is me"),
            Json.fromString("it isn't me")
          )
        )
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$.foo.bar[?($.foo)]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.fromString("it is me")
        )
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$[?(true || false)]",
      source = Json.obj(
        "foo" -> Json.fromString("whatever"),
        "bar" -> Json.fromString("whatever")
      ),
      expectedResult = Json.obj(
        "foo" -> Json.fromString("whatever"),
        "bar" -> Json.fromString("whatever")
      )
    ),
    Case(
      expression = "$[?(true && true)]",
      source = Json.obj(
        "foo" -> Json.fromString("whatever"),
        "bar" -> Json.fromString("whatever")
      ),
      expectedResult = Json.obj(
        "foo" -> Json.fromString("whatever"),
        "bar" -> Json.fromString("whatever")
      )
    ),
    Case(
      expression = "$[?(true && false)]",
      source = Json.obj(
        "foo" -> Json.fromString("whatever"),
        "bar" -> Json.fromString("whatever")
      ),
      expectedResult = Json.Null
    ),
    Case(
      expression = "$.foo.bar[?(!$.foo)]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.fromString("it is not me")
        )
      ),
      expectedResult = Json.Null
    ),
    Case(
      expression = "$.foo.bar[?(@.pickMe)]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it isn't me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(true),
              "name" -> Json.fromString("it is me"),
              "pickMe" -> Json.fromString("whatever")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it isn't me")
            ),
            Json.fromString("it isn't me"),
            Json.fromString("it isn't me")
          )
        )
      ),
      expectedResult = Json.arr(
        Json.obj(
          "enabled" -> Json.fromBoolean(true),
          "name" -> Json.fromString("it is me"),
          "pickMe" -> Json.fromString("whatever")
        )
      )
    ),
    Case(
      expression = "$.foo.bar[?(!@.enabled)]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it is me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(true),
              "name" -> Json.fromString("it is not me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it is me")
            ),
            Json.fromString("it is me"),
            Json.fromString("it is me")
          )
        )
      ),
      expectedResult = Json.arr(
        Json.obj(
          "enabled" -> Json.fromBoolean(false),
          "name" -> Json.fromString("it is me")
        ),
        Json.obj(
          "enabled" -> Json.fromBoolean(false),
          "name" -> Json.fromString("it is me")
        ),
        Json.fromString("it is me"),
        Json.fromString("it is me")
      )
    ),
    Case(
      expression = "$.foo.bar[?(@.enabled && !@.deleted)]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it is not me"),
              "deleted" -> Json.fromBoolean(false)
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(true),
              "name" -> Json.fromString("it is me"),
              "deleted" -> Json.fromBoolean(false)
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(true),
              "name" -> Json.fromString("it is not me"),
              "deleted" -> Json.fromBoolean(true)
            ),
            Json.fromString("it isn't me"),
            Json.fromString("it isn't me")
          )
        )
      ),
      expectedResult = Json.arr(
        Json.obj(
          "enabled" -> Json.fromBoolean(true),
          "name" -> Json.fromString("it is me"),
          "deleted" -> Json.fromBoolean(false)
        )
      )
    ),
    Case(
      expression = "$.foo.bar[?(!@.enabled == true)]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it is me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(true),
              "name" -> Json.fromString("it is not me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it is me")
            ),
            Json.fromString("it is me"),
            Json.fromString("it is me")
          )
        )
      ),
      expectedResult = Json.arr(
        Json.obj(
          "enabled" -> Json.fromBoolean(false),
          "name" -> Json.fromString("it is me")
        ),
        Json.obj(
          "enabled" -> Json.fromBoolean(false),
          "name" -> Json.fromString("it is me")
        ),
        Json.fromString("it is me"),
        Json.fromString("it is me")
      )
    ),
    Case(
      expression = "$.foo.bar[?(@.enabled == true)]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it isn't me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(true),
              "name" -> Json.fromString("it is me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it isn't me")
            ),
            Json.fromString("it isn't me"),
            Json.fromString("it isn't me")
          )
        )
      ),
      expectedResult = Json.arr(
        Json.obj(
          "enabled" -> Json.fromBoolean(true),
          "name" -> Json.fromString("it is me")
        )
      )
    ),
    Case(
      expression = "$.foo.bar[?(@.pickMe)].name",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it isn't me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(true),
              "name" -> Json.fromString("it is me"),
              "pickMe" -> Json.fromString("whatever")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it isn't me")
            ),
            Json.fromString("it isn't me"),
            Json.fromString("it isn't me")
          )
        )
      ),
      expectedResult = Json.arr(
        Json.fromString("it is me")
      )
    ),
    Case(
      expression = "$.foo.bar[?(@.enabled == true)].name",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it isn't me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(true),
              "name" -> Json.fromString("it is me")
            ),
            Json.obj(
              "enabled" -> Json.fromBoolean(false),
              "name" -> Json.fromString("it isn't me")
            ),
            Json.fromString("it isn't me"),
            Json.fromString("it isn't me")
          )
        )
      ),
      expectedResult = Json.arr(
        Json.fromString("it is me")
      )
    ),
    Case(
      expression = "$.foo.bar[?(@.enabled == true)]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.obj(
            "enabled" -> Json.fromBoolean(true),
            "name" -> Json.fromString("it is me")
          )
        )
      ),
      expectedResult = Json.obj(
        "enabled" -> Json.fromBoolean(true),
        "name" -> Json.fromString("it is me")
      )
    ),
    Case(
      expression = "$.foo.bar[?(@.enabled == true)].name",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.obj(
            "enabled" -> Json.fromBoolean(true),
            "name" -> Json.fromString("it is me")
          )
        )
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$.foo.bar[?($.foo)].name",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.obj(
            "enabled" -> Json.fromBoolean(true),
            "name" -> Json.fromString("it is me")
          )
        )
      ),
      expectedResult = Json.fromString("it is me")
    ),
    Case(
      expression = "$.foo.bar[?($.bar)].name",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.obj(
            "enabled" -> Json.fromBoolean(true),
            "name" -> Json.fromString("it is me")
          )
        )
      ),
      expectedResult = Json.Null
    ),
    Case(
      expression = "$.foo.bar[?(@.tags empty true)]",
      source = Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.arr(
            Json.obj(
              "name" -> Json.fromString("it isn't me"),
              "tags" -> Json.arr(
                Json.fromString("one")
              )
            ),
            Json.obj(
              "name" -> Json.fromString("it is me"),
              "tags" -> Json.arr()
            ),
            Json.obj(
              "name" -> Json.fromString("it isn't me"),
              "tags" -> Json.arr(
                Json.fromString("one"),
                Json.fromString("two")
              )
            ),
            Json.fromString("it isn't me"),
            Json.fromString("it isn't me")
          )
        )
      ),
      expectedResult = Json.arr(
        Json.obj(
          "name" -> Json.fromString("it is me"),
          "tags" -> Json.arr()
        )
      )
    )
  ).map { case Case(expression, source, expected) =>
    test(s"should support \"${expression}\"") {

      val result = CirceJsonPath.parse(expression).map { matcher =>
        matcher(source)
      }

      assertEquals(
        clue(result),
        Right(expected),
        s"Cannot parse ${expression}"
      )
    }
  }
}
