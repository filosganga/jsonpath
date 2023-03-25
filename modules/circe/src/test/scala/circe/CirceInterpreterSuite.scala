package com.filippodeluca.jsonpath
package circe

import io.circe.Json

class CirceInterpreterSuite extends munit.FunSuite {

  import CirceInterpreter._

  test("toBoolean should return false for null") {
    assertEquals(toBoolean(Json.Null), false)
  }

  test("toBoolean should return false for empty string") {
    assertEquals(toBoolean(Json.fromString("")), false)
  }

  test("toBoolean should return false for 0") {
    assertEquals(toBoolean(Json.fromInt(0)), false)
  }

  test("toBoolean should return false for false") {
    assertEquals(toBoolean(Json.fromBoolean(false)), false)
  }

  test("toBoolean should return true for true") {
    assertEquals(toBoolean(Json.fromBoolean(true)), true)
  }

  test("toBoolean should return true for notNull") {
    assertEquals(toBoolean(Json.fromString("foo")), true)
  }

  test("toBoolean should return true for 1") {
    assertEquals(toBoolean(Json.fromInt(1)), true)
  }

}
