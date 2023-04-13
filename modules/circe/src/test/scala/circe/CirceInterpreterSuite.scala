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

// package com.filippodeluca.jsonpath
// package circe

// import io.circe.Json

// class CirceInterpreterSuite extends munit.FunSuite {

//   import CirceInterpreter.*

//   test("toBoolean should return false for null") {
//     assertEquals(toBoolean(Json.Null), false)
//   }

//   test("toBoolean should return false for empty string") {
//     assertEquals(toBoolean(Json.fromString("")), false)
//   }

//   test("toBoolean should return false for 0") {
//     assertEquals(toBoolean(Json.fromInt(0)), false)
//   }

//   test("toBoolean should return false for false") {
//     assertEquals(toBoolean(Json.fromBoolean(false)), false)
//   }

//   test("toBoolean should return true for true") {
//     assertEquals(toBoolean(Json.fromBoolean(true)), true)
//   }

//   test("toBoolean should return true for notNull") {
//     assertEquals(toBoolean(Json.fromString("foo")), true)
//   }

//   test("toBoolean should return true for 1") {
//     assertEquals(toBoolean(Json.fromInt(1)), true)
//   }

// }
