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

import com.filippodeluca.jsonpath.ast.*
import com.filippodeluca.jsonpath.generic.Ctx
import cats.instances.seq

object CirceSolver {
  // TODO Shall we use ADT instead?
  case class Context(values: Vector[Json], root: Json) extends Ctx[Context, Json] {
    // Returns Some only if there is only one result in the result list otherwise None
    def value: Option[Json] = if (values.size == 1) {
      values.headOption
    } else {
      None
    }

    def one(value: Json, root: Json) = Context.one(value, root)
    def many(values: Vector[Json], root: Json) = Context(values, root)
    def current = this

    def nullCtx(root: Json) = Context.one(Json.Null, root)
    def stringCtx(value: String, root: Json) = Context.one(Json.fromString(value), root)
    def booleanCtx(value: Boolean, root: Json) = Context.one(Json.fromBoolean(value), root)
    def numberCtx(value: Double, root: Json) =
      Context.one(Json.fromDouble(value).getOrElse(Json.fromBigDecimal(BigDecimal(value))), root)

    def arrayValue(json: Json): Option[Seq[Json]] = json.asArray
    def arrayToValue(seq: Seq[Json]): Json = Json.fromValues(seq)

    def mapValue(json: Json): Option[Map[String, Json]] = json.asObject.map(_.toMap)

    def propertyKey(json: Json): Option[String] = {
      json.asString
        .orElse(json.asNumber.map(_.toDouble.toString))
        .orElse(json.asBoolean.map(_.toString))
    }

    def stringValue(json: Json): Option[String] = json.asString

    def intValue(json: Json): Option[Int] = json.asNumber.flatMap(_.toInt)

    def doubleValue(json: Json): Option[Double] = json.asNumber.map(_.toDouble)

    // According to https://www.sitepoint.com/javascript-truthy-falsy/
    def booleanValue(json: Json): Boolean = {
      val isFalse = json.fold[Boolean](
        true,
        _ == false,
        jsn => jsn.toBigDecimal.exists(_ == BigDecimal(0)) || jsn.toDouble == 0d,
        _.isEmpty,
        _ => false,
        _ => false
      )
      !isFalse
    }
  }

  object Context {
    def one(value: Json, root: Json) = Context(Vector(value), root)
  }

  def solve(exp: ast.Exp, source: Json): Vector[Json] = {
    val sourceCtx = Context.one(source, source)
    sourceCtx.loop(exp).values
  }
}
