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

object CirceSolver {

  case class Context(values: Vector[Json]) {
    def value: Option[Json] = if (values.size == 1) {
      values.headOption
    } else {
      None
    }
  }

  object Context {
    def one(value: Json) = Context(Vector(value))
  }

  def solve(exp: ast.Exp, source: Json): Vector[Json] = {

    // $.foo.bar

    // solve(bar, solve("foo", solve($, current)))

    def stringValue(json: Json): Option[String] = {
      json.asString
        .orElse(json.asNumber.map(_.toDouble.toString))
        .orElse(json.asBoolean.map(_.toString))
    }

    def intValue(json: Json): Option[Int] = {
      json.asNumber.flatMap(_.toInt)
    }

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

    def loop(exp: ast.Exp, current: Context, root: Json): Context = exp match {
      case NullLiteral => Context.one(Json.Null)
      case StringLiteral(value) => Context.one(Json.fromString(value))
      case BooleanLiteral(value) => Context.one(Json.fromBoolean(value))
      case NumberLiteral(value) =>
        Context.one(Json.fromDouble(value).getOrElse(Json.fromBigDecimal(BigDecimal(value))))
      case This => current
      case Root => Context.one(root)

      case Property(nameExp, parent) =>
        val targetCtx = loop(parent, current, root)
        val results = targetCtx.values.mapFilter { target =>
          // TODO Fail if the value is an array
          val name = loop(nameExp, Context.one(target), root).value.flatMap(stringValue)
          name
            .flatMap { name =>
              target.asObject.flatMap(obj => obj(name))
            }
        }
        Context(results)
      case Wildcard(target) =>
        val results = loop(target, current, root).values.mapFilter { target =>
          target.asArray.orElse(target.asObject.map(_.values.toVector))
        }.flatten

        Context(results)

      case ArrayIndex(indexExp, parentExp) =>
        val targetCtx = loop(parentExp, current, root)
        val results = targetCtx.values.mapFilter { target =>
          // TODO Fail if the value is an array
          val index = loop(indexExp, Context.one(target), root).value.flatMap(intValue)
          index
            .flatMap { index =>
              target.asArray
                .flatMap { arr =>
                  arr.get(index.toLong)
                }
            }

        }
        Context(results)

      case ArraySlice(startExp, endExp, stepExp, targetExp) =>
        val targetCtx = loop(targetExp, current, root)
        val results = targetCtx.values.mapFilter { target =>
          target.asArray.map { array =>
            val targetCtx = Context.one(target)
            val start = loop(startExp, targetCtx, root).value.flatMap(intValue)
            val end = loop(endExp, targetCtx, root).value.flatMap(intValue)
            val step = loop(stepExp, targetCtx, root).value.flatMap(intValue).getOrElse(1)

            val range = if (step > 0) {
              start.map(x => if (x < 0) array.size + x else x).getOrElse(0) until end
                .map(x => if (x < 0) array.size + x else x)
                .getOrElse(
                  array.length
                ) by step
            } else {
              (start.map(x => if (x < 0) array.size + x else x).getOrElse(array.size)) until end
                .map(x => if (x < 0) array.size + x else x)
                .getOrElse(-1) by step
            }

            Console.err.println(
              s"start: ${start}, end: ${end}, step: ${step}, range: ${range.toVector}"
            )

            Json.fromValues(range.toVector.mapFilter { idx =>
              val value = array.get(idx.toLong)

              value
            })
          }
        }

        Context(results)

      case Filter(predicate, parent) =>
        val targetCtx = loop(parent, current, root)
        val results = targetCtx.values.flatMap { target =>
          target.asArray.fold {
            // TODO Fail if the value is an array
            val predicateValue =
              loop(predicate, Context.one(target), root).value.map(booleanValue).getOrElse(false)
            if (predicateValue) {
              Vector(target)
            } else {
              Vector.empty
            }
          } { targets =>
            targets.filter { item =>
              // TODO Fail if the value is an array
              loop(predicate, Context.one(item), root).value.map(booleanValue).getOrElse(false)
            }
          }
        }
        Context(results)

      // TODO think about associativity

      case Eq(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
        } yield left == right

        Context.one(result.fold(Json.False)(Json.fromBoolean))

      case Gt(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
        } yield {
          left.asString.exists(l => right.asString.exists(r => l > r)) ||
          left.asNumber.exists(l => right.asNumber.exists(r => l.toDouble > r.toDouble))
        }

        Context.one(result.fold(Json.False)(Json.fromBoolean))

      case Gte(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
        } yield {
          left.asString.exists(l => right.asString.exists(r => l > r)) ||
          left.asNumber.exists(l => right.asNumber.exists(r => l.toDouble >= r.toDouble))
        }

        Context.one(result.fold(Json.False)(Json.fromBoolean))

      case Lt(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
        } yield {
          left.asString.exists(l => right.asString.exists(r => l > r)) ||
          left.asNumber.exists(l => right.asNumber.exists(r => l.toDouble < r.toDouble))
        }

        Context.one(result.fold(Json.False)(Json.fromBoolean))

      case Lte(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
        } yield {
          left.asString.exists(l => right.asString.exists(r => l > r)) ||
          left.asNumber.exists(l => right.asNumber.exists(r => l.toDouble <= r.toDouble))
        }

        Context.one(result.fold(Json.False)(Json.fromBoolean))

      case Not(exp) =>
        val result = for {
          value <- loop(exp, current, root).value.map(booleanValue)
        } yield Json.fromBoolean(!value)

        Context(result.toVector)

      case Or(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value.map(booleanValue)
          right <- loop(rightExp, current, root).value.map(booleanValue)
        } yield left || right

        Context.one(result.fold(Json.False)(Json.fromBoolean))

      case And(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value.map(booleanValue)
          right <- loop(rightExp, current, root).value.map(booleanValue)
        } yield left && right

        Context.one(result.fold(Json.False)(Json.fromBoolean))

      case In(itemExp, setExp) =>
        val result = for {
          set <- loop(setExp, current, root).value
          item <- loop(itemExp, current, root).value
        } yield {
          set.asArray.exists(_.contains(item)) ||
          item.asString.exists(key => set.asObject.exists(jso => jso.contains(key)))
        }

        Context(result.map(Json.fromBoolean).toVector)

    }

    loop(exp, Context.one(source), source).values
  }
}
