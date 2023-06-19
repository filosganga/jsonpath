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
  // TODO Shall we use ADT instead?
  case class Context(values: Vector[Json], root: Json) {
    // Returns Some only if there is only one result in the result list otherwise None
    def value: Option[Json] = if (values.size == 1) {
      values.headOption
    } else {
      None
    }

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

    def loop(exp: ast.Exp): Context = exp match {
      case NullLiteral => Context.one(Json.Null, root)
      case StringLiteral(value) => Context.one(Json.fromString(value), root)
      case BooleanLiteral(value) => Context.one(Json.fromBoolean(value), root)
      case NumberLiteral(value) =>
        Context.one(Json.fromDouble(value).getOrElse(Json.fromBigDecimal(BigDecimal(value))), root)
      case This => this
      case Root => Context.one(root, root)

      case Property(nameExp, target) =>
        val targetCtx = loop(target)
        val results = targetCtx.values.mapFilter { target =>
          // TODO Should id fail if the value is an array?
          val newTargetCtx = Context.one(target, root)
          val name = newTargetCtx.loop(nameExp).value.flatMap(stringValue)
          name
            .flatMap { name =>
              target.asObject.flatMap(obj => obj(name))
            }
        }
        Context(results, root)
      case Wildcard(target) =>
        val results = loop(target).values.mapFilter { target =>
          target.asArray.orElse(target.asObject.map(_.values.toVector))
        }.flatten

        Context(results, root)

      case ArrayIndex(indexExp, targetExp) =>
        val targetCtx = loop(targetExp)
        val results = targetCtx.values.mapFilter { target =>
          // TODO Should id fail if the value is an array?
          val newTargetCtx = Context.one(target, root)
          val index = newTargetCtx.loop(indexExp).value.flatMap(intValue)
          index
            .flatMap { index =>
              target.asArray
                .flatMap { arr =>
                  if (index < 0) {
                    arr.get((arr.length + index).toLong)
                  } else {
                    arr.get(index.toLong)
                  }

                }
            }

        }
        Context(results, root)

      case ArraySlice(startExp, endExp, stepExp, targetExp) =>
        val targetCtx = loop(targetExp)
        val results = targetCtx.values.mapFilter { target =>
          target.asArray.map { array =>
            val targetCtx = Context.one(target, root)
            val start = targetCtx.loop(startExp).value.flatMap(intValue)
            val end = targetCtx.loop(endExp).value.flatMap(intValue)
            val step = targetCtx.loop(stepExp).value.flatMap(intValue).getOrElse(1)

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

        Context(results, root)

      case Filter(predicate, target) =>
        val targetCtx = loop(target)
        val results = targetCtx.values.flatMap { target =>
          target.asArray.fold {
            // TODO Should id fail if the value is an array?
            val newTargetCtx = Context.one(target, root)
            val predicateValue =
              newTargetCtx.loop(predicate).value.map(booleanValue).getOrElse(false)
            if (predicateValue) {
              Vector(target)
            } else {
              Vector.empty
            }
          } { targets =>
            targets.filter { item =>
              // TODO Should id fail if the value is an array?
              val itemCtx = Context.one(item, root)
              itemCtx.loop(predicate).value.map(booleanValue).getOrElse(false)
            }
          }
        }
        Context(results, root)

      // TODO think about associativity and how to handle operators on multiple results

      case Eq(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield left == right

        Context.one(result.fold(Json.False)(Json.fromBoolean), root)

      case Gt(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield {
          left.asString.exists(l => right.asString.exists(r => l > r)) ||
          left.asNumber.exists(l => right.asNumber.exists(r => l.toDouble > r.toDouble))
        }

        Context.one(result.fold(Json.False)(Json.fromBoolean), root)

      case Gte(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield {
          left.asString.exists(l => right.asString.exists(r => l > r)) ||
          left.asNumber.exists(l => right.asNumber.exists(r => l.toDouble >= r.toDouble))
        }

        Context.one(result.fold(Json.False)(Json.fromBoolean), root)

      case Lt(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield {
          left.asString.exists(l => right.asString.exists(r => l > r)) ||
          left.asNumber.exists(l => right.asNumber.exists(r => l.toDouble < r.toDouble))
        }

        Context.one(result.fold(Json.False)(Json.fromBoolean), root)

      case Lte(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield {
          left.asString.exists(l => right.asString.exists(r => l > r)) ||
          left.asNumber.exists(l => right.asNumber.exists(r => l.toDouble <= r.toDouble))
        }

        Context.one(result.fold(Json.False)(Json.fromBoolean), root)

      case Not(exp) =>
        val result = for {
          value <- loop(exp).value.map(booleanValue)
        } yield Json.fromBoolean(!value)

        Context(result.toVector, root)

      case Or(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value.map(booleanValue)
          right <- loop(rightExp).value.map(booleanValue)
        } yield left || right

        Context.one(result.fold(Json.False)(Json.fromBoolean), root)

      case And(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value.map(booleanValue)
          right <- loop(rightExp).value.map(booleanValue)
        } yield left && right

        Context.one(result.fold(Json.False)(Json.fromBoolean), root)

      case In(itemExp, setExp) =>
        val result = for {
          set <- loop(setExp).value
          item <- loop(itemExp).value
        } yield {
          set.asArray.exists(_.contains(item)) ||
          item.asString.exists(key => set.asObject.exists(jso => jso.contains(key)))
        }

        Context(result.map(Json.fromBoolean).toVector, root)

      case Plus(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- r.asNumber
          ln <- l.asNumber
          result <- Json.fromDouble(ln.toDouble + rn.toDouble)
        } yield result
        Context(result.toVector, root)

      case Minus(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- r.asNumber
          ln <- l.asNumber
          result <- Json.fromDouble(ln.toDouble - rn.toDouble)
        } yield result
        Context(result.toVector, root)

      case Times(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- r.asNumber
          ln <- l.asNumber
          result <- Json.fromDouble(ln.toDouble * rn.toDouble)
        } yield result
        Context(result.toVector, root)

      case DividedBy(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- r.asNumber
          ln <- l.asNumber
          result <- Json.fromDouble(ln.toDouble / rn.toDouble)
        } yield result
        Context(result.toVector, root)

      case Modulo(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- r.asNumber
          ln <- l.asNumber
          result <- Json.fromDouble(ln.toDouble % rn.toDouble)
        } yield result
        Context(result.toVector, root)

      case Union(exps) =>
        Context(exps.flatMap(exp => loop(exp).values), root)
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
