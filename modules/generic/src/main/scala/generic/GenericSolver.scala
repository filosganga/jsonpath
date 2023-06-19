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

import com.filippodeluca.jsonpath.ast.*

object GenericSolver {

  case class Context(values: Vector[Any], root: Any) extends Ctx[Context, Any] {
    // Returns Some only if there is only one result in the result list otherwise None
    def value: Option[Any] = if (values.size == 1) {
      values.headOption
    } else {
      None
    }

    def many(values: Vector[Any], root: Any) = Context(values, root)
    def one(value: Any, root: Any) = Context.one(value, root)
    def current = this

    def nullCtx(root: Any) = Context.one(None, root)
    def booleanCtx(value: Boolean, root: Any) = Context.one(value, root)
    def stringCtx(value: String, root: Any) = Context.one(value, root)
    def numberCtx(value: Double, root: Any) = Context.one(value, root)

    def arrayValue(any: Any): Option[Seq[Any]] = any match {
      case arr: Seq[Any] => arr.some
      case _ => None
    }
    def arrayToValue(arr: Seq[Any]): Any = arr.asInstanceOf[Any]

    def mapValue(any: Any): Option[Map[String, Any]] = any match {
      case map: Map[String, Any] => map.some
      case _ => None
    }

    def propertyKey(any: Any): Option[String] = any.toString.some

    def stringValue(any: Any): Option[String] = any match {
      case s: String => s.some
      case _ => None
    }

    def intValue(any: Any): Option[Int] = any match {
      case n: Int => n.toInt.some
      case n: Long => n.toInt.some
      case n: Double => n.toInt.some
      case n: BigDecimal => n.toInt.some
      case n: BigInt => n.toInt.some
      case _ => None
    }

    def doubleValue(any: Any): Option[Double] = {
      any match {
        case n: Int => n.toDouble.some
        case n: Long => n.toDouble.some
        case n: Double => n.some
        case n: BigDecimal => n.toDouble.some
        case n: BigInt => n.toDouble.some
        case _ => None
      }
    }

    // According to https://www.sitepoint.com/javascript-truthy-falsy/
    def booleanValue(any: Any): Boolean = any match {
      case n: Int => n != 0
      case n: Long => n != 0
      case n: Double => n != 0
      case n: BigDecimal => n != 0
      case n: BigInt => n != 0
      case null => false
      case None => false
      case s: String => !s.isEmpty
      case false => false
      case _ => true
    }
  }

  object Context {
    def one(value: Any, root: Any) = Context(Vector(value), root)
  }

  def solve(exp: Exp, source: Any): Vector[Any] = {
    val sourceCtx = Context.one(source, source)
    sourceCtx.loop(exp).values
  }
}
