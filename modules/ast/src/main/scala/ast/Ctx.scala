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

package com.filippodeluca.jsonpath.ast

import cats.syntax.all.*

abstract class Ctx[T <: Ctx[T, A], A](val values: Vector[A], val root: A) {
  // Returns Some only if there is only one result in the result list otherwise None
  def value: Option[A] = if (values.size == 1) {
    values.headOption
  } else {
    None
  }

  def one(value: A, root: A): T
  def many(values: Vector[A], root: A): T
  def current: T

  def nullCtx(root: A): T
  def stringCtx(value: String, root: A): T
  def booleanCtx(value: Boolean, root: A): T
  def numberCtx(value: Double, root: A): T

  def arrayValue(a: A): Option[Seq[A]]
  def arrayToValue(seq: Seq[A]): A

  def mapValue(a: A): Option[Map[String, A]]
  def propertyKey(a: A): Option[String]

  def intValue(a: A): Option[Int]
  def doubleValue(a: A): Option[Double]
  def stringValue(a: A): Option[String]
  def booleanValue(a: A): Boolean

  def loop(exp: Exp): T = exp match {
    case NullLiteral => nullCtx(root)
    case StringLiteral(value) => stringCtx(value, root)
    case BooleanLiteral(value) => booleanCtx(value, root)
    case NumberLiteral(value) => numberCtx(value, root)
    case This => current
    case Root => one(root, root)

    case prop: Property => getProperty(prop)
    case wildcard: Wildcard => getWildcard(wildcard)
    case idx: ArrayIndex => getArrayIndex(idx)
    case slice: ArraySlice => sliceArray(slice)
    case filter: Filter => applyFilter(filter)

    // TODO think about associativity and how to handle operators on multiple results

    case Eq(leftExp, rightExp) => {
      val result = for {
        left <- loop(leftExp).value
        right <- loop(rightExp).value
      } yield left == right

      booleanCtx(result.getOrElse(false), root)
    }

    case Not(exp) => {
      val result = for {
        value <- loop(exp).value.map(booleanValue)
      } yield !value

      result.fold(many(Vector.empty, root))(booleanCtx(_, root))
    }

    case Or(leftExp, rightExp) => {
      val result = for {
        left <- loop(leftExp).value.map(booleanValue)
        right <- loop(rightExp).value.map(booleanValue)
      } yield left || right

      booleanCtx(result.getOrElse(false), root)
    }

    case And(leftExp, rightExp) => {
      val result = for {
        left <- loop(leftExp).value.map(booleanValue)
        right <- loop(rightExp).value.map(booleanValue)
      } yield left && right

      booleanCtx(result.getOrElse(false), root)
    }

    case In(itemExp, setExp) => {
      val result = for {
        set <- loop(setExp).value
        item <- loop(itemExp).value
      } yield {
        arrayValue(set).exists(_.contains(item)) ||
        stringValue(item).exists(key => mapValue(set).exists(map => map.contains(key)))
      }

      booleanCtx(result.getOrElse(false), root)
    }

    case Gt(leftExp, rightExp) => {
      val result = for {
        left <- loop(leftExp).value
        right <- loop(rightExp).value
      } yield {
        stringValue(left).exists(l => stringValue(right).exists(r => l > r)) ||
        doubleValue(left).exists(l => doubleValue(right).exists(r => l > r))
      }

      booleanCtx(result.getOrElse(false), root)
    }

    case Gte(leftExp, rightExp) => {
      val result = for {
        left <- loop(leftExp).value
        right <- loop(rightExp).value
      } yield {
        stringValue(left).exists(l => stringValue(right).exists(r => l >= r)) ||
        doubleValue(left).exists(l => doubleValue(right).exists(r => l >= r))
      }

      booleanCtx(result.getOrElse(false), root)
    }

    case Lt(leftExp, rightExp) => {
      val result = for {
        left <- loop(leftExp).value
        right <- loop(rightExp).value
      } yield {
        stringValue(left).exists(l => stringValue(right).exists(r => l < r)) ||
        doubleValue(left).exists(l => doubleValue(right).exists(r => l < r))
      }

      booleanCtx(result.getOrElse(false), root)
    }

    case Lte(leftExp, rightExp) => {
      val result = for {
        left <- loop(leftExp).value
        right <- loop(rightExp).value
      } yield {
        stringValue(left).exists(l => stringValue(right).exists(r => l <= r)) ||
        doubleValue(left).exists(l => doubleValue(right).exists(r => l <= r))
      }

      booleanCtx(result.getOrElse(false), root)
    }

    case Plus(lExp, rExp) => {
      val result = for {
        r <- loop(rExp).value
        l <- loop(lExp).value
        rn <- doubleValue(r)
        ln <- doubleValue(l)
        result <- (ln + rn).some
      } yield result

      result.fold(many(Vector.empty, root))(numberCtx(_, root))
    }

    case Minus(lExp, rExp) => {
      val result = for {
        r <- loop(rExp).value
        l <- loop(lExp).value
        rn <- doubleValue(r)
        ln <- doubleValue(l)
        result <- (ln - rn).some
      } yield result

      result.fold(many(Vector.empty, root))(numberCtx(_, root))
    }

    case Times(lExp, rExp) => {
      val result = for {
        r <- loop(rExp).value
        l <- loop(lExp).value
        rn <- doubleValue(r)
        ln <- doubleValue(l)
        result <- (ln * rn).some
      } yield result

      result.fold(many(Vector.empty, root))(numberCtx(_, root))
    }

    case DividedBy(lExp, rExp) => {
      val result = for {
        r <- loop(rExp).value
        l <- loop(lExp).value
        rn <- doubleValue(r)
        ln <- doubleValue(l)
        result <- (ln / rn).some
      } yield result

      result.fold(many(Vector.empty, root))(numberCtx(_, root))
    }

    case Modulo(lExp, rExp) => {
      val result = for {
        r <- loop(rExp).value
        l <- loop(lExp).value
        rn <- doubleValue(r)
        ln <- doubleValue(l)
        result <- (ln % rn).some
      } yield result

      result.fold(many(Vector.empty, root))(numberCtx(_, root))
    }

    case Union(exps) => many(exps.flatMap(exp => loop(exp).values), root)
  }

  def sliceArray(slice: ArraySlice): T = {
    val targetCtx = loop(slice.target)
    val results = targetCtx.values.map { target =>
      val arr = arrayValue(target)
      arr.fold(arrayToValue(Vector.empty[A])) { arr =>
        val targetCtx = one(target, root)
        val start = targetCtx.loop(slice.start).value.flatMap(intValue)
        val end = targetCtx.loop(slice.end).value.flatMap(intValue)
        val step = targetCtx.loop(slice.step).value.flatMap(intValue).getOrElse(1)

        val range = if (step > 0) {
          start.map(x => if (x < 0) arr.size + x else x).getOrElse(0) until end
            .map(x => if (x < 0) arr.size + x else x)
            .getOrElse(
              arr.length
            ) by step
        } else {
          (start.map(x => if (x < 0) arr.size + x else x).getOrElse(arr.size)) until end
            .map(x => if (x < 0) arr.size + x else x)
            .getOrElse(-1) by step
        }

        Console.err.println(
          s"start: ${start}, end: ${end}, step: ${step}, range: ${range.toVector}"
        )

        arrayToValue(
          range.toVector
            .mapFilter { idx =>
              arr.get(idx.toLong)
            }
        )
      }
    }

    many(results, root)
  }

  def getProperty(prop: Property) = {
    val targetCtx = loop(prop.target)
    val results = targetCtx.values.mapFilter { target =>
      // TODO Should id fail if the value is an array?
      val newTargetCtx = one(target, root)
      val name = newTargetCtx.loop(prop.name).value.flatMap(propertyKey)
      name
        .flatMap { name =>
          mapValue(target).flatMap(obj => obj.get(name))
        }
    }
    many(results, root)
  }

  def getArrayIndex(idx: ArrayIndex) = {
    val targetCtx = loop(idx.target)
    val results = targetCtx.values.mapFilter { target =>
      // TODO Should id fail if the value is an array?
      val newTargetCtx = one(target, root)
      val index = newTargetCtx.loop(idx.index).value.flatMap(intValue)
      index
        .flatMap { index =>
          arrayValue(target)
            .flatMap { arr =>
              if (index < 0) {
                arr.get((arr.length + index).toLong)
              } else {
                arr.get(index.toLong)
              }
            }
        }

    }
    many(results, root)
  }

  def getWildcard(wildcard: Wildcard) = {
    val results = loop(wildcard.target).values.mapFilter { target =>
      arrayValue(target).orElse(mapValue(target).map(_.values))
    }.flatten

    many(results, root)
  }

  def applyFilter(filter: Filter) = {
    val targetCtx = loop(filter.target)
    val results = targetCtx.values.flatMap { target =>
      arrayValue(target).fold {
        // TODO Should id fail if the value is an array?
        val newTargetCtx = one(target, root)
        val predicateValue =
          newTargetCtx.loop(filter.predicate).value.map(booleanValue).getOrElse(false)
        if (predicateValue) {
          Vector(target)
        } else {
          Vector.empty
        }
      } { targets =>
        targets.filter { item =>
          // TODO Should id fail if the value is an array?
          val itemCtx = one(item, root)
          itemCtx.loop(filter.predicate).value.map(booleanValue).getOrElse(false)
        }
      }
    }
    many(results, root)
  }

}
