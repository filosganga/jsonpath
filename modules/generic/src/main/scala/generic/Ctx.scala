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

abstract class Ctx[T <: Ctx[T, A], A] {
  def root: A
  def value: Option[A]
  def values: Vector[A]

  def one(value: A, root: A): T
  def many(values: Vector[A], root: A): T

  def loop(exp: Exp): T

  def arrayValue(a: A): Option[Seq[A]]
  def arrayToValue(seq: Seq[A]): A

  def mapValue(a: A): Option[Map[String, A]]

  def intValue(a: A): Option[Int]
  def stringValue(a: A): Option[String]
  def booleanValue(a: A): Boolean

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
      val name = newTargetCtx.loop(prop.name).value.flatMap(stringValue)
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
