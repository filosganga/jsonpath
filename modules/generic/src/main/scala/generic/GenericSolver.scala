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

  def toDouble(any: Any): Option[Double] = {
    any match {
      case n: Int => n.toDouble.some
      case n: Long => n.toDouble.some
      case n: Double => n.some
      case n: BigDecimal => n.toDouble.some
      case n: BigInt => n.toDouble.some
      case _ => None
    }
  }

  case class Context(values: Vector[Any], root: Any) extends Ctx[Context, Any] {
    // Returns Some only if there is only one result in the result list otherwise None
    def value: Option[Any] = if (values.size == 1) {
      values.headOption
    } else {
      None
    }

    def many(values: Vector[Any], root: Any) = Context(values, root)
    def one(value: Any, root: Any) = Context.one(value, root)

    def sequenceValue(any: Any): Option[Seq[Any]] = any match {
      case seq: Seq[Any] => seq.some
      case _ => None
    }
    def sequenceToValue(seq: Seq[Any]): Any = seq.asInstanceOf[Any]

    def mapValue(any: Any): Option[Map[String, Any]] = any match {
      case map: Map[String, Any] => map.some
      case _ => None
    }

    def stringValue(any: Any): Option[String] = any.toString.some

    def intValue(any: Any): Option[Int] = any match {
      case n: Int => n.toInt.some
      case n: Long => n.toInt.some
      case n: Double => n.toInt.some
      case n: BigDecimal => n.toInt.some
      case n: BigInt => n.toInt.some
      case _ => None
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

    override def loop(exp: Exp): Context = exp match {
      case NullLiteral => Context.one(None, root)
      case StringLiteral(value) => Context.one(value, root)
      case BooleanLiteral(value) => Context.one(value, root)
      case NumberLiteral(value) => Context.one(value, root)
      case This => this
      case Root => Context.one(root, root)

      case prop: Property => getProperty(prop)
      case Wildcard(target) =>
        val results = loop(target).values.mapFilter { target =>
          target match {
            case seq: Seq[_] => seq.some
            case obj: Map[_, _] => obj.values.toVector.some
            case _ => None
          }
        }.flatten

        Context(results, root)

      case idx: ArrayIndex => getArrayIndex(idx)
      case slice: ArraySlice => sliceSequence(slice)

      case Filter(predicate, target) =>
        val targetCtx = loop(target)
        val results = targetCtx.values.flatMap { target =>
          val seq: Seq[Any] = target match {
            case seq: Seq[Any] => seq
            case _ => {
              // TODO Should id fail if the value is an array?
              val newTargetCtx = Context.one(target, root)
              val predicateValue = newTargetCtx
                .loop(predicate)
                .value
                .map(booleanValue)
                .getOrElse(false)
              if (predicateValue) {
                Vector(target)
              } else {
                Vector.empty
              }
            }
          }

          seq.filter { item =>
            // TODO Should id fail if the value is an array?
            val itemCtx = Context.one(item, root)
            itemCtx.loop(predicate).value.map(booleanValue).getOrElse(false)
          }
        }
        Context(results, root)

      // TODO think about associativity and how to handle operators on multiple results

      case Eq(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield left == right

        Context.one(result.getOrElse(false), root)

      case Gt(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield {
          (left, right) match {
            case (l: String, r: String) => l > r
            case (left, right) =>
              (toDouble(left), toDouble(right)) match {
                case (Some(l), Some(r)) => l > r
                case _ => false
              }
          }
        }

        Context.one(result.getOrElse(false), root)

      case Gte(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield {
          (left, right) match {
            case (l: String, r: String) => l >= r
            case (left, right) =>
              (toDouble(left), toDouble(right)) match {
                case (Some(l), Some(r)) => l >= r
                case _ => false
              }
          }
        }

        Context.one(result.getOrElse(false), root)

      case Lt(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield {
          (left, right) match {
            case (l: String, r: String) => l < r
            case (left, right) =>
              (toDouble(left), toDouble(right)) match {
                case (Some(l), Some(r)) => l < r
                case _ => false
              }
          }
        }

        Context.one(result.getOrElse(false), root)

      case Lte(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value
          right <- loop(rightExp).value
        } yield {
          (left, right) match {
            case (l: String, r: String) => l <= r
            case (left, right) =>
              (toDouble(left), toDouble(right)) match {
                case (Some(l), Some(r)) => l <= r
                case _ => false
              }
          }
        }

        Context.one(result.getOrElse(false), root)

      case Not(exp) =>
        val result = for {
          value <- loop(exp).value.map(booleanValue)
        } yield !value

        Context(result.toVector, root)

      case Or(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value.map(booleanValue)
          right <- loop(rightExp).value.map(booleanValue)
        } yield left || right

        Context.one(result.getOrElse(false), root)

      case And(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp).value.map(booleanValue)
          right <- loop(rightExp).value.map(booleanValue)
        } yield left && right

        Context.one(result.getOrElse(false), root)

      case In(itemExp, setExp) =>
        val result = for {
          set <- loop(setExp).value
          item <- loop(itemExp).value
        } yield {
          set match {
            case seq: Seq[Any] => seq.contains(item)
            case obj: Map[Any, Any] => obj.contains(item)
            case _ => false
          }
        }

        Context(result.toVector, root)

      case Plus(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln + rn)
        } yield result
        Context(result.toVector, root)

      case Minus(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln - rn)
        } yield result
        Context(result.toVector, root)

      case Times(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln * rn)
        } yield result
        Context(result.toVector, root)

      case DividedBy(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln / rn)
        } yield result
        Context(result.toVector, root)

      case Modulo(lExp, rExp) =>
        val result = for {
          r <- loop(rExp).value
          l <- loop(lExp).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln % rn)
        } yield result
        Context(result.toVector, root)

      case Union(exps) =>
        Context(exps.flatMap(exp => loop(exp).values), root)
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
