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

  case class Context(values: Vector[Any]) {
    // Returns Some only if there is only one result in the result list otherwise None
    def value: Option[Any] = if (values.size == 1) {
      values.headOption
    } else {
      None
    }
  }

  object Context {
    def one(value: Any) = Context(Vector(value))
  }

  def solve(exp: Exp, source: Any): Vector[Any] = {

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

    def loop(exp: Exp, current: Context, root: Any): Context = exp match {
      case NullLiteral => Context.one(None)
      case StringLiteral(value) => Context.one(value)
      case BooleanLiteral(value) => Context.one(value)
      case NumberLiteral(value) => Context.one(value)
      case This => current
      case Root => Context.one(root)

      case Property(nameExp, target) =>
        val targetCtx = loop(target, current, root)
        val results = targetCtx.values.mapFilter { target =>
          // TODO Should id fail if the value is an array?
          val name = loop(nameExp, Context.one(target), root).value.flatMap(stringValue)
          name
            .flatMap { name =>
              target match {
                case obj: Map[String, _] => obj.get(name)
                case _ => None
              }
            }
        }
        Context(results)
      case Wildcard(target) =>
        val results = loop(target, current, root).values.mapFilter { target =>
          target match {
            case seq: Seq[_] => seq.some
            case obj: Map[_, _] => obj.values.toVector.some
            case _ => None
          }
        }.flatten

        Context(results)

      case ArrayIndex(indexExp, targetExp) =>
        val targetCtx = loop(targetExp, current, root)
        val results = targetCtx.values.mapFilter { target =>
          // TODO Should id fail if the value is an array?
          val index = loop(indexExp, Context.one(target), root).value.flatMap(intValue)
          index
            .flatMap { index =>
              target match {
                // TODO: CirceSolver used index.toLong when getting, why?
                case seq: Seq[_] =>
                  if (index < 0) {
                    seq.get((seq.length + index))
                  } else {
                    seq.get(index)
                  }
                case _ => None
              }
            }

        }
        Context(results)

      case ArraySlice(startExp, endExp, stepExp, targetExp) =>
        val targetCtx = loop(targetExp, current, root)
        val results = targetCtx.values.map { target =>
          target match {
            case seq: Seq[Any] => {
              val targetCtx = Context.one(target)
              val start = loop(startExp, targetCtx, root).value.flatMap(intValue)
              val end = loop(endExp, targetCtx, root).value.flatMap(intValue)
              val step = loop(stepExp, targetCtx, root).value.flatMap(intValue).getOrElse(1)

              val range = if (step > 0) {
                start.map(x => if (x < 0) seq.size + x else x).getOrElse(0) until end
                  .map(x => if (x < 0) seq.size + x else x)
                  .getOrElse(
                    seq.length
                  ) by step
              } else {
                (start.map(x => if (x < 0) seq.size + x else x).getOrElse(seq.size)) until end
                  .map(x => if (x < 0) seq.size + x else x)
                  .getOrElse(-1) by step
              }

              Console.err.println(
                s"start: ${start}, end: ${end}, step: ${step}, range: ${range.toVector}"
              )

              range.toVector.mapFilter { idx =>
                seq.get(idx.toLong)
              }
            }
            case _ => Vector.empty[Any]
          }
        }.flatten

        Context(results)

      case Filter(predicate, target) =>
        val targetCtx = loop(target, current, root)
        val results = targetCtx.values.flatMap { target =>
          val seq: Seq[Any] = target match {
            case seq: Seq[Any] => seq
            case _ => {
              // TODO Should id fail if the value is an array?
              val predicateValue =
                loop(predicate, Context.one(target), root).value.map(booleanValue).getOrElse(false)
              if (predicateValue) {
                Vector(target)
              } else {
                Vector.empty
              }
            }
          }

          seq.filter { item =>
            // TODO Should id fail if the value is an array?
            loop(predicate, Context.one(item), root).value.map(booleanValue).getOrElse(false)
          }
        }
        Context(results)

      // TODO think about associativity and how to handle operators on multiple results

      case Eq(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
        } yield left == right

        Context.one(result.getOrElse(false))

      case Gt(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
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

        Context.one(result.getOrElse(false))

      case Gte(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
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

        Context.one(result.getOrElse(false))

      case Lt(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
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

        Context.one(result.getOrElse(false))

      case Lte(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
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

        Context.one(result.getOrElse(false))

      case Not(exp) =>
        val result = for {
          value <- loop(exp, current, root).value.map(booleanValue)
        } yield !value

        Context(result.toVector)

      case Or(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value.map(booleanValue)
          right <- loop(rightExp, current, root).value.map(booleanValue)
        } yield left || right

        Context.one(result.getOrElse(false))

      case And(leftExp, rightExp) =>
        val result = for {
          left <- loop(leftExp, current, root).value.map(booleanValue)
          right <- loop(rightExp, current, root).value.map(booleanValue)
        } yield left && right

        Context.one(result.getOrElse(false))

      case In(itemExp, setExp) =>
        val result = for {
          set <- loop(setExp, current, root).value
          item <- loop(itemExp, current, root).value
        } yield {
          set match {
            case seq: Seq[Any] => seq.contains(item)
            case obj: Map[Any, Any] => obj.contains(item)
            case _ => false
          }
        }

        Context(result.toVector)

      case Plus(lExp, rExp) =>
        val result = for {
          r <- loop(rExp, current, root).value
          l <- loop(lExp, current, root).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln + rn)
        } yield result
        Context(result.toVector)

      case Minus(lExp, rExp) =>
        val result = for {
          r <- loop(rExp, current, root).value
          l <- loop(lExp, current, root).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln - rn)
        } yield result
        Context(result.toVector)

      case Times(lExp, rExp) =>
        val result = for {
          r <- loop(rExp, current, root).value
          l <- loop(lExp, current, root).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln * rn)
        } yield result
        Context(result.toVector)

      case DividedBy(lExp, rExp) =>
        val result = for {
          r <- loop(rExp, current, root).value
          l <- loop(lExp, current, root).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln / rn)
        } yield result
        Context(result.toVector)

      case Modulo(lExp, rExp) =>
        val result = for {
          r <- loop(rExp, current, root).value
          l <- loop(lExp, current, root).value
          rn <- toDouble(r)
          ln <- toDouble(l)
          result <- Some(ln % rn)
        } yield result
        Context(result.toVector)

      case Union(exps) =>
        Context(exps.flatMap(exp => loop(exp, current, root).values))
    }

    loop(exp, Context.one(source), source).values
  }
}
