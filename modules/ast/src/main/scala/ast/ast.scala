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
package ast

import cats.Show
import cats.syntax.all.*

object Exp {

  implicit def show[T <: Exp]: Show[T] = Show.show {
    case Root => "$"
    case This => "@"
    case NullLiteral => ""
    case StringLiteral(s) => "'" ++ s ++ "'"
    case BooleanLiteral(b) => b.toString
    case NumberLiteral(n) if n.isValidInt => n.toInt.toString
    case NumberLiteral(n) => n.toString
    case Property(StringLiteral(name), target) => show"${target}.${name}"
    case Property(name, target) => show"${target}[(${name})]"
    case Wildcard(target) => show"${target}.*"
    case ArrayIndex(index, target) => show"$target[$index]"
    case ArraySlice(start, end, step, target) => show"${target}[${start}:${end}:${step}]"
    case Filter(predicate, target) => show"$target[?($predicate)]"
    case Eq(left, right) => show"($left) == ($right)"
    case Gt(left, right) => show"($left) > ($right)"
    case Gte(left, right) => show"($left) >= ($right)"
    case Lt(left, right) => show"($left) < ($right)"
    case Lte(left, right) => show"($left) <= ($right)"
    case Not(exp) => show"!($exp)"
    case And(l, r) => show"$l && $r"
    case Or(l, r) => show"$l || $r"
    case In(item, set) => show"$item in $set"
  }

}

sealed trait Exp

/** $.foo[1:2] = ArraySlice(1,2, Property("foo", Root))
  *
  * TOOD how to model open ended?
  *
  * @see
  *   https://cburgmer.github.io/json-path-comparison/results/array_slice.html
  * @see
  *   https://cburgmer.github.io/json-path-comparison/results/array_slice_on_exact_match.html
  * @see
  *   https://cburgmer.github.io/json-path-comparison/results/array_slice_on_non_overlapping_array.html
  * @see
  *   https://cburgmer.github.io/json-path-comparison/results/array_slice_on_object.html
  * @see
  *   https://cburgmer.github.io/json-path-comparison/results/array_slice_with_open_start_and_end.html
  */
case class ArraySlice(start: Exp, end: Exp, step: Exp, target: Exp) extends Exp

case class ArrayIndex(index: Exp, target: Exp) extends Exp

/** @see
  *   https://cburgmer.github.io/json-path-comparison/results/root.html
  */
case object Root extends Exp

case object This extends Exp

case object NullLiteral extends Exp

case class StringLiteral(value: String) extends Exp

case class NumberLiteral(value: Double) extends Exp

case class BooleanLiteral(value: Boolean) extends Exp

case class Property(name: Exp, target: Exp) extends Exp

case class Wildcard(target: Exp) extends Exp

case class Filter(predicate: Exp, target: Exp) extends Exp

// *** Operator (shall them be not an expresion or a different one like filter?)
case class Eq(left: Exp, right: Exp) extends Exp

case class Gt(left: Exp, right: Exp) extends Exp

case class Gte(left: Exp, right: Exp) extends Exp

case class Lt(left: Exp, right: Exp) extends Exp

case class Lte(left: Exp, right: Exp) extends Exp

case class Not(exp: Exp) extends Exp

case class And(left: Exp, right: Exp) extends Exp

case class Or(left: Exp, right: Exp) extends Exp

case class In(item: Exp, set: Exp) extends Exp

// TODO Implement Union
// case class Union(exps: List[Exp])
