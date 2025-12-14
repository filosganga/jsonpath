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

  implicit def show[T <: Exp]: Show[T] = {
    def showExp(exp: Exp): String = exp match {
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
      case Plus(l, r) => show"$l + $r"
      case Minus(l, r) => show"$l - $r"
      case Times(l, r) => show"$l * $r"
      case DividedBy(l, r) => show"$l / $r"
      case Modulo(l, r) => show"$l % $r"
      case Union(exps) => s"[${exps.map(showExp).mkString(",")}]"
    }
    Show.show(showExp)
  }

}

sealed trait Exp

// *** Literals
// TODO Shall the literals be in Literal hierarchy?
case object NullLiteral extends Exp

case class StringLiteral(value: String) extends Exp

case class NumberLiteral(value: Double) extends Exp

case class BooleanLiteral(value: Boolean) extends Exp

// *** Selectors
case object Root extends Exp

case object This extends Exp

case class Property(name: Exp, target: Exp) extends Exp

case class Wildcard(target: Exp) extends Exp

case class ArraySlice(start: Exp, end: Exp, step: Exp, target: Exp) extends Exp

case class ArrayIndex(index: Exp, target: Exp) extends Exp

case class Filter(predicate: Exp, target: Exp) extends Exp

// TODO Implement Union
case class Union(exps: Vector[Exp]) extends Exp

// TODO Implement ...dunno the name nbiut this: {a: @.foo.bar, b: @.bar.foo}
// Is the key an expression as well? Why no? {($.a.b): @.foo.bar, ('b'): @.bar.foo}
// case class NoName(Map[Exp, Exp])

// *** Operators
// TODO Shall the operator be not an expresion or perhaps a different one?
case class Eq(left: Exp, right: Exp) extends Exp

case class Gt(left: Exp, right: Exp) extends Exp

case class Gte(left: Exp, right: Exp) extends Exp

case class Lt(left: Exp, right: Exp) extends Exp

case class Lte(left: Exp, right: Exp) extends Exp

case class Not(exp: Exp) extends Exp

case class And(left: Exp, right: Exp) extends Exp

case class Or(left: Exp, right: Exp) extends Exp

case class In(item: Exp, set: Exp) extends Exp

case class Plus(l: Exp, r: Exp) extends Exp

case class Minus(l: Exp, r: Exp) extends Exp

case class Times(l: Exp, r: Exp) extends Exp

case class DividedBy(l: Exp, r: Exp) extends Exp

case class Modulo(l: Exp, r: Exp) extends Exp

// TODO Add function: max(@.foo, @.bar)
// Should the name be an expression? In the AST probably yes, not in the syntax
// case class Function(name: Exp, targets: Vector[Exp]) extends Exp
