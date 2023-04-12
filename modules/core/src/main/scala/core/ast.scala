package com.filippodeluca.jsonpath
package core

import cats.parse._
import cats.syntax.all._

object ast {

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
  case class ArraySlice(start: Exp, end: Exp, step: Exp, parent: Exp) extends Exp

  case class ArrayIndex(index: Exp, parent: Exp) extends Exp

  /** @see
    *   https://cburgmer.github.io/json-path-comparison/results/root.html
    */
  case object Root extends Exp

  case object This extends Exp

  case class Property(name: Exp, parent: Exp) extends Exp

  case object NullLiteral extends Exp

  case class StringLiteral(value: String) extends Exp

  case class NumberLiteral(value: Double) extends Exp

  case class BooleanLiteral(value: Boolean) extends Exp

  case class Filter(predicate: Exp, parent: Exp) extends Exp

  // *** Operator (shall them be not an expresion or a different one like filter?)
  case class Eq(left: Exp, right: Exp) extends Exp

  // TODO Implement Union
  // case class Union(exps: List[Exp])
}
