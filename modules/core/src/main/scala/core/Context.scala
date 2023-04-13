package com.filippodeluca.jsonpath
package core

import cats.parse.*
import cats.syntax.all.*

sealed abstract trait ExpressionContext[T] {
  def root: T

  def withCurrent(value: T): ExpressionContext[T] =
    ExpressionContext.single(value, root)

  def withCurrent(values: List[T]): ExpressionContext[T] =
    ExpressionContext.multiple(values, root)

  def single: Option[ExpressionContext.Single[T]] = fold(
    s => s.some,
    _ => none
  )

  def unsafeSingle: ExpressionContext.Single[T] = single.get

  def multiple: Option[ExpressionContext.Multiple[T]] = fold(
    _ => none,
    m => m.some
  )

  def unsafeMultiple: ExpressionContext.Multiple[T] = multiple.get

  // TODO It should flatten as well
  def asListOfSingle: List[ExpressionContext.Single[T]] = fold(
    List(_),
    m => m.currents.map(current => ExpressionContext.Single(current = current, root = m.root))
  )

  def fold[A](
      onSingle: ExpressionContext.Single[T] => A,
      onMultiple: ExpressionContext.Multiple[T] => A
  ) = this match {
    case s: ExpressionContext.Single[T] => onSingle(s)
    case m: ExpressionContext.Multiple[T] => onMultiple(m)
  }
}

object ExpressionContext {
  case class Single[T](current: T, root: T) extends ExpressionContext[T]

  case class Multiple[T](currents: List[T], root: T) extends ExpressionContext[T]

  def single[T](current: T, root: T): ExpressionContext[T] =
    Single(current, root)

  def multiple[T](
      current: List[T],
      root: T
  ): ExpressionContext[T] = Multiple(current, root)
}
