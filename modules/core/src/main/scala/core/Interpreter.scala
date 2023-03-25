package com.filippodeluca.jsonpath
package core

import cats.parse._
import cats.syntax.all._

import Interpreter._

object Interpreter {
  type Exp[T] = ExpressionContext[T] => ExpressionContext[T]
  type Op[T] = (T, T) => T
  type UnaryOp[T] = (T) => T
}

trait Interpreter[T] {
  def inOp: Op[T]
  def ninOp: Op[T]
  def equalOp: Op[T]
  def notEqualOp: Op[T]
  def sizeOp: Op[T]
  def emptyOp: Op[T]

  def andOp: Op[T]
  val orOp: Op[T]
  val notOp: UnaryOp[T]

  def nThArrayItemExp(index: Int): Exp[T]
  def propertyExp(name: String): Exp[T]
  def propertyExp(exp: Exp[T]): Exp[T]

  def filterExp(exp: Exp[T]): Exp[T]

  def opExp(op: Op[T], l: Exp[T], r: Exp[T]): Exp[T]

  def unaryOpExp(op: UnaryOp[T], l: Exp[T]): Exp[T]

  def thisExp: Exp[T] = identity

  def rootExp: Exp[T] = ctx => ctx.withCurrent(ctx.root)

  def numberExp(value: Double): Exp[T]

  def booleanExp(value: Boolean): Exp[T]

  def stringExp(value: String): Exp[T]
}
