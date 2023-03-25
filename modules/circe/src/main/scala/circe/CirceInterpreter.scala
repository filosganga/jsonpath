package com.filippodeluca.jsonpath
package circe

import cats.parse._
import cats.syntax.all._

import io.circe.Json

import com.filippodeluca.jsonpath.core._

object CirceInterpreter extends Interpreter[Json] {

  import Interpreter._

  val inOp: Op[Json] = (l, r) => {
    val result = List(
      r.asArray.exists(_.contains(l)),
      r.asObject.exists(jso => l.asString.exists(key => jso.contains(key)))
    ).reduce(_ || _)

    Json.fromBoolean(result)
  }

  val ninOp: Op[Json] = (l, r) => Json.fromBoolean(!toBoolean(inOp(l, r)))

  val equalOp: Op[Json] = (l, r) => Json.fromBoolean(l == r)

  val notEqualOp: Op[Json] = (l, r) => Json.fromBoolean(l != r)

  val sizeOp: Op[Json] = (l, r) => {
    val result = r.asNumber
      .flatMap(_.toInt)
      .map { size =>
        List(
          l.asString.exists(_.size == size),
          l.asArray.exists(_.size == size),
          l.asObject.exists(_.size == size)
        ).reduce(_ || _)
      }
      .getOrElse(false)

    Json.fromBoolean(result)
  }

  // TODO If a property is null, dos it count as empty?
  val emptyOp: Op[Json] = (l, r) => {
    val expected = toBoolean(r)
    val result = List(
      l.asString.exists(_.isEmpty),
      l.asArray.exists(_.isEmpty),
      l.asObject.exists(_.isEmpty)
    ).reduce(_ || _)

    Json.fromBoolean(result == expected)
  }

  val andOp: Op[Json] = (l, r) => Json.fromBoolean(toBoolean(l) && toBoolean(r))
  val orOp: Op[Json] = (l, r) => Json.fromBoolean(toBoolean(l) || toBoolean(r))
  val notOp: UnaryOp[Json] = value => Json.fromBoolean(!toBoolean(value))

  def nThArrayItemExp(index: Int): Exp[Json] =
    propertyExp(ctx => ctx.withCurrent(Json.fromInt(index)))

  def propertyExp(name: String): Exp[Json] =
    propertyExp(ctx => ctx.withCurrent(Json.fromString(name)))

  def propertyExp(exp: Exp[Json]): Exp[Json] = ctx => {

    def singleCase(sCtx: ExpressionContext.Single[Json]) = {
      val value = exp(ctx)

      val onArray = (
        sCtx.current.asArray,
        value.single
          .flatMap(_.current.asNumber)
          .flatMap(x => x.toLong)
      ).mapN { (xs, index) =>
        xs.get(index).getOrElse(Json.Null)
      }

      val onObject =
        (sCtx.current.asObject, value.single.flatMap(_.current.asString)).mapN { (m, key) =>
          m(key).getOrElse(Json.Null)
        }

      onObject
        .orElse(onArray)
        .map(ctx.withCurrent)
        .getOrElse(ctx.withCurrent(Json.Null))
    }

    def multipleCase(mCtx: ExpressionContext.Multiple[Json]) = {
      mCtx.withCurrent(
        mCtx.asListOfSingle
          .map(x => singleCase(x).single)
          .flatten
          .map(_.current)
      )
    }

    ctx.fold(
      singleCase,
      multipleCase
    )

  }

  def filterExp(exp: Exp[Json]): Exp[Json] = ctx => {

    def singleCase(ctx: ExpressionContext.Single[Json]): ExpressionContext[Json] = {

      ctx.current.asArray
        .map { values =>
          val filteredValues = values.filter { x =>
            val sample = exp(ctx.withCurrent(x))
            toBoolean(sample.unsafeSingle.current)
          }

          ctx.withCurrent(filteredValues.toList)
        }
        .getOrElse {
          if (toBoolean(exp(ctx).unsafeSingle.current)) {
            ctx
          } else {
            ctx.withCurrent(Json.Null)
          }
        }
    }

    def multipleCase(ctx: ExpressionContext.Multiple[Json]): ExpressionContext[Json] = {
      val values = ctx.currents
        .flatMap { x =>
          singleCase(
            ExpressionContext.Single(current = x, root = ctx.root)
          ).asListOfSingle
        }
        .map(_.current)

      ctx.withCurrent(values)

    }

    ctx.fold(
      singleCase,
      multipleCase
    )
  }

  def opExp(op: Op[Json], l: Exp[Json], r: Exp[Json]): Exp[Json] = { ctx =>
    ctx.withCurrent(
      op(l(ctx).unsafeSingle.current, r(ctx).unsafeSingle.current)
    )
  }

  def unaryOpExp(op: UnaryOp[Json], l: Exp[Json]): Exp[Json] = { ctx =>
    ctx.withCurrent(
      op(l(ctx).unsafeSingle.current)
    )
  }

  def numberExp(value: Double): Exp[Json] = ctx =>
    ctx.withCurrent(Json.fromDouble(value).getOrElse(throw new RuntimeException)) // TODO

  def booleanExp(value: Boolean): Exp[Json] = ctx => ctx.withCurrent(Json.fromBoolean(value))

  def stringExp(value: String): Exp[Json] = ctx => ctx.withCurrent(Json.fromString(value))

  // According to https://www.sitepoint.com/javascript-truthy-falsy/
  def toBoolean(json: Json) = {
    val isFalse = List(
      json.isNull,
      json.asBoolean.exists(_ == false),
      json.asString.exists(_.isEmpty),
      json.asNumber.exists(n => n.toBigDecimal.exists(_ === BigDecimal(0)))
    ).reduce(_ || _)

    !isFalse
  }

}
