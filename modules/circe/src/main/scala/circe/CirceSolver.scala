package circe

import cats.parse._
import cats.syntax.all._

import io.circe.Json

import com.filippodeluca.jsonpath.core._
import com.filippodeluca.jsonpath.core.ast._

object CirceSolver {

  case class Context(values: Vector[Json]) {
    def value: Option[Json] = if (values.size == 1) {
      values.headOption
    } else {
      None
    }
  }

  object Context {
    def one(value: Json) = Context(Vector(value))
  }

  def solve(exp: ast.Exp, source: Json): Vector[Json] = {

    // $.foo.bar

    // solve(bar, solve("foo", solve($, current)))

    def stringValue(json: Json): Option[String] = {
      json.asString
        .orElse(json.asNumber.map(_.toDouble.toString))
        .orElse(json.asBoolean.map(_.toString))
    }

    def intValue(json: Json): Option[Int] = {
      json.asNumber.flatMap(_.toInt)
    }

    // According to https://www.sitepoint.com/javascript-truthy-falsy/
    def booleanValue(json: Json): Boolean = {
      val isFalse = json.fold[Boolean](
        true,
        _ == false,
        jsn => jsn.toBigDecimal.exists(_ == BigDecimal(0)) || jsn.toDouble == 0d,
        _.isEmpty,
        _ => false,
        _ => false
      )
      !isFalse
    }

    def loop(exp: ast.Exp, current: Context, root: Json): Context = exp match {
      case NullLiteral => Context.one(Json.Null)
      case StringLiteral(value) => Context.one(Json.fromString(value))
      case BooleanLiteral(value) => Context.one(Json.fromBoolean(value))
      case NumberLiteral(value) =>
        Context.one(Json.fromDouble(value).getOrElse(Json.fromBigDecimal(BigDecimal(value))))
      case This => current
      case Root => Context.one(root)
      case Property(nameExp, parent) =>
        val targetCtx = loop(parent, current, root)
        Context(targetCtx.values.mapFilter { target =>
          // TODO Fail if the value is an array
          val name = loop(nameExp, Context.one(target), root).value.flatMap(stringValue)
          name
            .flatMap { name =>
              target.asObject.flatMap(obj => obj(name))
            }
        })

      case ArrayIndex(indexExp, parentExp) =>
        val targetCtx = loop(parentExp, current, root)
        Context(targetCtx.values.mapFilter { target =>
          // TODO Fail if the value is an array
          val index = loop(indexExp, Context.one(target), root).value.flatMap(intValue)
          index
            .flatMap { index =>
              target.asArray
                .flatMap { arr =>
                  arr.get(index)
                }
            }

        })

      case ArraySlice(startExp, endExp, stepExp, targetExp) =>
        val targetCtx = loop(targetExp, current, root)
        Context(targetCtx.values.mapFilter { target =>
          target.asArray.map { array =>
            val targetCtx = Context.one(target)
            val start = loop(startExp, targetCtx, root).value.flatMap(intValue)
            val end = loop(endExp, targetCtx, root).value.flatMap(intValue)
            val step = loop(stepExp, targetCtx, root).value.flatMap(intValue).getOrElse(1)

            val range = if (step > 0) {
              start.map(x => if (x < 0) array.size + x else x).getOrElse(0) until end
                .map(x => if (x < 0) array.size + x else x)
                .getOrElse(
                  array.length
                ) by step
            } else {
              (start.map(x => if(x < 0) array.size + x else x ).getOrElse(array.size)) until end
                .map(x => if (x < 0) array.size + x else x)
                .getOrElse(-1) by step
            }

            Console.err.println(
              s"start: ${start}, end: ${end}, step: ${step}, range: ${range.toVector}"
            )

            Json.fromValues(range.toVector.mapFilter { idx =>
              val value = array.get(idx)

              value
            })
          }
        })

      case Filter(predicate, parent) =>
        val targetCtx = loop(parent, current, root)
        Context(targetCtx.values.flatMap { target =>
          target.asArray.fold {
            // TODO Fail if the value is an array
            val predicateValue =
              loop(predicate, Context.one(target), root).value.map(booleanValue).getOrElse(false)
            if (predicateValue) {
              Vector(target)
            } else {
              Vector.empty
            }
          } { targets =>
            targets.filter { item =>
              // TODO Fail if the value is an array
              loop(predicate, Context.one(item), root).value.map(booleanValue).getOrElse(false)
            }
          }
        })

      case Eq(leftExp, rightExp) =>
        Context.one((for {
          left <- loop(leftExp, current, root).value
          right <- loop(rightExp, current, root).value
        } yield left == right).fold(Json.False)(Json.fromBoolean))
    }

    loop(exp, Context.one(source), source).values
  }
}
