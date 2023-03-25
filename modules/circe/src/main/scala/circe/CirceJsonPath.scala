package com.filippodeluca.jsonpath
package circe

import cats.parse.Parser

import io.circe.Json
import com.filippodeluca.jsonpath.core.{
  JsonPath,
  Interpreter,
  JsonPathParser,
  Matcher,
  ExpressionContext
}

object CirceJsonPath extends JsonPath[Json] {

  import Interpreter._

  private val parser = new JsonPathParser(CirceInterpreter)

  case class JsonMatcher(expression: Exp[Json]) extends Matcher[Json] {
    def apply(dv: Json): Json = {
      val ctx = ExpressionContext.single(dv, dv)
      val result = expression(ctx)
      result.fold(
        s => s.current,
        m => Json.fromValues(m.currents)
      )
    }
  }

  def parse(expression: String): Either[Parser.Error, Matcher[Json]] =
    parser.pathP.parseAll(expression).map(JsonMatcher(_))

}
