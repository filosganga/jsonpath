package com.filippodeluca.jsonpath
package core

import cats.parse.Parser

trait JsonPath[T] {

  def parse(expression: String): Either[Parser.Error, Matcher[T]]

  def unsafeParse(expression: String): Matcher[T] =
    parse(expression).fold(error => throw new RuntimeException(error.toString()), identity)

}
