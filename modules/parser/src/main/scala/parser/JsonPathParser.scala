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
package parser

import cats.parse.*
import cats.syntax.all.*

class JsonPathParser {

  val dotP: Parser[Unit] = Parser.char('.')
  val openSquareBraceP: Parser[Unit] = Parser.char('[')
  val closeSquareBraceP: Parser[Unit] = Parser.char(']')
  val questionMarkP: Parser[Unit] = Parser.char('?')
  val starP: Parser[Unit] = Parser.char('*')
  val equalP: Parser[Unit] = Parser.char('=')
  val bangP: Parser[Unit] = Parser.char('!')
  val gtP = Parser.char('>')
  val ltP = Parser.char('<')
  val ampersandP = Parser.char('&')
  val pipeP = Parser.char('|')
  val dollarP = Parser.char('$')
  val atP = Parser.char('@')
  val columnP: Parser[Unit] = Parser.char(':')
  val openParenP: Parser[Unit] = Parser.char('(')
  val closeParenP: Parser[Unit] = Parser.char(')')
  val singleQuoteP: Parser[Unit] = Parser.char('\'')
  val whitespaceP: Parser[Unit] = Parser.charIn(' ', '\t').void
  val whitespacesP: Parser[Unit] = whitespaceP.rep(1).void

  val `[(` : Parser[Unit] =
    openSquareBraceP *> whitespacesP.rep0 *> openParenP
  val `[?(` =
    openSquareBraceP *> whitespacesP.rep0 *> questionMarkP *> whitespacesP.rep0 *> openParenP
  val `)]` = closeParenP *> whitespacesP.rep0 *> closeSquareBraceP

  // val equalOpP: Parser[Op[T]] =
  //   (equalP *> equalP).withContext("equalOpP").as(interpreter.equalOp)
  // val notEqualP: Parser[Op[T]] =
  //   (bangP *> equalP).withContext("notEqualP").as(interpreter.notEqualOp)
  // val andOpP: Parser[Op[T]] =
  //   (ampersandP *> ampersandP).withContext("andOpP").as(interpreter.andOp)
  // val orOpP: Parser[Op[T]] = (pipeP *> pipeP).withContext("orOpP").as(interpreter.orOp)

  // TODO implement these operators
  // val gtOpP: Parser[Op[T]] = gtP
  // val ltOpP: Parser[Op[T]] = ltP
  // val geOp: Parser[Op[T]] = gtP *> equalP
  // val leOp: Parser[Op[T]] = ltP *> equalP
  // val subsetofOpP: Parser[Op[T]] = Parser.string("subsetof")
  // val containsOpP: Parser[Op[T]] = Parser.string("contains")
  // val regexOpP: Parser[Op[T]] = Parser.string("~=")

  // val inOpP: Parser[Op[T]] = Parser.string("in").withContext("inOpP")
  //   .as(interpreter.inOp)
  // val ninOpP: Parser[Op[T]] =
  //   Parser.string("nin").withContext("ninOpP").as(interpreter.ninOp)

  // val sizeOpP: Parser[Op[T]] =
  //   Parser.string("size").withContext("sizeOpP").as(interpreter.sizeOp)

  // val emptyOpP: Parser[Op[T]] =
  //   Parser.string("empty").withContext("emptyOpP").as(interpreter.emptyOp)

  // val opP = Parser
  //   .oneOf(List(equalOpP, notEqualP, sizeOpP, emptyOpP, andOpP, orOpP))
  //   .withContext("opP")

  // val notP = bangP.as(interpreter.notOp).withContext("notOp")
  // val unaryOpP: Parser[UnaryOp[T]] = Parser.oneOf(notP :: Nil)

  // val intP = (Parser.char('0').as(0) | (Parser
  //   .charWhere(c => c.isDigit && c != '0') ~ Parser
  //   .charWhere(c => c.isDigit)
  //   .rep0)
  //   .map { case (head, tail) =>
  //     (head :: tail).toList.mkString.toInt
  //   })
  //   .withContext("intP")

  // val segmentP: Parser[String] =
  //   (Parser.charWhere(c => c.isLetter | c == '_' | c == '-' | c == '&') ~ Parser
  //     .charWhere(c => c.isLetterOrDigit | c == '_' | c == '-' | c == '&')
  //     .rep0)
  //     .map { case (h, tail) =>
  //       (h :: tail).mkString
  //     }
  //     .withContext("segmentP")

  // val stringP = Parser
  //   .until(singleQuoteP)
  //   .surroundedBy(singleQuoteP)
  //   .withContext("stringP")

  // val booleanP =
  //   (Parser.string("true").as(true) | Parser.string("false").as(false))
  //     .withContext("booleanP")

  // lazy val thisP: Parser[Exp[T]] = atP.as(interpreter.thisExp)

  // lazy val rootP: Parser[Exp[T]] = dollarP.as(interpreter.rootExp)

  // lazy val pathP = ((thisP | rootP) ~ Parser
  //   .recursive[Exp[T]] { p =>
  //     (Parser.oneOf(
  //       List(
  //         (dotP *> segmentP).map(interpreter.propertyExp).withContext(".property"),
  //         openSquareBraceP *> (
  //           Parser.oneOf(
  //             // TODO add *
  //             // TODO add ..property
  //             // TODO add [index1,index2,…]
  //             // TODO add [start:]
  //             // TODO add [start:end]
  //             // TODO add [:n]
  //             // TODO add [-n:]
  //             List(
  //               stringP.map(interpreter.propertyExp).withContext("['property']"),
  //               intP.map(interpreter.nThArrayItemExp).withContext("[num]"),
  //               (questionMarkP *> openParenP *> whitespaceP.rep0 *> Parser
  //                 .defer(expP)
  //                 .map(interpreter.filterExp) <* whitespaceP.rep0 <* closeParenP)
  //                 .withContext("[?(filter)]"),
  //               Parser.defer(expP).map(interpreter.propertyExp).withContext("[(exp)]")
  //             )
  //           )
  //         ) <* closeSquareBraceP
  //       )
  //     ) ~ p.rep0).map { case (h, tail) =>
  //       tail.foldLeft(h) { (s, x) => s.andThen(x) }
  //     }
  //   }
  //   .?).map {
  //   case (h, Some(t)) => h.andThen(t)
  //   case (h, None) => h
  // }

  // lazy val expP: Parser[Exp[T]] = Parser.recursive[Exp[T]] { recurse =>
  //   val intExpP = intP
  //     .withContext("intP")
  //     .map(_.toDouble)
  //     .map(interpreter.numberExp)

  //   val stringExpP = stringP
  //     .withContext("stringP")
  //     .map(interpreter.stringExp)

  //   val booleanExpP =
  //     booleanP
  //       .withContext("booleanP")
  //       .map(interpreter.booleanExp)

  //   val literalExp = Parser.oneOf(intExpP :: stringExpP :: booleanExpP :: Nil)

  //   val parentsExpP =
  //     (openParenP *> whitespaceP.rep0) *> recurse <* (whitespaceP.rep0 <* closeParenP)
  //       .withContext("parentsExpP")

  //   val unaryOpExpP =
  //     ((unaryOpP <* whitespacesP.rep0) ~ (parentsExpP | pathP | literalExp))
  //       .withContext("unaryOpExpP")
  //       .map { case (op, value) =>
  //         interpreter.unaryOpExp(op, value)
  //       }

  //   val opExpP =
  //     ((unaryOpExpP | parentsExpP | pathP | literalExp) ~ (whitespacesP *> opP <* whitespacesP) ~ recurse)
  //       .withContext("opExpP")
  //       .map { case ((l, op), r) =>
  //         interpreter.opExp(op, l, r)
  //       }

  //   Parser.oneOf(
  //     opExpP.backtrack :: unaryOpExpP :: parentsExpP :: pathP :: literalExp :: Nil
  //   )
  // }
}
