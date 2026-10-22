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
import com.filippodeluca.jsonpath.ast.*

object JsonPathParser {

  val dotP: Parser[Unit] = Parser.char('.')
  val commaP: Parser[Unit] = Parser.char(',')
  val openSquareBraceP: Parser[Unit] = Parser.char('[')
  val closeSquareBraceP: Parser[Unit] = Parser.char(']')
  val questionMarkP: Parser[Unit] = Parser.char('?')
  val starP: Parser[Unit] = Parser.char('*')
  val plusP = Parser.char('+')
  val slahsP = Parser.char('/')
  val percentP = Parser.char('%')
  val equalP: Parser[Unit] = Parser.char('=')
  val bangP: Parser[Unit] = Parser.char('!')
  val dashP: Parser[Unit] = Parser.char('-')
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
  val doubleQuoteP: Parser[Unit] = Parser.char('\"')
  val whitespaceP: Parser[Unit] = Parser.charIn(' ', '\t').void
  val whitespacesP: Parser[Unit] = whitespaceP.rep(1).void
  val whitespacesP0: Parser0[Unit] = whitespaceP.rep0.void

  val singleQuotedP: Parser[StringLiteral] =
    (singleQuoteP *> Parser.until(singleQuoteP) <* singleQuoteP).map(StringLiteral.apply)

  val doubleQuotedP: Parser[StringLiteral] =
    (doubleQuoteP *> Parser.until(doubleQuoteP) <* doubleQuoteP).map(StringLiteral.apply)

  val stringLiteralP: Parser[StringLiteral] = singleQuotedP

  val nameSelectorP: Parser[StringLiteral] = singleQuotedP.orElse(doubleQuotedP)

  val numberLiteralP: Parser[NumberLiteral] = {
    val zeroP = Parser.char('0').as(0)
    val intP: Parser[Int] = zeroP | (Parser
      .charWhere(c => c.isDigit && c != '0') ~ Parser
      .charWhere(c => c.isDigit)
      .rep0).map { case (h, t) =>
      (h :: t).mkString.toInt
    }

    (dashP.?.map(_.fold(1)(_ => -1)).with1 ~ intP ~ (dotP *> intP).?)
      .map {
        case ((factor, int), Some(decimal)) => s"$int.$decimal".toDouble * factor
        case ((factor, int), None) => int.toDouble * factor
      }
      .map(NumberLiteral.apply)
  }

  val booleanLiteralP: Parser[BooleanLiteral] =
    (Parser.string("true").as(true) | Parser.string("false").as(false)).map(BooleanLiteral.apply)

  val literalP: Parser[Exp] = stringLiteralP | booleanLiteralP | numberLiteralP

  val thisP: Parser[This.type] = atP.as(This)
  val rootP: Parser[Root.type] = dollarP.as(Root)

  val unaryOpP: Parser[Exp => Exp] = {
    val notOpP: Parser[Exp => Exp] = (bangP).as(exp => Not(exp))

    Parser.oneOf(notOpP :: Nil)
  }

  type BinOp = (Exp, Exp) => Exp

  val orOpP: Parser[BinOp] = (pipeP *> pipeP).void.as(Or(_, _))
  val andOpP: Parser[BinOp] = (ampersandP *> ampersandP).void.as(And(_, _))

  val comparisonOpP: Parser[BinOp] = {
    val eqOpP: Parser[BinOp] = (equalP *> equalP).void.as(Eq(_, _))
    val neqOpP: Parser[BinOp] = (bangP *> equalP).void.as({ (l, r) => Not(Eq(l, r)) })
    val gtOrGteOpP: Parser[BinOp] = (gtP *> equalP.?).map { opt =>
      opt.fold[BinOp](Gt.apply)(_ => Gte.apply)
    }
    val ltOrLteOpP: Parser[BinOp] = (ltP *> equalP.?).map { opt =>
      opt.fold[BinOp](Lt.apply)(_ => Lte.apply)
    }

    Parser.oneOf(List(eqOpP, neqOpP, gtOrGteOpP, ltOrLteOpP))
  }

  val additiveOpP: Parser[BinOp] = {
    val plusOpP: Parser[BinOp] = (plusP).void.as(Plus(_, _))
    val minusOpP: Parser[BinOp] = (dashP).void.as(Minus(_, _))

    Parser.oneOf(List(plusOpP, minusOpP))
  }

  val multiplicativeOpP: Parser[BinOp] = {
    val timesOpP: Parser[BinOp] = (starP).void.as(Times(_, _))
    val divideByOpP: Parser[BinOp] = (slahsP).void.as(DividedBy(_, _))
    val moduloOpP: Parser[BinOp] = (percentP).void.as(Modulo(_, _))

    Parser.oneOf(List(timesOpP, divideByOpP, moduloOpP))
  }

  // Backtracks so that trailing whitespace not followed by an operator is left unconsumed
  private def binOpP(opP: Parser[BinOp]): Parser[BinOp] =
    (whitespacesP0.with1 *> opP <* whitespacesP0).backtrack

  def leftAssocP(operandP: Parser[Exp], opP: Parser[BinOp]): Parser[Exp] =
    (operandP ~ (binOpP(opP) ~ operandP).rep0).map { case (head, tail) =>
      tail.foldLeft(head) { case (l, (op, r)) => op(l, r) }
    }

  def nonAssocP(operandP: Parser[Exp], opP: Parser[BinOp]): Parser[Exp] =
    (operandP ~ (binOpP(opP) ~ operandP).?).map {
      case (l, Some((op, r))) => op(l, r)
      case (l, None) => l
    }

  val expP: Parser[Exp] = {

    val parensExpP =
      (openParenP *> whitespaceP.rep0) *> Parser.defer(expP) <* (whitespaceP.rep0 <* closeParenP)
        .withContext(
          "parensExpP"
        )

    val bracketsP: Parser[Exp => Exp] = {

      val arraySliceP: Parser[Exp => Exp] = {
        val numberOrNull = numberLiteralP.?.map(_.getOrElse(NullLiteral))
        val sliceSep = columnP.surroundedBy(whitespacesP0)
        (numberOrNull.with1.soft ~ (sliceSep *> numberOrNull) ~ (sliceSep *> numberOrNull).?).map {
          case ((start, end), step) => ArraySlice(start, end, step.getOrElse(NullLiteral), _)
        }
      }.withContext("arraySliceP")

      val filterP: Parser[Exp => Exp] = {
        (questionMarkP.surroundedBy(whitespacesP0) *> parensExpP).map(filterExp =>
          Filter(filterExp, _)
        )
      }.withContext("filterP")

      (openSquareBraceP *> (
        Parser.oneOf(
          List(
            starP.as(Wildcard(_)),
            nameSelectorP.map(name => Property(name, _)),
            arraySliceP,
            filterP,
            numberLiteralP.map(index => ArrayIndex(index, _)),
            parensExpP.map(name => Property(name, _))
          )
        )
      ) <* closeSquareBraceP).withContext("bracketsP")
    }

    val rootOrThis: Parser[Exp] = (thisP | rootP)
    val dotPropertyP: Parser[Exp => Exp] = {

      val segmentP: Parser[String] =
        (Parser.charWhere(c => c.isLetter | c == '_' | c == '-' | c == '&') ~ Parser
          .charWhere(c => c.isLetterOrDigit | c == '_' | c == '-' | c == '&')
          .rep0)
          .map { case (h, tail) =>
            (h :: tail).mkString
          }
          .withContext("segmentP")

      (dotP *> (starP
        .as(Wildcard(_)) | segmentP.map(StringLiteral.apply).map(name => Property(name, _))))
    }.withContext("dotPropertyP")

    val selectorP: Parser[Exp] = (rootOrThis ~ (dotPropertyP | bracketsP).rep0)
      .map {
        case (target, xs) if xs.nonEmpty =>
          xs.reduceRight[Exp => Exp] { (l, r) => r.compose(l) }(target)
        case (target, _) =>
          target
      }
      .withContext("selectorP")

    val unionExpP =
      ((openSquareBraceP *> whitespacesP0 *> Parser.defer(
        expP
      )) ~ (commaP *> whitespacesP0 *> Parser
        .defer(expP)).rep0 <* (whitespacesP0 <* closeSquareBraceP))
        .map { case (head, tail) =>
          (head :: tail).toVector
        }
        .map(Union.apply)

    val primaryP: Parser[Exp] = Parser.oneOf(
      List(
        unionExpP,
        literalP,
        parensExpP,
        selectorP
      )
    )

    lazy val unaryP: Parser[Exp] = Parser.defer(
      (unaryOpP ~ (whitespacesP0 *> unaryP)).map { case (op, exp) => op(exp) } | primaryP
    )

    val multiplicativeP = leftAssocP(unaryP, multiplicativeOpP)
    val additiveP = leftAssocP(multiplicativeP, additiveOpP)
    val comparisonP = nonAssocP(additiveP, comparisonOpP)
    val andP = leftAssocP(comparisonP, andOpP)
    val orP = leftAssocP(andP, orOpP)

    orP.withContext("expP")
  }

  def parse(str: String) = expP.parseAll(str)

  def unsafeParse(str: String) =
    parse(str).fold(e => throw new RuntimeException(e.toString), identity)

}
