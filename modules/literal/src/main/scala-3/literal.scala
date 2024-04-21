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

import org.typelevel.literally.Literally

import com.filippodeluca.jsonpath.ast.Exp
import com.filippodeluca.jsonpath.parser.JsonPathParser

object literal:

  extension (inline ctx: StringContext)
    inline def jsonPath(inline args: Any*): Exp =
      ${ JsonPathLiteral('ctx, 'args) }

  object JsonPathLiteral extends Literally[Exp]:
    def validate(s: String)(using Quotes) =
      JsonPathParser.parse(s) match {
        case Left(e) => Left(s"Error parsing JsonPath: ${e}")
        case Right(_) =>
          Right('{ JsonPathParser.unsafeParse(${ Expr(s) }) })

      }
