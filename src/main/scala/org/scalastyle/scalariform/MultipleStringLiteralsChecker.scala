// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle.scalariform

import org.langmeta.inputs.Position
import org.scalastyle.ScalametaChecker
import org.scalastyle.ScalastyleError

import scala.meta.Lit
import scala.meta.Term
import scala.meta.Tree
import scala.util.matching.Regex

class MultipleStringLiteralsChecker extends ScalametaChecker {
  private val DefaultAllowed = 1
  private val DefaultIgnoreRegex = "^\"\"$"
  val errorKey = "multiple.string.literals"

  private val escapeMap = Map(
    '\b' -> "\\b",
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\f' -> "\\f",
    '\r' -> "\\r"
  )

  def verify(ast: Tree): List[ScalastyleError] = {
    val allowed = getInt("allowed", DefaultAllowed)
    val ignoreRegex = getString("ignoreRegex", DefaultIgnoreRegex).replaceAllLiterally("^\"\"$", "^$").r

    val interpolates = SmVisitor.getAll[Term.Interpolate](ast)
    val stringsUnderInterpolates: List[Lit.String] = interpolates.flatMap(i => SmVisitor.getAll[Lit.String](i))

    val strings: List[Lit.String] = SmVisitor.getAll[Lit.String](ast).filterNot(parentIsXml)

    val stringsWithoutInterpolateStrings: Seq[Lit.String] = strings.filterNot(s => positionExistsIn(stringsUnderInterpolates, s.pos))

    val both: Seq[Term] = stringsWithoutInterpolateStrings ++ interpolates

    val seq = for {
      (s, list) <- both.groupBy(s => value(s))
      if !matches(s, ignoreRegex)
      if list.size  > allowed
    } yield toError(list.head, List(escape(s), "" + list.size, "" + allowed))

    seq.toList.sortBy(_.line)
  }

  private def escape(s: String): String = {
    val b = new StringBuilder

    for (c <- s.toCharArray) {
      escapeMap.get(c) match {
        case Some(escape) => b.append(escape)
        case None => {
          if (c >= 128 || c < 32) { // scalastyle:ignore magic.number
            b.append("\\u").append(f"${c.toInt}%04X")
          } else {
            b.append(c)
          }
        }
      }
    }

    b.toString
  }

  private def value(t: Tree) = t match {
    case s: Lit.String => s.value
    case i: Term.Interpolate => i.toString().replaceAll("^" + i.prefix + "\"", "").replaceAll("\"$", "")
    case _ => ""
  }

  private def positionExistsIn(positions: List[Lit.String], p: Position): Boolean = positions.exists(po => samePosition(po.pos, p))

  private def samePosition(p1: Position, p2: Position): Boolean = p1.start == p2.start && p1.end == p2.end

  private def matches(s: String, regex: Regex) = regex.findAllIn(s).size == 1

  // XML literals are parsed to a set of literals, we should ignore them
  private def parentIsXml(t: Tree): Boolean = {
    t.parent match {
      case None => false
      case Some(x) => x match {
        case _: Term.Xml => true
        case _ => false
      }
    }
  }
}
