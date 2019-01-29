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

import org.scalastyle.ScalametaChecker
import org.scalastyle.ScalastyleError

import scala.meta.Defn
import scala.meta.Tree
import scala.meta.Type
import scala.util.matching.Regex

class EmptyClassChecker extends ScalametaChecker {
  val errorKey = "empty.class"

  final def verify(ast: Tree): List[ScalastyleError] = {
    val classErrors = SmVisitor.getAll[Defn.Class](ast).filter(matches).map(_.name)
    val traitErrors = SmVisitor.getAll[Defn.Trait](ast).filter(matches).map(_.name)

    (classErrors ::: traitErrors).map(toError)
  }

  private def matches(t: Defn.Class): Boolean = t.templ.stats.isEmpty && t.templ.toString().startsWith("{")
  private def matches(t: Defn.Trait): Boolean = t.templ.stats.isEmpty && t.templ.toString().startsWith("{")
}

class ClassTypeParameterChecker extends ScalametaChecker {
  val DefaultRegex = "^[A-Z_]$"
  val errorKey = "class.type.parameter.name"

  final def verify(ast: Tree): List[ScalastyleError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val classErrors = SmVisitor.getAll[Defn.Class](ast).filter(matchesClass(regex)).map(_.name)
    val traitErrors = SmVisitor.getAll[Defn.Trait](ast).filter(matchesTrait(regex)).map(_.name)

    (classErrors ::: traitErrors).map(e => toError(e, List(regexString)))
  }

  private[this] def matchesType(regex: Regex)(t: Type.Param): Boolean = {
    innermostNames(t).exists(s => !matchesRegex(regex, s))
  }

  private[this] def matchesRegex(regex: Regex, s: String) = regex.findAllIn(s).size == 1

  private[this] def innermostNames(tp: Type.Param): List[String] = {
    if (tp.tparams.isEmpty) {
      List(tp.name.value)
    } else {
      tp.tparams.flatMap(innermostNames)
    }
  }

  private def matchesClass(regex: Regex)(t: Defn.Class): Boolean = t.tparams.exists(matchesType(regex))
  private def matchesTrait(regex: Regex)(t: Defn.Trait): Boolean = t.tparams.exists(matchesType(regex))

}
