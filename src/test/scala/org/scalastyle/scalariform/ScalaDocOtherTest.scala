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

import org.junit.Assert.assertEquals
import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.junit.AssertionsForJUnit

import scala.meta.Ctor
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Lit
import scala.meta.Mod
import scala.meta.Pat
import scala.meta.Self
import scala.meta.Template
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type

class ScalaDocIndentTest extends AssertionsForJUnit with Matchers {
  @Test
  def styleFrom(): Unit = {
    assertEquals(ScalaDocIndent.JavaDocStyle, ScalaDocIndent.getStyleFrom("javadoc"))
    assertEquals(ScalaDocIndent.JavaDocStyle, ScalaDocIndent.getStyleFrom("JAVADOC"))

    assertEquals(ScalaDocIndent.ScalaDocStyle, ScalaDocIndent.getStyleFrom("scaladoc"))
    assertEquals(ScalaDocIndent.ScalaDocStyle, ScalaDocIndent.getStyleFrom("SCALADOC"))

    assertEquals(ScalaDocIndent.AnyDocStyle, ScalaDocIndent.getStyleFrom("anydoc"))
    assertEquals(ScalaDocIndent.AnyDocStyle, ScalaDocIndent.getStyleFrom("ANYDOC"))
    assertEquals(ScalaDocIndent.AnyDocStyle, ScalaDocIndent.getStyleFrom(""))

    val e = intercept[IllegalArgumentException] {
      ScalaDocIndent.getStyleFrom("foobar")
    }

    assertEquals("Unsupported ScalaDocChecker indentStyle: 'foobar'", e.getMessage)
  }


  @Test
  def indentStyleUndefined(): Unit = {
    val source = """   /**
                   |   * ...
                   |   */""".stripMargin('|')

    assertEquals(ScalaDocIndent.AnyDocStyle, ScalaDocIndent.parse(source, 3))
  }

  @Test
  def indentStyleScaladoc(): Unit = {
    val source = """   /**
                   |     * ...
                   |     */""".stripMargin('|')

    assertEquals(ScalaDocIndent.ScalaDocStyle, ScalaDocIndent.parse(source, 3))
  }

  @Test
  def indentStyleJavadoc(): Unit = {
    val source = """   /**
                   |    * ...
                   |    */""".stripMargin('|')

    assertEquals(ScalaDocIndent.JavaDocStyle, ScalaDocIndent.parse(source, 3))
  }

}

class ScalaDocCheckerObjectTest extends AssertionsForJUnit with Matchers {
  @Test
  def assertTokensToIgnoreKO(): Unit = {
    val valid = Set("PatDefOrDcl", "TypeDefOrDcl", "FunDefOrDcl", "TmplDef")

    val e = intercept[IllegalArgumentException] {
      ScalaDocChecker.assertTokensToIgnore(valid ++ Set("foo", "bar"))
    }

    assertEquals("ignoreTokenTypes contained wrong types: bar,foo, available types are FunDefOrDcl,PatDefOrDcl,TmplDef,TypeDefOrDcl", e.getMessage)
  }

  @Test
  def assertTokensToIgnoreOK(): Unit = {
    val valid = Set("PatDefOrDcl", "TypeDefOrDcl", "FunDefOrDcl", "TmplDef")

    ScalaDocChecker.assertTokensToIgnore(valid) // no exception, no problem
  }

  @Test
  def scaladocTree(): Unit = {
    val termName = Term.Name("term")
    val typeName = Type.Name("type")
    val template = Template(Nil, Nil, Self(termName, None), Nil)
    val all = Set("PatDefOrDcl", "TypeDefOrDcl", "FunDefOrDcl", "TmplDef")

    def check(tokenType: String, t: Tree): Unit = {
      assertEquals(s"${t.getClass.getName} empty set", true, ScalaDocChecker.scaladocTree(Set())(t))
      assertEquals(s"${t.getClass.getName} everything but ${tokenType}", true, ScalaDocChecker.scaladocTree(all.diff(Set(tokenType)))(t))
      assertEquals(s"${t.getClass.getName} only token type ${tokenType}", false, ScalaDocChecker.scaladocTree(Set(tokenType))(t))
    }

    check("TmplDef", Defn.Object(Nil, termName, template))
    check("TmplDef", Defn.Class(Nil, typeName, Nil, Ctor.Primary(Nil, termName, Nil), template))
    check("TmplDef", Defn.Trait(Nil, typeName, Nil, Ctor.Primary(Nil, termName, Nil), Template(Nil, Nil, Self(termName, None), Nil)))

    check("FunDefOrDcl", Defn.Def(Nil, termName, Nil, Nil, None, Term.Block(Nil)))
    check("PatDefOrDcl", Defn.Var(Nil, List(Pat.Wildcard()), Some(typeName), Some(termName)))
    check("PatDefOrDcl", Defn.Val(Nil, List(Pat.Wildcard()), Some(typeName), termName))
    check("TypeDefOrDcl", Defn.Type(Nil, typeName, Nil, typeName))

    check("FunDefOrDcl", Decl.Def(Nil, termName, Nil, Nil, typeName))
    check("PatDefOrDcl", Decl.Var(Nil, List(Pat.Wildcard()), typeName))
    check("PatDefOrDcl", Decl.Val(Nil, List(Pat.Wildcard()), typeName))
    check("TypeDefOrDcl", Decl.Type(Nil, typeName, Nil, Type.Bounds(None, None)))

    val tree = Lit.Int(2)
    assertEquals(s"${tree.getClass.getName} empty set", false, ScalaDocChecker.scaladocTree(Set())(tree))
    assertEquals(s"${tree.getClass.getName} all token type", false, ScalaDocChecker.scaladocTree(all)(tree))
  }

  @Test
  def extractMods(): Unit = {
    val mods = List(Mod.Abstract(), Mod.Final())
    val termName = Term.Name("term")
    val typeName = Type.Name("type")
    val template = Template(Nil, Nil, Self(termName, None), Nil)

    def check(expected: List[Mod], t: Tree): Unit = {
      assertEquals(s"${t.getClass.getName}", expected.toString, ScalaDocChecker.extractMods(t).toString)
    }

    check(mods, Defn.Object(mods, termName, template))
    check(mods, Defn.Class(mods, typeName, Nil, Ctor.Primary(Nil, termName, Nil), template))
    check(mods, Defn.Trait(mods, typeName, Nil, Ctor.Primary(Nil, termName, Nil), Template(Nil, Nil, Self(termName, None), Nil)))

    check(mods, Defn.Def(mods, termName, Nil, Nil, None, Term.Block(Nil)))
    check(mods, Defn.Var(mods, List(Pat.Wildcard()), Some(typeName), Some(termName)))
    check(mods, Defn.Val(mods, List(Pat.Wildcard()), Some(typeName), termName))
    check(mods, Defn.Type(mods, typeName, Nil, typeName))

    check(mods, Decl.Def(mods, termName, Nil, Nil, typeName))
    check(mods, Decl.Var(mods, List(Pat.Wildcard()), typeName))
    check(mods, Decl.Val(mods, List(Pat.Wildcard()), typeName))
    check(mods, Decl.Type(mods, typeName, Nil, Type.Bounds(None, None)))

    check(Nil, Lit.Int(2))
  }



}
