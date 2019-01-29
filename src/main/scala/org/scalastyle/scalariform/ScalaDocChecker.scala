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

import org.scalastyle.CombinedMeta
import org.scalastyle.CombinedMetaChecker
import org.scalastyle.LineError
import org.scalastyle.ScalastyleError

import scala.collection.immutable.TreeSet
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Mod
import scala.meta.Name
import scala.meta.Tree
import scala.meta.contrib.DocToken
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

case class ScalaDocWithTree(comment: Token.Comment, parsed: List[DocToken], tree: Tree, previousWhiteSpace: Int)

/**
  * Checks that the ScalaDoc exists for all accessible members:
  * - classes, traits, case classes and objects
  * - methods
  * - vals, vars and types
  *
  * The ScalaDoc's structure must satisfy the parameter of the constructor in case of
  * case classes and classes, or the parameter of the methods. The ScalaDoc must include
  * the type parameters. Finally, the ScalaDoc must include return description for non-Unit
  * returning methods.
  */
class ScalaDocChecker extends CombinedMetaChecker {
  import ScalaDocChecker._ // scalastyle:ignore underscore.import import.grouping
  import ScalaDocIndent._ // scalastyle:ignore underscore.import import.grouping

  protected val errorKey: String = "scaladoc"

  private val DefaultIgnoreRegex = "^$"
  private val DefaultIgnoreTokenTypes = ""
  private val DefaultIgnoreOverride = false
  private val DefaultIndentStyle = ""

  override def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val tokens = ast.tree.tokens

    val ignoreRegex = getString("ignoreRegex", DefaultIgnoreRegex)
    val tokensToIgnore = TreeSet[String]() ++ getString("ignoreTokenTypes", DefaultIgnoreTokenTypes).split(",").filterNot(_.isEmpty).toSet
    val ignoreOverride = getBoolean("ignoreOverride", DefaultIgnoreOverride)
    val indentStyle = getStyleFrom(getString("indentStyle", DefaultIndentStyle))

    assertTokensToIgnore(tokensToIgnore)

    val allTrees = SmVisitor.getAll[Tree](ast.tree)

    val treesWhichShouldHaveScaladoc = allTrees
      .filter(scaladocTree(tokensToIgnore))
      .filterNot(isPrivate)
      .filterNot(t => ignoreOverride && isOverride(t))
      .filterNot(matchesRegex(ignoreRegex))

//    println("treesWhichShouldHaveScaladoc=" + treesWhichShouldHaveScaladoc.map(x => x.pos.start + " " + x.getClass).mkString("\n"))
    val ss = findTokensAndAttachedTree(tokensToIgnore, allTrees, ast.tree.tokens)

    treesWhichShouldHaveScaladoc.flatMap { t =>
      ss.find(p => p.tree.pos.start == t.pos.start) match {
        case Some(p) => check(t, p, indentStyle)
        case None    => List(LineError(t.pos.startLine + 1, List(Missing)))
      }
    }
  }

  private def matchesRegex(ignoreRegex: String)(t: Tree): Boolean = enclosingTopLevelName(t).exists(_.matches(ignoreRegex))

  private def enclosingTopLevelName(t: Tree): Option[String] = {
    t.parent.flatMap(enclosingTopLevelName).orElse {
      t match {
        case f: Defn.Class  => Some(f.name.value)
        case f: Defn.Object => Some(f.name.value)
        case _              => None
      }
    }
  }

  private def check(t: Tree, p: ScalaDocWithTree, indentStyle: DocIndentStyle): List[ScalastyleError] = {
    val list = p.tree match {
      case d: Defn.Object => checkObject(d, p)
      case d: Defn.Class  => checkClass(d, p)
      case d: Defn.Trait  => checkTrait(d, p)

      case d: Defn.Def  => checkDefnDef(d, p)
      case d: Defn.Var  => checkDefnVar(d, p)
      case d: Defn.Val  => checkDefnVal(d, p)
      case d: Defn.Type => checkDefnType(d, p)

      case d: Decl.Def  => checkDeclDef(d, p)
      case d: Decl.Var  => checkDeclVar(d, p)
      case d: Decl.Type => checkDeclType(d, p)

      case _ => Nil
    }

    list ::: indentErrors(t.pos.startLine + 1, indentStyle, ScalaDocIndent.parse(p.comment.text, p.previousWhiteSpace))
  }


  private def isPrivate(t: Tree): Boolean = extractMods(t).exists(isPrivate)
  private def isOverride(t: Tree): Boolean = extractMods(t).exists(isOverride)

  private def checkObject(c: Defn.Object, sd: ScalaDocWithTree): List[ScalastyleError] = {
    // TODO what about inheritance
    // TODO do we need to check the description?
    Nil
  }

  private def checkClass(c: Defn.Class, sd: ScalaDocWithTree): List[ScalastyleError] = {
    val cparams = c.ctor.paramss.flatten.flatMap(p => docParamExists(c, p.name.value, sd.parsed))

    val tparams = c.tparams.flatMap(p => docTparamExists(c, p.name.value, sd.parsed)).headOption.toList

    cparams ::: tparams
  }

  private def checkTrait(t: Defn.Trait, sd: ScalaDocWithTree): List[ScalastyleError] = {
    val cparams = t.ctor.paramss.flatten.flatMap(p => docParamExists(t, p.name.value, sd.parsed))

    val tparams = t.tparams.flatMap(p => docTparamExists(t, p.name.value, sd.parsed)).headOption.toList

    cparams ::: tparams
  }

  private def checkDefnDef(t: Defn.Def, sd: ScalaDocWithTree): List[ScalastyleError] = {
    val cparams = t.paramss.flatten.flatMap(p => docParamExists(t, p.name.value, sd.parsed))

    val tparams = t.tparams.flatMap(p => docTparamExists(t, p.name.value, sd.parsed)).headOption.toList

    val ret = t.decltpe.flatMap(p => docReturnExists(p, sd.parsed)).toList

    cparams ::: tparams ::: ret
  }

  private def checkDefnVar(t: Defn.Var, sd: ScalaDocWithTree): List[ScalastyleError] = Nil

  private def checkDefnVal(t: Defn.Val, sd: ScalaDocWithTree): List[ScalastyleError] = Nil

  private def checkDefnType(t: Defn.Type, sd: ScalaDocWithTree): List[ScalastyleError] = Nil

  private def checkDeclDef(t: Decl.Def, sd: ScalaDocWithTree): List[ScalastyleError] = {
    val cparams = t.paramss.flatten.flatMap(p => docParamExists(t, p.name.value, sd.parsed))

    val tparams = t.tparams.flatMap(p => docTparamExists(t, p.name.value, sd.parsed)).headOption.toList

    val ret = docReturnExists(t.decltpe, sd.parsed).toList

    cparams ::: tparams ::: ret
  }

  private def checkDeclVar(t: Decl.Var, sd: ScalaDocWithTree): List[ScalastyleError] = {
    // TODO add these tests in
    ???
  }

  private def checkDeclVal(t: Decl.Val, sd: ScalaDocWithTree): List[ScalastyleError] = {
    // TODO add these tests in
    ???
  }

  private def checkDeclType(t: Decl.Type, sd: ScalaDocWithTree): List[ScalastyleError] = {
    // TODO add these tests in
    ???
  }

  private def isPrivate(m: Mod): Boolean = m match {
    case p: Mod.Private => {
      p.within match {
        case i: Name.Indeterminate => false
        case _                     => true
      }
    }
    case _ => false
  }

  private def isOverride(m: Mod): Boolean = m match {
    case p: Mod.Override => true
    case _               => false
  }

  private def docParamExists(t: Tree, name: String, tokens: List[DocToken]): Option[ScalastyleError] = {
    tokens.filter(_.kind == DocToken.Param).find(p => p.name.getOrElse("") == name) match {
      case None => Some(LineError(t.pos.startLine + 1, List(missingParam(name))))
      case Some(x) => {
        val text = x.body.getOrElse("").trim
        if (text == "" || text.startsWith("@")) Some(LineError(t.pos.startLine + 1, List(emptyParam(name)))) else None
      }
    }
  }

  private def docTparamExists(t: Tree, name: String, tokens: List[DocToken]): Option[ScalastyleError] = {
    tokens.filter(_.kind == DocToken.TypeParam).find(p => p.name.getOrElse("") == name) match {
      case None                                       => Some(LineError(t.pos.startLine + 1, List(MalformedTypeParams)))
      case Some(x) if x.body.getOrElse("").trim == "" => Some(LineError(t.pos.startLine + 1, List(emptyParam(name))))
      case _                                          => None
    }
  }

  private def docReturnExists(t: scala.meta.Type, tokens: List[DocToken]): Option[ScalastyleError] = {
    if (t.toString == "Unit" || t.toString.trim == "") {
      None
    } else {
      tokens.find(_.kind == DocToken.Return) match {
        case None                                       => Some(LineError(t.pos.startLine + 1, List(MalformedReturn)))
        case Some(x) if x.body.getOrElse("").trim == "" => Some(LineError(t.pos.startLine + 1, List(MalformedReturn)))
        case _                                          => None
      }
    }
  }

  private def findTokensAndAttachedTree(ignoreTokens: Set[String], ast: List[Tree], tokens: scala.meta.tokens.Tokens): Seq[ScalaDocWithTree] = {
    SmVisitor
      .getTokens[meta.tokens.Token.Comment](tokens)
      .filter(c => CommentOps2.isScaladoc(c))
      .flatMap { c =>
        for {
          parsed <- CommentOps2.docTokens(c)
          t <- findNextNonWhitespaceToken(ignoreTokens, ast, c)
        } yield {
          ScalaDocWithTree(c, parsed, t, findNumberOfWhitespaceBefore(tokens, c))
        }
      }
  }

  private def findNumberOfWhitespaceBefore(tokens: Tokens, comment: Token.Comment): Int = {
    val index = tokens.indexOf(comment)
    var count = 0
    for (i <- index - 1 to 0 by -1) {
      if (!isSpace(tokens(i))) {
        return count
      }

      count = count + 1
    }

    count
  }

  private def isSpace(t: Token): Boolean = {
    t match {
      case t: Token.Space => true
      case t: Token.Tab   => true
      case _              => false
    }
  }

  private def findNextNonWhitespaceToken(ignoreTokens: Set[String], trees: List[Tree], c: Token.Comment): Option[Tree] = {
    trees.find(t => scaladocTree(ignoreTokens)(t) && t.pos.start > c.pos.start)
  }

  private def indentErrors(line: Int, style: DocIndentStyle, scalaDocStyle: DocIndentStyle): List[ScalastyleError] = {
    if (style == AnyDocStyle || style == scalaDocStyle) {
      Nil
    } else {
      List(LineError(line, List(InvalidDocStyle)))
    }
  }
}

object ScalaDocChecker {
  private val availableTokensToIgnore = TreeSet("PatDefOrDcl", "TypeDefOrDcl", "FunDefOrDcl", "TmplDef")

  val Missing = "Missing"
  def missingParam(name: String): String = "Missing @param " + name
  def extraParam(name: String): String = "Extra @param " + name
  def emptyParam(name: String): String = "Missing text for @param " + name
  val MalformedTypeParams = "Malformed @tparams"
  val MalformedReturn = "Malformed @return"
  val InvalidDocStyle = "Invalid doc style"

  def assertTokensToIgnore(tokensToIgnore: Set[String]): Unit = {
    val wrongTokensToIgnore = tokensToIgnore.diff(availableTokensToIgnore)
    if (wrongTokensToIgnore.nonEmpty) {
      throw new IllegalArgumentException(
        s"ignoreTokenTypes contained wrong types: ${wrongTokensToIgnore.toList.sorted.mkString(",")}, " +
          s"available types are ${availableTokensToIgnore.toList.sorted.mkString(",")}")
    }
  }

  // scalastyle:off cyclomatic.complexity

  def scaladocTree(ignoreTokens: Set[String])(t: Tree): Boolean = t match {
    case d: Defn.Object => !ignoreTokens.contains("TmplDef")
    case d: Defn.Class  => !ignoreTokens.contains("TmplDef")
    case d: Defn.Trait  => !ignoreTokens.contains("TmplDef")

    case d: Defn.Def  => !ignoreTokens.contains("FunDefOrDcl")
    case d: Defn.Var  => !ignoreTokens.contains("PatDefOrDcl")
    case d: Defn.Val  => !ignoreTokens.contains("PatDefOrDcl")
    case d: Defn.Type => !ignoreTokens.contains("TypeDefOrDcl")

    case d: Decl.Def  => !ignoreTokens.contains("FunDefOrDcl")
    case d: Decl.Var  => !ignoreTokens.contains("PatDefOrDcl")
    case d: Decl.Val  => !ignoreTokens.contains("PatDefOrDcl")
    case d: Decl.Type => !ignoreTokens.contains("TypeDefOrDcl")
    case _            => false
  }

  def extractMods(t: Tree): List[Mod] = t match {
    case d: Defn.Object => d.mods
    case d: Defn.Class  => d.mods
    case d: Defn.Trait  => d.mods

    case d: Defn.Def  => d.mods
    case d: Defn.Var  => d.mods
    case d: Defn.Val  => d.mods
    case d: Defn.Type => d.mods

    case d: Decl.Def  => d.mods
    case d: Decl.Var  => d.mods
    case d: Decl.Val  => d.mods
    case d: Decl.Type => d.mods
    case _            => Nil
  }

  // scalastyle:on cyclomatic.complexity

}

/**
  * Contains the ScalaDoc model with trivial parsers
  */
object ScalaDocIndent {
  sealed trait DocIndentStyle
  object ScalaDocStyle extends DocIndentStyle
  object JavaDocStyle extends DocIndentStyle
  object AnyDocStyle extends DocIndentStyle
  object UndefinedDocStyle extends DocIndentStyle

  def getStyleFrom(name: String): DocIndentStyle = name.toLowerCase match {
    case "scaladoc"    => ScalaDocStyle
    case "javadoc"     => JavaDocStyle
    case "anydoc" | "" => AnyDocStyle
    case _             => throw new IllegalArgumentException(s"Unsupported ScalaDocChecker indentStyle: '$name'")
  }

  /**
    * Take the ``raw`` and parse an instance of ``ScalaDoc``
    * @param comment the text containing the scaladoc
    * @param previousWhitespace column number of scaladoc's first string
    * @return the parsed instance
    */
  def parse(comment: String, previousWhitespace: Int): DocIndentStyle = {
    val strings = comment.split("\\n").toList

    getStyle(strings.tail, UndefinedDocStyle, previousWhitespace)
  }

  // scalastyle:off cyclomatic.complexity
  def getStyle(xs: List[String], style: DocIndentStyle, previousWhitespace: Int): DocIndentStyle = xs match {
    case x :: tail =>
      val prefixSizeDiff = if (x.trim.head == '*') x.substring(0, x.indexOf("*")).length - previousWhitespace else -1
      val lineStyle = prefixSizeDiff match {
        case 1 => JavaDocStyle
        case 2 => ScalaDocStyle
        case _ => AnyDocStyle
      }
      style match {
        case ScalaDocStyle | JavaDocStyle => if (lineStyle == style) getStyle(tail, style, previousWhitespace) else AnyDocStyle
        case AnyDocStyle                  => AnyDocStyle
        case UndefinedDocStyle            => getStyle(tail, lineStyle, previousWhitespace)
      }
    case Nil => if (style == UndefinedDocStyle) AnyDocStyle else style
  }
}
