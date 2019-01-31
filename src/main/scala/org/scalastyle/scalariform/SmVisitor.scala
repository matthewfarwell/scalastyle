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

import org.scalastyle.Checker

import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Dialect
import scala.meta.Term
import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

object SmVisitor {
  class Clazz[+T <: Tree]()
  trait TreeVisit[T] {
    def subs: List[T]
  }

  def getTokens[T <: Token](tokens: Tokens)(implicit manifest: Manifest[T]): Seq[T] = {
    for {
      t <- tokens
      if manifest.runtimeClass.isAssignableFrom(t.getClass)
    } yield {
      t.asInstanceOf[T]
    }
  }

  protected[scalariform] def filterTokens[T <: Token](tokens: Tokens, matches: T => Boolean)(implicit manifest: Manifest[T]): Seq[T] = {
    for {
      t <- tokens
      if manifest.runtimeClass.isAssignableFrom(t.getClass)
      if matches(t.asInstanceOf[T])
    } yield t.asInstanceOf[T]
  }

  protected[scalariform] def getAll[T <: Tree](ast: Tree)(implicit manifest: Manifest[T]): List[T] = {
    def fn(t: T): List[T] = List[T](t)

    visit0[T, T](manifest.runtimeClass.asInstanceOf[Class[T]], fn)(ast)
  }

  private[this] def visit0[T <: Tree, X](clazz: Class[T], fn: T => List[X])(ast: Any): List[X] = {
    val l = if (clazz.isAssignableFrom(ast.getClass)) {
      fn(ast.asInstanceOf[T])
    } else {
      Nil
    }

    l ::: visit(ast, visit0(clazz, fn))
  }

  protected[scalariform] def visit[T](ast: Any, visitfn: (Any) => List[T]): List[T] = {
    ast match {
      case a: Tree                   => visitfn(a.children)
      case Some(x)                   => visitfn(x)
      case xs @ (_ :: _)             => xs.flatMap(visitfn)
      case Left(x)                   => visitfn(x)
      case Right(x)                  => visitfn(x)
      case (l, r)                    => visitfn(l) ::: visitfn(r)
      case (x, y, z)                 => visitfn(x) ::: visitfn(y) ::: visitfn(z)
      case true | false | Nil | None => List()
    }
  }

  def sliding2(tree: Tree)(implicit dialect: Dialect): Iterator[(Token, Token)] = {
    tree.tokens.sliding(2).filter(_.size == 2).map(l => (l(0), l(1)))
  }

  def sliding3(tree: Tree, reverse: Boolean = false)(implicit dialect: Dialect): Iterator[(Token, Token, Token)] = {
    tree.tokens.sliding(3).filter(_.size == 3).map(l => if (reverse) (l(2), l(1), l(0)) else (l(0), l(1), l(2)))
  }

  def sliding5(tree: Tree)(implicit dialect: Dialect): Iterator[(Token, Token, Token, Token, Token)] = {
    tree.tokens.sliding(5).filter(_.size == 5).map(l => (l(0), l(1), l(2), l(3), l(4))) // scalastyle:ignore magic.number
  }

  def isA[T, U](o: T, cls: Class[U]): Boolean = cls.isAssignableFrom(o.getClass)

  def matchMethod(name: String, paramTypesMatch: List[String] => Boolean)(t: Tree): Boolean = {
    t match {
      case d: Defn.Def => methodDefnMatch(name, paramTypesMatch)(d)
      case d: Decl.Def => methodDeclMatch(name, paramTypesMatch)(d)
      case _ => false
    }
  }

  private def methodDefnMatch(name: String, paramTypesMatch: List[String] => Boolean)(t: Defn.Def): Boolean = {
    t.name.value == name && paramTypesMatch(getParamTypes(t.paramss))
  }

  private def methodDeclMatch(name: String, paramTypesMatch: List[String] => Boolean)(t: Decl.Def): Boolean = {
    t.name.value == name && paramTypesMatch(getParamTypes(t.paramss))
  }

  def singleParameter(fn: String => Boolean)(params: List[String]): Boolean = params.size == 1 && fn(params(0))
  def noParameter()(params: List[String]): Boolean = params.isEmpty
  def isEqualsObject(t: Tree): Boolean = matchMethod("equals", singleParameter(Checker.isObject))(t)

  def getParams(p: List[List[Term.Param]]): List[Term.Param] = {
    p.flatten
  }
  def getParamTypes(pc: List[List[Term.Param]]): List[String] = getParams(pc).map(p => typename(p.decltpe.get))
  def typename(t: scala.meta.Type): String = t.toString

  /**
    * Counts the number of new lines between for the tree, adjusted for comments.
    */
  def countNewLines(t: Tree): Int = {
    var minus = 0
    val startLine = t.pos.startLine
    val endLine = t.pos.endLine

    for (line <- startLine to endLine) {
      val list = t.tokens.filter(_.pos.startLine == line)

      if (list.isEmpty) {
        // we are in a multi-line comment
        minus = minus + 1
      } else if (list.exists(t => SmVisitor.isA(t, classOf[Token.Comment])) && list.forall(onlyCommentsAndSpace)) {
        // we have a comment but everthing else is just whitespace or comments, so we can ignore this line
        minus = minus + 1
      }
    }

    endLine - startLine - minus - 1 //  don't count first line
  }

  def onlyCommentsAndSpace(t: Token): Boolean = t match {
    case t: Token.Comment => true
    case t: Token.Space => true
    case t: Token.LF => true
    case t: Token.Tab => true
    case t: Token.CR => true
    case _ => false
  }
}
