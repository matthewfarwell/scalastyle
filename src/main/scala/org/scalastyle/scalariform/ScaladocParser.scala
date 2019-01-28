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

// scalastyle:off multiple.string.literals underscore.import import.grouping

import fastparse.all._
import fastparse.core.Parsed

import scala.annotation.tailrec
import scala.meta.contrib.DocToken
import scala.meta.contrib.DocToken._
import scala.meta.contrib.implicits.CommentExtensions._
import scala.meta.tokens.Token.Comment

object CommentOps2 {

  private[this] val scaladocSymbols: Char => Boolean = Set('*', ' ')
  private[this] val scaladocBorderSymbols: Char => Boolean = Set('/', '*', ' ')

  @tailrec
  private def dropRightWhile(str: String, predicate: (Char) => Boolean): String =
    if (str.isEmpty)
      ""
    else if (predicate(str.last))
      dropRightWhile(str.init, predicate)
    else
      str

  @inline
  def isScaladoc(c: Comment): Boolean = {
    val rawSyntax: String = c.syntax.trim
    rawSyntax.startsWith("/**") && rawSyntax.endsWith("*/")
  }

  def content(c: Comment): Option[String] = {
    val rawSyntax: String = c.syntax.trim
    if (isScaladoc(c)) {
      val content =
        dropRightWhile(rawSyntax, scaladocBorderSymbols)
          .dropWhile(scaladocBorderSymbols)

      Option(
        content.lines
          .map(_.dropWhile(scaladocSymbols)) // Removes leading comments symbols
          .map(_.trim)
          .mkString("\n")
          .trim)
    } else {
      Option.empty
    }
  }

  @inline
  def docTokens(c: Comment): Option[List[DocToken]] = ScaladocParser2.parseScaladoc(c)
}


object ScaladocParser2 {

  private[this] val numberOfSupportedHeadingLevels = 6

  /**
    * Parses a scaladoc comment.
    */
  def parseScaladoc(comment: Comment): Option[List[DocToken]] = {

    def parseRec(toParse: String): List[DocToken] = {
      parsers
        .find(_.parse(toParse).index != 0)
        .map(_.parse(toParse)) match {
        case Some(p: Parsed.Success[DocToken, _, _]) =>
          // Parse was successful, check the remaining Scaladoc
          val remainingScaladoc =
            toParse
              .substring(p.index, toParse.length)
              .dropWhile(c => c == ' ')

          if (remainingScaladoc.trim.nonEmpty || remainingScaladoc.contains("\n\n")) {
            // Adds the parsed token to the list of tokens and parse the rest of the string recursively.
            if (remainingScaladoc.take(2) == "\n\n") {
              List(p.value, DocToken(Paragraph)) ++ parseRec(remainingScaladoc.dropWhile(_ == '\n'))
            } else {
              List(p.value) ++ parseRec(remainingScaladoc.dropWhile(c => c == ' ' || c == '\n'))
            }
          } else {
            // No more elements to parse, end recursion.
            List(p.value)
          }
        // Can't parse anymore, end recursion.
        case _ => List()
      }
    }

    comment.content.map(parseRec)
  }

  private[this] def generateHeadingParser(headingType: Heading): Parser[DocToken] = {
    val headingSymbols = "=" * headingType.level
    P(
      // Code block start
      headingSymbols
        // Heading description
        ~ ((AnyChar ~ !"=").rep ~ AnyChar).!.map(c => DocToken(headingType, c.trim))
        // Code block end
        ~ headingSymbols
    )
  }

  /**
    * Set containing all the scaladoc parsers.
    */
  private[this] val parsers: List[Parser[DocToken]] = {

    // don't allow the first character of a body to be @, map as an empty body
    val bodyParser = ((!AnyChar.!.filter(s => s != "@")).map(_ => "") | ((AnyChar ~ !("\n@" | "{{{" | "\n\n" | End)).rep ~ AnyChar)).!.map(_.trim)

    // Paragraph Parser
    val paragraphParser = "\n\n".rep.!.map(_ => DocToken(Paragraph))

    // Parser for CodeBlock instances
    val codeBlockParser =
      P(
        // Code block start
        "{{{"
          // Code within the code block.
          ~ ((AnyChar ~ !"}}}").rep ~ AnyChar).!.map(c => DocToken(CodeBlock, c.trim))
          // Code block end
          ~ "}}}"
      )

    // Parsers for headings/subheadings instances.
    val headingsParsers = DocToken.allHeadings.reverse.map(generateHeadingParser)

    // Parser for Inheritdoc instances
    val inheritDocParser = P("@inheritdoc".!).map(_ => DocToken(InheritDoc))

    // Parsers for all labelled docs instances
    val labelledParsers: List[Parser[DocToken]] = {

      DocToken.tagTokenKinds.map {
        // Single parameter doc tokens
        case kind @ DocToken.TagKind(label, 1) =>
          P(s"$label" ~ " " ~ bodyParser.map(c => DocToken(kind, c.trim)))

        // Multiple parameter doc tokens
        case kind @ DocToken.TagKind(label, 2) =>
          val nameParser = ((AnyChar ~ !(" " | "\n")).rep ~ AnyChar).!.map(_.trim)

          val nameAndBodyParsers = {
            (nameParser ~ ("\n" | " ").rep.? ~ bodyParser.!).map {
              case (name, body) => DocToken(kind, name, body)
            }
          }
          P(s"$label" ~ nameAndBodyParsers)
      }
    }

    // Fallback parser(Used when no label or description is provided)
    val descriptionParser = bodyParser.map(DocToken(Description, _))

    // Merges all the parsers in a single list, with the description parser as the fallback,
    // in case no valid parser was found for an Scaladoc comment.
    (List(
      paragraphParser,
      inheritDocParser,
      codeBlockParser
    ) ++ headingsParsers ++ labelledParsers) :+ descriptionParser
  }
}
