package org.scalastyle.scalariform

import scala.meta.Dialect
import scala.meta.common.Convert
import scala.meta.parsers.Parse
import scala.meta.parsers.Parsed
import java.nio.charset.MalformedInputException

import org.langmeta.inputs.Position

import scala.annotation.tailrec
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConversions.seqAsJavaList
import scala.io.Codec
import scala.meta.Source
import scala.meta.Tree
import scala.meta._
import org.scalameta.logger
import fastparse.core._

object Foobar {
  import scala.meta._

  def main(args: Array[String]): Unit = {
    val text =
      """
        |object X {
        |
        |  /**
        |   * description
        |   * @param a
        |   *   Some text for parameter A
        |   *   More for A
        |   * @param b B
        |   * @param c
        |   * @returns some integer
        |   */
        |  def foo(a: Int, b: Int, c: Int): Int = a + b
        |}
      """.stripMargin

    val tree = text.parse[Source].get

    val c = SmVisitor.getTokens[Token.Comment](tree.tokens)

    val comment = c.head
    println("comment=\n" + comment.value)
    println("comment=\n" + comment.value.map(_.toInt))
    println("tokens=" + CommentOps2.docTokens(comment))
  }
}
