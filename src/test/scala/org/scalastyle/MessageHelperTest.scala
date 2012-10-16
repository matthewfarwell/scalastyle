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

package org.scalastyle

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import scala.xml.XML
import scala.xml.Elem
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters

class MessageHelperTest extends AssertionsForJUnit {
  val classLoader = this.getClass().getClassLoader()
  val definition = ScalastyleDefinition.readFromXml(classLoader.getResourceAsStream("scalastyle_definition.xml"))
  val messageHelper = new MessageHelper(classLoader)

  // tests that for each class specified in scalastyle_definitions.xml
  // there exists all of the messages required,
  // i.e. label, message, description
  // depends upon MessageHelper returning the key if there isn't a key in scalastyle_messages
  @Test def testMessages(): Unit = {
    definition.checkers.foreach(c => {
      assertMessage(c.id, "message", messageHelper.message _)
      assertMessage(c.id, "label", messageHelper.label _)
      assertMessage(c.id, "description", messageHelper.description _)

      c.parameters.foreach(p => {
        val key = c.id + "." + p._1
        assertMessage(key, "label", messageHelper.description _)
        assertMessage(key, "description", messageHelper.description _)
      })
    })
  }

  private[this] def assertMessage(id: String, suffix: String, fn: (ClassLoader, String, List[String]) => String) = {
    val key = id + "." + suffix
    assert(fn(classLoader, id, List[String]()) != key, "checker " + id + " should have a (" + key + ")")
  }

  private[this] def assertMessage(id: String, suffix: String, fn: (String) => String) = {
    val key = id + "." + suffix
    assert(fn(id) != key, "checker " + id + " should have a (" + key + ")")
  }
}

import scala.collection.JavaConversions.asJavaIterable

object DocumentationTest {
  val classLoader = this.getClass().getClassLoader()
  val definition = ScalastyleDefinition.readFromXml(classLoader.getResourceAsStream("scalastyle_definition.xml"))
  private def arr[T](a: List[T]) = a.map(al => Array(al.asInstanceOf[java.lang.Object]))

  @Parameters(name= "{index}: documentation({0})")
  def data(): java.lang.Iterable[Array[java.lang.Object]] = arr(definition.checkers.map(_.id))
}

@RunWith(classOf[Parameterized])
class DocumentationTest(id: String) extends AssertionsForJUnit {
  val classLoader = this.getClass().getClassLoader()
  val documentation = XML.load(classLoader.getResourceAsStream("scalastyle_documentation.xml"))

  // tests that for each class specified in scalastyle_definitions.xml
  // there exists in scalastyle_documentation an entry for example-configuration and justification

  @Test def testExampleConfiguration(): Unit = {
    assertElementExists(id, "example-configuration", documentation)
  }

  @Test def testJustification(): Unit = {
    assertElementExists(id, "justification", documentation)
  }

  private[this] def assertElementExists(id: String, name: String, elem: Elem) = {
    val seq = (elem \\ "check" filter { _ \\ "@id" exists (_.text == id) }) \\ name
    assertEquals("did not find " + name, 1, seq.size)
  }
}
