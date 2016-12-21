package com.sagacify.sonar.scala

import org.scalatest._

class MeasuresSpec extends FlatSpec with Matchers {

    val exampleSourceFile = """/*
 * Sonar Scala Plugin
 * Copyright (C) 2011-2016 SonarSource SA
 * mailto:contact AT sonarsource DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package com.sagacify.example

import collection.mutable.Stack
import org.scalatest._

class ScalaSensorSpec extends FlatSpec with Matchers {

  // Example test
  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1) // This is
    stack.push(2) // a pointless
    stack.pop() should be (2) // example
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }
}
"""

  "A Comment lines counter" should "count line comments" in {
    val tokens = Scala.tokenize("// this is a test", "2.11.8")
    val count = Measures.countCommentLines(tokens)
    assert(count == 1)
  }

  it should "count multiline comments" in {
    val tokens = Scala.tokenize("/* this\n *is\n *a\n *test*/", "2.11.8")
    val count = Measures.countCommentLines(tokens)
    assert(count == 4)
  }

  it should "count trailing comments." in {
    val tokens = Scala.tokenize("case class Test() // this is a test", "2.11.8")
    val count = Measures.countCommentLines(tokens)
    assert(count == 1)
  }

  it should "count the correct number of comments" in {
    val tokens = Scala.tokenize(exampleSourceFile, "2.11.8")
    val count = Measures.countCommentLines(tokens)
    assert(count == 23)
  }

  "A Non-Comment lines counter" should "count non-comment lines of codes" in {
    val tokens = Scala.tokenize("package com.example", "2.11.8")
    println(tokens)
    val count = Measures.countNCLoC(tokens)
    assert(count == 1)
  }

  it should "count lines of code with a trailing comment" in {
    val tokens = Scala.tokenize("case class Test() /*\n * test\n */", "2.11.8")
    val count = Measures.countNCLoC(tokens)
    assert(count == 1)
  }

  it should "count trailing code." in {
    val tokens = Scala.tokenize("/* this is a test */ case class Test()", "2.11.8")
    val count = Measures.countNCLoC(tokens)
    assert(count == 1)
  }

  it should "count the correct number of lines" in {
    val tokens = Scala.tokenize(exampleSourceFile, "2.11.8")
    val count = Measures.countNCLoC(tokens)
    assert(count == 18)
  }

  it should "count the number of classes" in {
    val tokens = Scala.tokenize(exampleSourceFile, "2.11.8")
    val count = Measures.countClasses(tokens)
    assert(count == 1)
  }

  it should "count the number of functions" in {
    val tokens = Scala.tokenize("def foo(x: Int) = {}", "2.11.8")
    val count = Measures.countFunctions(tokens)
    assert(count == 1)
  }

  it should "count class, object, and trait constructors as functions" in {
    val tokens = Scala.tokenize("class Foo {} object Bar {} trait Baz {}", "2.11.8")
    val count = Measures.countFunctions(tokens)
    assert(count == 3)
  }

  it should "count complexity of a simple function" in {
    val tokens = Scala.tokenize("def foo = 1", "2.11.8")
    val count = Measures.calculateComplexity(tokens)
    assert(count == 1)
  }

  it should "add to complexity in a function with IF-ELSE statement" in {
    val tokens = Scala.tokenize("def foo(s: Boolean) = if s then 1 else 0", "2.11.8")
    val count = Measures.calculateComplexity(tokens)
    assert(count == 2)
  }

  it should "add to complexity in a function with MATCH-CASE statement" in {
    val tokens = Scala.tokenize(
"""
      def foo(i: Int) = {
        i match {
          case 0 => 1
          case 1 => 10
        }
      }
""", "2.11.8")
    val count = Measures.calculateComplexity(tokens)
    assert(count == 3)
  }

  it should "add to complexity in a function with WHILE statement" in {
    val tokens = Scala.tokenize(
"""
      def foo(i: Int) = {
        var a = 10
        while (a > 0) {
          a = a - 1
        }
      }
""", "2.11.8")
    val count = Measures.calculateComplexity(tokens)
    assert(count == 2)
  }

  it should "count correct complexity" in {
    val tokens = Scala.tokenize(
"""
      def foo(i: Int, b: Boolean) = {
        val bar = if (i > 10) {
          21
        } else {
          i match {
            case i < 9 => 231
            case i == 32 if b => 444 // Note the extra IF!
            case _ => 456
          }
        }

        var a = 0
        while (a < 100) {
          a = a + 1
        }

        try {
          for {
            b <- doFoo()
            c <- doBar()
            d <- doBaz()
          } yield "bla"
        } catch {
          case exception: Throwable => "error"
        }
      }
""", "2.11.8")
    val count = Measures.calculateComplexity(tokens)
    assert(count == 9)
  }
}
