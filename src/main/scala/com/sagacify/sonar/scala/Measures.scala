package com.sagacify.sonar.scala

import scala.annotation.tailrec
import scalariform.lexer.{Token, Tokens}
import scalariform.lexer.Tokens.LINE_COMMENT
import scalariform.lexer.Tokens.MULTILINE_COMMENT
import scalariform.lexer.Tokens.XML_COMMENT
import scalariform.lexer.Tokens.WS
import scalariform.lexer.Tokens.EOF
import scalariform.parser.AstNode

object Measures {

  /* applied on raw source code */

  /* applied on lines of code */

  /* applied on tokenised code */

  @tailrec
  final def countCommentLines(tokens: List[Token], i: Int = 0): Int = {
    tokens match {
      case Nil => i
      case token :: tail if token.tokenType.isComment => {
          token.tokenType match {
            case LINE_COMMENT =>
              countCommentLines(tail, i + 1)
            case MULTILINE_COMMENT =>
              countCommentLines(tail, i + token.rawText.count(_ == '\n') + 1)
            case XML_COMMENT =>
              new scala.NotImplementedError("XML ?!"); i
          }
        }
      case _ :: tail => countCommentLines(tail, i)
    }
  }

  @tailrec
  final def countClasses(tokens: List[Token], i: Int = 0): Int = {
    tokens match {
      case Nil => i
      case token :: tail if token.tokenType == Tokens.CLASS => countClasses(tail, i + 1)
      case _ :: tail => countClasses(tail, i)
    }
  }

  @tailrec
  final def countFunctions(tokens: List[Token], i: Int = 0): Int = {
    tokens match {
      case Nil => i
      case token :: tail if token.tokenType == Tokens.DEF => countFunctions(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.CLASS => countFunctions(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.OBJECT => countFunctions(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.TRAIT => countFunctions(tail, i + 1)
      case _ :: tail => countFunctions(tail, i)
    }
  }

  @tailrec
  final def countNCLoC(tokens: List[Token], i: Int = 0): Int = {

    @tailrec
    def getNextLine(tokens: List[Token]): List[Token] = {
      tokens match {
        case Nil => Nil
        case token :: tail if token.tokenType == WS &&
                              token.text.contains('\n') => tail
        case token :: tail if token.tokenType == LINE_COMMENT => tail
        case token :: tail => getNextLine(tail)
      }
    }

    tokens match {
      case Nil => i
      case token :: tail if token.tokenType == WS => countNCLoC(tail, i)
      case token :: tail if token.tokenType == EOF => i
      case token :: tail =>
        if( !token.tokenType.isNewline & !token.tokenType.isComment) {
          countNCLoC(getNextLine(tail), i + 1)
        } else {
          countNCLoC(tail, i)
        }
    }
  }

  @tailrec
  final def calculateComplexity(tokens: List[Token], i: Int = 0): Int = {
    tokens match {
      case Nil => i

      case token :: tail if token.tokenType == Tokens.CASE => calculateComplexity(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.DO => calculateComplexity(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.FOR => calculateComplexity(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.FORSOME => calculateComplexity(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.IF => calculateComplexity(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.WHILE => calculateComplexity(tail, i + 1)

      case token :: tail if token.tokenType == Tokens.DEF => calculateComplexity(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.TRAIT => calculateComplexity(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.CLASS => calculateComplexity(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.OBJECT => calculateComplexity(tail, i + 1)

      case _ :: tail => calculateComplexity(tail, i)
    }
  }
}
