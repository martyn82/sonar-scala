package com.sagacify.sonar.scala

import scala.annotation.tailrec
import scalariform.lexer.{Token, Tokens}
import scalariform.lexer.Tokens.LINE_COMMENT
import scalariform.lexer.Tokens.MULTILINE_COMMENT
import scalariform.lexer.Tokens.XML_COMMENT
import scalariform.lexer.Tokens.WS
import scalariform.lexer.Tokens.EOF
import scalariform.parser._

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
      case token :: tail if token.tokenType == Tokens.OBJECT => countClasses(tail, i + 1)
      case token :: tail if token.tokenType == Tokens.TRAIT => countClasses(tail, i + 1)
      case _ :: tail => countClasses(tail, i)
    }
  }

  @tailrec
  final def countNCLoC(tokens: List[Token], i: Int = 0): Int = {
    @tailrec
    def getNextLine(tokens: List[Token]): List[Token] = {
      tokens match {
        case Nil => Nil
        case token :: tail if token.tokenType == WS && token.text.contains('\n') => tail
        case token :: tail if token.tokenType == LINE_COMMENT => tail
        case token :: tail => getNextLine(tail)
      }
    }

    tokens match {
      case Nil => i
      case token :: tail if token.tokenType == WS => countNCLoC(tail, i)
      case token :: tail if token.tokenType == EOF => i
      case token :: tail =>
        if (!token.tokenType.isNewline & !token.tokenType.isComment) {
          countNCLoC(getNextLine(tail), i + 1)
        } else {
          countNCLoC(tail, i)
        }
    }
  }

  private def isFunctionNode(node: AstNode): Boolean = {
    node match {
      case _: FunDefOrDcl => true
      case _ => false
    }
  }

  private def isClassNode(node: AstNode): Boolean = {
    node match {
      case n: FullDefOrDcl if n.firstToken.tokenType == Tokens.CLASS || n.firstToken.tokenType == Tokens.TRAIT
        || n.firstToken.tokenType == Tokens.OBJECT || n.firstToken.tokenType == Tokens.CASE => true
      case _ => false
    }
  }

  final def extractClasses(ast: AstNode): List[AstNode] = {
    def innerExtractClasses(ast: AstNode, classes: List[AstNode]): List[AstNode] = {
      ast.immediateChildren.foldLeft(classes) { (classes, node) =>
        node match {
          case n if n.isEmpty => classes
          case n if isClassNode(n) => innerExtractClasses(n, n :: classes)
          case n => innerExtractClasses(n, classes)
        }
      }
    }

    ast match {
      case n if isClassNode(n) => innerExtractClasses(ast, n :: List.empty[AstNode])
      case _ => innerExtractClasses(ast, List.empty[AstNode])
    }
  }

  final def extractFunctions(ast: AstNode): List[AstNode] = {
    def innerExtractFunctions(ast: AstNode, functions: List[AstNode]): List[AstNode] = {
      ast.immediateChildren.foldLeft(functions) { (functions, node) =>
        node match {
          case n if n.isEmpty => functions
          case n if isFunctionNode(n) => innerExtractFunctions(n, n :: functions)
          case n => innerExtractFunctions(n, functions)
        }
      }
    }

    ast match {
      case n if isFunctionNode(n) => innerExtractFunctions(ast, n :: List.empty[AstNode])
      case _ => innerExtractFunctions(ast, List.empty[AstNode])
    }
  }

  final def calculateComplexity(ast: AstNode): Int = {
    @tailrec
    def innerCalculateComplexity(node: AstNode, i: Int = 0): Int = {
      node match {
        case IfExpr(_, _, _, body, None)              => innerCalculateComplexity(body, i + 1)
        case IfExpr(_, _, _, body, Some(elseClause))  => calculateBranches(body, elseClause.elseBody, i)
        case CaseClause(_, stats)                     => innerCalculateComplexity(stats, i + 1)
        case WhileExpr(_, _, _, body)                 => innerCalculateComplexity(body, i + 1)
        case DoExpr(_, body, _, _, _)                 => innerCalculateComplexity(body, i + 1)
        case ForExpr(_, _, _, _, _, _, body)          => innerCalculateComplexity(body, i + 1)
        case _: FunDefOrDcl                           => i
        case n                                        => calculateChildren(n, i)
      }
    }

    def calculateBranches(branchA: AstNode, branchB: AstNode, i: Int): Int =
      innerCalculateComplexity(branchA, i + 1) + innerCalculateComplexity(branchB, i)

    def calculateChildren(n: AstNode, i: Int): Int =
      n.immediateChildren.map(n => innerCalculateComplexity(n)).sum + i

    ast match {
      case FunDefOrDcl(_, _, _, _, _, Some(body), _)  => innerCalculateComplexity(body, 1)
      case FullDefOrDcl(_, _, defOrDcl)               => innerCalculateComplexity(defOrDcl, 1)
      case _ => 0
    }
  }
}
