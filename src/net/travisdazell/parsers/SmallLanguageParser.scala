package net.travisdazell.parsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import net.travisdazell.parsers.model._

// small interpreted language with the following features:
// 		- variable definitions and references
//		- if-else statements
//		- loops
//		- error handling
//		- scoping
//		- functions with named arguments
class SmallLanguageParser extends StandardTokenParsers {
  lexical.reserved += ("var", "println", "loop", "times", "endloop", "if", "then", "else", "endif", "func", "return", "endfunc", "main")
  lexical.delimiters += ("*", "/", "%", "+", "-", "(", ")", "=", "<", ">", "==", "!=", "<=", ">=", ",", ":")

  def program: Parser[Program] = (rep(function) <~ ("main" ~ ":")) ~ codeblock ^^ {
    case f ~ c => new Program(f, c)
  }

  def function: Parser[Function] = ("func" ~> ident) ~ ("(" ~> arguments) ~ (")" ~> codeblock) ~ opt(returnStatement) <~ "endfunc" ^^ {
    case a ~ b ~ c ~ None => new Function(a, b, c, Number(0))
    case a ~ b ~ c ~ d => new Function(a, b, c, d.get)
  }
  
  def returnStatement: Parser[Expr] = "return" ~> expr ^^ {
    e => e
  }

  def arguments: Parser[Map[String, Int]] = repsep(ident, ",") ^^ {
    argumentList => {
    	(for (a <- argumentList) yield (a -> 0)) toMap
    }
  }

  def codeblock: Parser[List[Statement]] = rep(statement) ^^ { a => a }

  def statement: Parser[Statement] = positioned(variableAssignment | outStatement | loopStatement | ifStatement | functionCall | outStatement) ^^ { a => a }

  def variableAssignment: Parser[VariableDefinition] = "var" ~> ident ~ "=" ~ positioned(functionCall | expr) ^^ { case a ~ "=" ~ b => { new VariableDefinition(a, b) } }

  def outStatement: Parser[PrintStatement] = "println" ~> positioned(expr) ^^ { case a => new PrintStatement(a) }

  def loopStatement: Parser[LoopStatement] = ("loop" ~> iterations <~ "times") ~ codeblock <~ "endloop" ^^ {
    case i ~ s => {
      new LoopStatement(i, s)
    }
  }

  def ifStatement: Parser[IfStatement] = conditional ~ codeblock ~ opt("else" ~> codeblock) <~ "endif" ^^ {
    case a ~ b ~ c => {
      c match {
        case None => new IfStatement(a, b, List())
        case _ => new IfStatement(a, b, c.get)
      }
    }
  }

  def conditional: Parser[Condition] = "if" ~ "(" ~> condition <~ ")" ~ "then"

  def condition: Parser[Condition] = positioned(expr) ~ ("<" | ">" | "==" | "!=" | "<=" | ">=") ~ positioned(expr) ^^ {
    case a ~ b ~ c => {
      new Condition(b, a, c)
    }
  }

  def iterations: Parser[Int] = numericLit ^^ { _ toInt }

  def functionCall: Parser[FunctionCall] = ((ident) <~ "(") ~ functionCallArguments <~ ")" ^^ {
    case a ~ l => new FunctionCall(a, l)
  }

  def functionCallArguments: Parser[Map[String, Expr]] = repsep(functionArgument, ",") ^^ {
    _ toMap
  }

  def functionArgument: Parser[(String, Expr)] = (ident <~ "=") ~ expr ^^ {
    case a ~ b => (a, b)
  }

  def expr: Parser[Expr] = term ~ rep(("+" | "-") ~ term) ^^ {
    case a ~ List() => a
    case a ~ b => {
      def appendExpression(c: Operator, p: Operator): Operator = {
        p.left = c
        p
      }

      var root: Operator = new Operator(b.head._1, a, b.head._2)

      for (f <- b.tail) {
        var parent =
          f._1 match {
            case "+" => new Operator("+", null, f._2)
            case "-" => Operator("-", null, f._2)
          }

        root = appendExpression(root, parent)
      }

      root
    }
  }

  def term: Parser[Expr] = multiplydividemodulo ^^ { l => l } | factor ^^ {
    a => a
  }

  // note that "rep" returns a List
  def multiplydividemodulo: Parser[Expr] = factor ~ rep(("*" | "/" | "%") ~ factor) ^^ {

    case a ~ List() => a
    case a ~ b => {
      def appendExpression(e: Operator, t: Operator): Operator = {
        t.left = e.right
        e.right = t
        t
      }

      var root: Operator = new Operator(b.head._1, a, b.head._2)
      var current = root

      // for each of these, i'm just building up the parse tree
      for (f <- b.tail) {
        var rightOperator =
          f._1 match {
            case "*" => Operator("*", null, f._2)
            case "/" => Operator("/", null, f._2)
            case "%" => Operator("%", null, f._2)
          }

        current = appendExpression(current, rightOperator)
      }

      root
    }
  }

  def factor: Parser[Expr] = numericLit ^^ { a => Number(a.toInt) } |
    "(" ~> expr <~ ")" ^^ { e => e } |
    ident ^^ { new Identifier(_) }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }
}