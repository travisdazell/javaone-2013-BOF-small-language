package net.travisdazell.parsers.runtime
import net.travisdazell.parsers.model._

class Interpreter(program: Program) {
  var currentScope = new Scope("global", null)

  def run() {
    walk(program.statements)
  }

  private def getVariable(ident: Identifier): Expr = {
    var s: Scope = currentScope

    while ((!s.name.equals("global")) && !s.variables.contains(ident.name)) s = s.parentScope

    if (s.variables.contains(ident.name)) s.variables(ident.name)
    else {
      sys.error("Error: Undefined variable " + ident.name +
        " at position [" +
        ident.pos.column + "] on line: " +
        ident.pos.line)
    }
  }

  private def calculateExpr(e: Expr): Int = {
    e match {
      case Number(value) => value
      case Identifier(name) => {
        calculateExpr(getVariable(e.asInstanceOf[Identifier]))
      }
      case Operator(op, left, right) => {
        op match {
          case "*" => calculateExpr(left) * calculateExpr(right)
          case "/" => calculateExpr(left) / calculateExpr(right)
          case "%" => calculateExpr(left) % calculateExpr(right)
          case "+" => calculateExpr(left) + calculateExpr(right)
          case "-" => calculateExpr(left) - calculateExpr(right)
        }
      }
    }
  }

  private def isConditionTrue(condition: Condition): Boolean = {
    val a = calculateExpr(condition.left)
    val b = calculateExpr(condition.right)

    condition.op match {
      case "==" => (a == b)
      case "!=" => (a != b)
      case "<=" => (a <= b)
      case "<" => (a < b)
      case ">=" => (a >= b)
      case ">" => (a > b)
    }
  }

  private def executeFunction(f: Function, arguments: Map[String, Expr]) {
    currentScope = new Scope(f.name, currentScope)

    for (v <- arguments) currentScope.variables(v._1) = v._2

    walk(f.statements)

    currentScope = currentScope.parentScope
  }

  private def walk(tree: List[Statement]) {
    if (!tree.isEmpty) {
      tree.head match {
        case FunctionCall(name, values) => {
          val f = program.functions.filter(x => x.name == name)

          if (f.size < 1) sys.error("Error: Undefined function '" +
            name + "' being called at position [" +
            tree.head.pos.column + "] on line: " +
            tree.head.pos.line)
          else {
            executeFunction(f(0), values)

            walk(tree.tail)
          }
        }
        case VariableDefinition(name, value) => {
          // push this variable into scope
          if (value.isInstanceOf[FunctionCall]) {
            val functionCall = value.asInstanceOf[FunctionCall]
            val function = program.functions.filter(x => x.name == functionCall.name)

            // check if proc is defined and if not throw an error
            if (function.size < 1) sys.error("Error: Undefined function '" +
              functionCall.name + "' being called at position [" +
              tree.head.pos.column + "] on line: " +
              tree.head.pos.line)
            else {
              executeFunction(function(0), functionCall.values)

              currentScope = currentScope.parentScope

              // assign the return value of the function to the variable
              currentScope.variables(name) = function(0).returnValue
            }
          } else {
            currentScope.variables(name) = value
          }

          walk(tree.tail)
        }
        case PrintStatement(value) => {
          println(calculateExpr(value))
          walk(tree.tail)
        }
        case LoopStatement(iterations, statements) => {
          currentScope = new Scope("", currentScope)

          for (i <- 0 until iterations) walk(statements)

          currentScope = currentScope.parentScope

          walk(tree.tail)
        }
        case IfStatement(condition, trueBranch, falseBranch) => {
          currentScope = new Scope("", currentScope)

          if (isConditionTrue(condition)) walk(trueBranch) else walk(falseBranch)

          currentScope = currentScope.parentScope

          walk(tree.tail)
        }
        case _ => ()
      }
    }
  }
}