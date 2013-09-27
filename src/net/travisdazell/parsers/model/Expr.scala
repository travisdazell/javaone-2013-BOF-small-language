package net.travisdazell.parsers.model

import scala.util.parsing.input.Positional

class Expr extends Positional

case class Number(value: Int) extends Expr

case class Operator(op: String, var left: Expr, var right: Expr) extends Expr

case class Identifier(name: String) extends Expr