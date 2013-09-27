package net.travisdazell.parsers.model

import scala.collection.mutable.HashMap
import scala.util.parsing.input.Positional

case class FunctionCall(name: String, values: Map[String, Expr]) extends Expr with Statement