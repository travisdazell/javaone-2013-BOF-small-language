package net.travisdazell.parsers.model

import scala.collection.mutable.HashMap

class Scope(val name: String, val parentScope: Scope) {
	var variables = new HashMap[String, Expr]
}