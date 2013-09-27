package net.travisdazell.parsers.model

case class LoopStatement(times: Int, statements: List[Statement]) extends Statement