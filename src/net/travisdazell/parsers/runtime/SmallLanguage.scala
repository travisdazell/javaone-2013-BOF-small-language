package net.travisdazell.parsers.runtime

import scala.io.Source

import net.travisdazell.parsers.SmallLanguageParser

object SmallLanguage {
  def main(args: Array[String]) {
    val inputFile = Source.fromFile("scripts/program.small")
    val inputSource = inputFile.mkString

    val parser = new SmallLanguageParser
    parser.parseAll(parser.program, inputSource) match {
      case parser.Success(r, n) => {
        val interpreter = new Interpreter(r)

        try {
          interpreter.run
        } catch {
          case e: RuntimeException => println(e.getMessage)
        }
      }
      case parser.Error(msg, n) => println("Error: " + msg)
      case parser.Failure(msg, n) => println("Error: " + msg)
      case _ =>
    }
  }
}