package scala
package util.parsing.bnf

import scala.util.parsing.combinator._

object BNF extends RegexParsers {

  override val skipWhitespace = false
  // override val whiteSpace = "".r

  /**
   * syntax     ::=  { rule }
   * rule       ::=  identifier  "::="  expression
   * expression ::=  term { "|" term }
   * term       ::=  factor { factor }
   * factor     ::=  identifier | quoted_symbol | "("  expression  ")" | "["  expression  "]" | "{"  expression  "}"
   * identifier ::=  letter { letter | digit }
   * quoted_symbol ::= """ { any_character } """
   */

  def syntax = rule.*
  def rule = identifier ~ "::=" ~ expression
  def expression: Parser[List[java.io.Serializable]] = repsep(term, "|")
  def term = factor.+
  def factor = (identifier | quoted_symbol | "(" ~ expression ~ ")" | "[" ~ expression ~ "]" | "{" ~ expression ~ "}")
  def identifier = rep(space) ~> letter ~ (letter | digit).* <~ rep(space)
  def quoted_symbol = rep(space) ~> "\"" ~> any_character <~ "\"" <~ rep(space)
  def any_character = ".".r
  def letter = lower | upper
  def lower = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
  def upper = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
  def digit = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"
  def space = " "
  def parse(input: String) = parseAll(syntax, input)
}
