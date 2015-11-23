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
  def letter = "[a-zA-Z]".r
  def digit = "[0-9]".r
  def space = " ".r
  def parse(input: String) = parseAll(syntax, input)
}
