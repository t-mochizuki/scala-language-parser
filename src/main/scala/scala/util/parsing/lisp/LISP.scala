package scala
package util.parsing.lisp

import scala.util.parsing.combinator._

object LISP extends RegexParsers {

  override val skipWhitespace = false

  /**
   * s_expression ::= atomic_symbol | "(" s_expression "." s_expression ")" | list
   * list ::= "(" s_expression {s_expression} ")"
   * atomic_symbol ::= letter atom_part
   * atom_part ::= empty | letter atom_part | number atom_part
   * letter ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
   * number ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"
   * empty ::= " "
   */

  def s_expression: Parser[Any] = (atomic_symbol | "(" ~ s_expression ~ rep(empty) ~ "." ~ rep(empty) ~ s_expression ~ ")" | list)
  def list = "(" ~ s_expression ~ rep(s_expression) ~ ")"
  def atomic_symbol = letter ~ opt(atom_part)
  def atom_part: Parser[Any] = (empty | letter ~ opt(atom_part) | number ~ opt(atom_part))
  def letter = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
  def number = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"
  def empty = " "
  def parse(input: String) = parseAll(s_expression, input)
}

