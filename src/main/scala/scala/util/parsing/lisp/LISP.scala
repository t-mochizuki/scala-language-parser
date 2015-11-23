package scala
package util.parsing.lisp

import scala.util.parsing.combinator._

object LISP extends RegexParsers {

  override val skipWhitespace = false

  /**
   * s_exp      ::= atom | list
   * atom       ::= number | symbol | string
   * list       ::= () | "(" s_exp_list ")" | "(" s_exp_list "." s_exp ")" | "\'" s_exp | "#\'" s_exp
   * s_exp_list ::= s_exp | s_exp_list s_exp
   * string     ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
   * number     ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"
   * symbol     ::= "+" | "-" | "*" | "/"
   */

  def s_exp: Parser[Any] = atom | list | space
  def atom = number | symbol | string
  def list = "(" ~ ")" | "(" ~ s_exp_list ~ ")" | "\'" ~ s_exp | sharp_quote ~ s_exp
  def s_exp_list: Parser[Any] = rep(s_exp)
  def number = ("1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0").+
  def symbol = "+" | "-" | "*" | "/"
  def string = ("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z").+
  def sharp_quote = "#" ~ "\'"
  def space = " "
  def parse(input: String) = parseAll(s_exp, input)
}

