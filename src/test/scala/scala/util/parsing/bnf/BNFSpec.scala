package scala.util.parsing.bnf

import org.scalatest.FlatSpec

class BNFSpec extends FlatSpec {

  behavior of "parse"

  it should "parse BNF rules of LISP" in {
    BNF.parse("empty ::= \" \"")
    BNF.parse("""empty ::= " """")
    BNF.parse("""number ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"""")
    BNF.parse("""letter ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"""")
    BNF.parse("atom ::= empty | letter atom | number atom")
    BNF.parse("atomic ::= letter atom")
    BNF.parse("""list ::= "(" expr {expr} ")"""")
    BNF.parse("""expr ::= atomic | "(" expr "." expr ")" | list""")
  }
}
