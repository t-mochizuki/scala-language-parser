package scala.util.parsing.bnf

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers

class BNFSpec extends FunSpec with MustMatchers {

  describe("parse") {
    it("parse BNF rules of LISP") {
      BNF.parse("empty ::= \" \"") must not be ""
      BNF.parse("""empty ::= " """") must not be ""
      BNF.parse("""number ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"""") must not be ""
      BNF.parse("""letter ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"""") must not be ""
      BNF.parse("atom ::= empty | letter atom | number atom") must not be ""
      BNF.parse("atomic ::= letter atom") must not be ""
      BNF.parse("""list ::= "(" expr {expr} ")"""") must not be ""
      BNF.parse("""expr ::= atomic | "(" expr "." expr ")" | list""") must not be ""
    }
  }
}
