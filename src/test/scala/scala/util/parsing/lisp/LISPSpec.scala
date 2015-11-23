package scala.util.parsing.lisp

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers

class LISPSpec extends FunSpec with MustMatchers {

  describe("LISP") {
    it("empty") {
      LISP.parseAll(LISP.empty, " ").toString mustBe "[1.2] parsed:  "
    }
    it("number") {
      LISP.parseAll(LISP.number, "1").toString mustBe "[1.2] parsed: 1"
      LISP.parseAll(LISP.number, "0").toString mustBe "[1.2] parsed: 0"
    }
    it("letter") {
      LISP.parseAll(LISP.letter, "a").toString mustBe "[1.2] parsed: a"
      LISP.parseAll(LISP.letter, "z").toString mustBe "[1.2] parsed: z"
    }
    it("atom_part") {
      LISP.parseAll(LISP.atom_part, " ").toString mustBe "[1.2] parsed:  "
      LISP.parseAll(LISP.atom_part, "z").toString mustBe "[1.2] parsed: (z~None)"
      LISP.parseAll(LISP.atom_part, "zz").toString mustBe "[1.3] parsed: (z~Some((z~None)))"
      LISP.parseAll(LISP.atom_part, "0").toString mustBe "[1.2] parsed: (0~None)"
      LISP.parseAll(LISP.atom_part, "00").toString mustBe "[1.3] parsed: (0~Some((0~None)))"
    }
    it("s_expression") {
      LISP.parseAll(LISP.s_expression, "abc").toString mustBe "[1.4] parsed: (a~Some((b~Some((c~None)))))"
      LISP.parseAll(LISP.s_expression, "(abc . def)").toString mustBe "[1.12] parsed: (((((((~(a~Some((b~Some((c~Some( )))))))~List())~.)~List( ))~(d~Some((e~Some((f~None))))))~))"
      LISP.parseAll(LISP.s_expression, "(abc def ghi)").toString mustBe "[1.14] parsed: ((((~(a~Some((b~Some((c~Some( )))))))~List((d~Some((e~Some((f~Some( )))))), (g~Some((h~Some((i~None)))))))~))"
    }
  }
}

