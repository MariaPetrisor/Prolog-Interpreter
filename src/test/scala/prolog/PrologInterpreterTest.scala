package prolog

import org.scalatest.FunSpec

class PrologInterpreterTest extends FunSpec {

  describe("Yes test") {
    it("simple yes") {

      /*
      drinks(mcnulty, jameson).
      ?- drinks(mcnulty, jameson).
      yes.
      */

      val expected = Yes(substitution = Substitution(Map.empty))

      val program = PrologInterpreter

      val prologProgram = Fact(Fun("drinks", List[Term](Var(Identifier(0, "mcnulty")), Var(Identifier(1, "jameson")))))
      val goal = Fun("drinks", List[Term](Var(Identifier(0, "mcnulty")), Var(Identifier(1, "jameson"))))

      val result = program.prolog(List(prologProgram), goal)
      assert(expected == result)
    }

    it("substitution yes") {

      /*
      drinks(mcnulty, jameson).
      ?- drinks(mcnulty, X).
      X = jameson.
      */

      val expected = Yes(substitution = Substitution(Map(Identifier(1, "X") -> Var(Identifier(1, "jameson")))))

      val program = PrologInterpreter

      val prologProgram = Fact(Fun("drinks", List[Term](Var(Identifier(0, "mcnulty")), Var(Identifier(1, "jameson")))))
      val goal = Fun("drinks", List[Term](Var(Identifier(0, "mcnulty")), Var(Identifier(1, "X"))))

      val result = program.prolog(List(prologProgram), goal)
      assert(expected == result)
    }


    it("substitution yes -- with 2 substitutions") {
      /*
      drinks(mcnulty, jameson).
      ?- drinks(X, Y).
      X = mcnulty.
      Y = jameson.
      */

      val expected = Yes(substitution = Substitution(Map(Identifier(1, "Y") -> Var(Identifier(1, "y")),
        Identifier(0, "X") -> Var(Identifier(0, "x")))))

      val program = PrologInterpreter
      val prologProgram = Fact(Fun("Suit", List[Term](Var(Identifier(0, "x")), Var(Identifier(1, "y")))))
      val goal = Fun("Suit", List[Term](Var(Identifier(0, "X")), Var(Identifier(1, "Y"))))
      val result = program.prolog(List(prologProgram), goal)

      assert(expected == result)
    }

    it("rule yes -- with substitution") {
      /*
      wearsSuits(bunk).

      wearsTies(bunk).

      suitAndTieMotherfucker(X) :-
        wearsSuits(X),
        wearsTies(X).

      ?- suitAndTieMotherfucker(X).
      X = bunk.
      */
      val expected = Yes(substitution = Substitution(Map(Identifier(0, "X") -> Var(Identifier(0, "bunk")))))

      val program = PrologInterpreter

      val firstFact = Fact(Fun("wearsSuits", List[Term](Var(Identifier(0, "bunk")))))
      val secondFact = Fact(Fun("wearsTies", List[Term](Var(Identifier(0, "bunk")))))

      val firstPredicate = Fun("wearsSuits", List[Term](Var(Identifier(0, "X"))))
      val secondPredicate = Fun("wearsTies", List[Term](Var(Identifier(0, "X"))))
      val term = Fun("suitAndTieMotherfucker", List[Term](Var(Identifier(0, "X"))))
      val rule = Rule(term, List[Term](firstPredicate, secondPredicate))

      val goal = Fun("suitAndTieMotherfucker", List[Term](Var(Identifier(0, "X"))))

      val result = program.prolog(List(firstFact, secondFact, rule), goal)
      assert(expected == result)
    }

    it("rule true") {
      /*
      wearsSuits(bunk).

      wearsTies(bunk).

      suitAndTieMotherfucker(X) :-
        wearsSuits(X),
        wearsTies(X).

      ?- suitAndTieMotherfucker(bunk).
      yes.
*/

      val expected = Yes(Substitution(Map.empty))

      val program = PrologInterpreter

      val firstFact = Fact(Fun("wearsSuits", List[Term](Var(Identifier(0, "bunk")))))
      val secondFact = Fact(Fun("wearsTies", List[Term](Var(Identifier(0, "bunk")))))

      val firstPredicate = Fun("wearsSuits", List[Term](Var(Identifier(0, "X"))))
      val secondPredicate = Fun("wearsTies", List[Term](Var(Identifier(0, "X"))))
      val term = Fun("suitAndTieMotherfucker", List[Term](Var(Identifier(0, "X"))))
      val rule = Rule(term, List[Term](firstPredicate, secondPredicate))

      val goal = Fun("suitAndTieMotherfucker", List[Term](Var(Identifier(0, "bunk"))))

      val result = program.prolog(List(firstFact, secondFact, rule), goal)
      assert(expected == result)
    }


  }

  describe("No test") {
    it("simple no") {

      /*
      drinks(mcnulty, jameson).
      ?- drinks(mcnulty, jackdaniels).
      no.
      */
      val expected = No()

      val program = PrologInterpreter
      val prologProgram = Fact(Fun("drinks", List[Term](Var(Identifier(0, "mcnulty")), Var(Identifier(1, "jameson")))))
      val goal = Fun("drinks", List[Term](Var(Identifier(0, "mcnulty")), Var(Identifier(1, "jackdaniels"))))
      val result = program.prolog(List(prologProgram), goal)
      assert(expected == result)
    }

    it("simple no -- Fun not defined") {

      /*
      drinks(mcnulty, jameson).
      ?- dri(mcnulty, jameson).
      no.
      */
      val expected = No()

      val program = PrologInterpreter
      val prologProgram = Fact(Fun("drinks", List[Term](Var(Identifier(0, "mcnulty")), Var(Identifier(1, "jameson")))))
      val goal = Fun("dri", List[Term](Var(Identifier(0, "mcnulty")), Var(Identifier(1, "jameson"))))
      val result = program.prolog(List(prologProgram), goal)
      assert(expected == result)
    }

    it("rule no") {
      /*
      wearsSuits(bunk).

      wearsTies(bunk).

      suitAndTieMotherfucker(X) :-
        wearsSuits(X),
        wearsTies(X).

      ?- suitAndTieMotherfucker(mcnulty).
      no.
      */
      val expected = No()

      val program = PrologInterpreter

      val firstFact = Fact(Fun("wearsSuits", List[Term](Var(Identifier(0, "bunk")))))
      val secondFact = Fact(Fun("wearsTies", List[Term](Var(Identifier(0, "bunk")))))

      val firstPredicate = Fun("wearsSuits", List[Term](Var(Identifier(0, "X"))))
      val secondPredicate = Fun("wearsTies", List[Term](Var(Identifier(0, "X"))))
      val term = Fun("suitAndTieMotherfucker", List[Term](Var(Identifier(0, "X"))))
      val rule = Rule(term, List[Term](firstPredicate, secondPredicate))

      val goal = Fun("suitAndTieMotherfucker", List[Term](Var(Identifier(0, "mcnulty"))))

      val result = program.prolog(List(firstFact, secondFact, rule), goal)
      assert(expected == result)
    }
  }

}
