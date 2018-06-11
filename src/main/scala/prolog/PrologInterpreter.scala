package prolog

case class Identifier(int: Int, string: String)

sealed trait Term
case class Var(identifier: Identifier) extends Term
case class Fun(string: String, terms: List[Term]) extends Term

sealed trait Clause
case class Rule(head: Term, body: List[Term]) extends Clause
case class Fact(term: Term) extends Clause

case class PrologProgram(clauses: List[Clause])

case class Substitution(map: Map[Identifier, Term])

sealed trait Answer
case class No() extends Answer
case class Yes(substitution: Substitution) extends Answer

object PrologInterpreter extends App{

  def isVariable(string: String): Boolean ={
    string.charAt(0).isUpper
  }

  def addToEnv(env: Map[String, List[Term]], program: List[Clause]): Map[String, List[Term]] = program match {
    case Nil => env
    case Fact(Fun(name, terms)) :: tail => addToEnv(env + (name -> terms), tail)
    case Rule(Fun(name, _), body) :: tail => addToEnv(env + (name -> body), tail)
  }

  def checkLists(list1: List[Term], list2: List[Term], index: Int, not_matching: List[(Term, Term)]): List[(Term, Term)] = list1 match {
    case Nil => not_matching
    case Var(Identifier(int, string)) :: tail =>
      list2(index) match {
        case Var(Identifier(int2, string2)) =>
          if(int == int2 && string == string2){
            checkLists(tail, list2, index + 1, not_matching)
          }
          else{
            checkLists(tail, list2, index + 1, (Var(Identifier(int, string)), Var(Identifier(int2, string2))) :: not_matching)
          }
      }
  }

  def results(list: List[(Term, Term)], result: Map[Identifier, Term]): Map[Identifier, Term] = list match {
    case Nil => result
    case (Var(Identifier(int, string)), Var(Identifier(int2, string2))) :: tail =>
      if(isVariable(string2)){
        results(tail, result + (Identifier(int2, string2) -> Var(Identifier(int, string))))
      }
      else {
        results(tail, result)
      }
  }

  def checkForSubstitution(env_entry: List[Term], goal: Term): Map[Identifier, Term] = goal match {
    case Fun(name, terms) =>

        val not_matching = checkLists(env_entry, terms, 0, List.empty)

        val result = results(not_matching, Map.empty)
        if (result.size == not_matching.size){
          result
        }
        else{
          Map.empty
        }
  }

  def checkIfRule(body: List[Term]): Boolean = body match {
    case Nil => true
    case head::tail =>
      head match {
        case Var(_) => false
        case Fun(_, _) => checkIfRule(tail)
    }
  }

  def checkRulePredicates(program: List[Clause], env: Map[String, List[Term]], body: List[Term], substs: List[Substitution]): Answer = body match {
    case Nil =>
      if(substs.nonEmpty){
        Yes(substs.head)
      }
      else{
        Yes(Substitution(Map.empty))
      }
    case head :: tail =>
      val ans: Answer = prolog(program, head)

      ans match {
        case No() => No()
        case Yes(Substitution(map: Map[Identifier, Term])) =>
          if(map == Map.empty) {
            if(substs.nonEmpty){
              No()
            }
            else {

              checkRulePredicates(program, env, tail, substs)
            }
          }
          else{

            if(!substs.contains(Substitution(map)) && substs.nonEmpty){
              No()
            }
            else {

              checkRulePredicates(program, env, tail, Substitution(map) :: substs)
            }
          }

      }
  }


  def replaceVarsWithConstants(goalList: List[Term], envList: List[Term], newList: List[Term]): List[Term] = envList match{
    case Nil => newList
    case Fun(name, _)::tail =>
      replaceVarsWithConstants(goalList, tail, Fun(name, goalList) :: newList)
  }

  def prolog(program: List[Clause], goal: Term): Answer ={
    val env = addToEnv(Map.empty, program)
    goal match {
      case Fun(name, terms) =>
        //did not get any Fact containing a Fun with that name
        if(!env.contains(name)) {
          No()
        }
        else {
          //checks if the goal refers to a Fun defined as a rule
          if (!checkIfRule(env(name))) {
            //the terms from the Fact are the same as the goal's terms
            if (env(name).equals(terms)) {
              Yes(substitution = Substitution(Map.empty))
            }
            //if not, extract the possible substitutions
            else {
              val result = checkForSubstitution(env(name), goal)
              //if there are no possible substitutions return No
              if (result.isEmpty) {
                No()
              }
              else {
                //return yes and the corresponding substitutions
                Yes(Substitution(result))
              }
            }
          }
          //it's a goal based on a rule
          else{
            //replace the predicate term list with the same as we got in the goal
            //e.g. if we got suitAndTieMotherfucker(bunk), update the environment so that the we have wearsSuits(bunk)
            // and wearsTies(bunk) (instead of wearsSuits(X) and wearsTies(X) and then check each one is true)
            val newList = replaceVarsWithConstants(terms, env(name), List.empty)
            //apply program to every predicate from the rule
            checkRulePredicates(program, env, newList, List.empty)
          }
        }
    }
  }
}