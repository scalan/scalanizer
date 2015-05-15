package scalan.plugin

import scalan.meta.ScalanAst._

trait PatternMatching {

  case class MatchingState(selector: SExpr,
                            condExpr: SExpr, thenExpr: SExpr, elseExpr: SExpr) {
    def get: SExpr = condExpr match {
      case SEmpty() => thenExpr
      case _ => SIf(condExpr, thenExpr, elseExpr)
    }
  }

  def transformPatternMatching(patternMatch: SMatch): SExpr = {
    val initState = MatchingState(
      selector = patternMatch.selector,
      condExpr = SEmpty(), thenExpr = SEmpty(), elseExpr = SEmpty()
    )

    val finalState = patternMatch.cases.foldRight(initState){
      (matchCase: SCase, state: MatchingState) => updateMatchingState(matchCase, state)
    }

    finalState.get
  }

  def updateMatchingState(matchCase: SCase, state: MatchingState): MatchingState = {
    matchCase.pat match {
      case SWildcardPattern() => getCaseBody(matchCase, state)
//      case SBindPattern(name, SWildcardPattern()) => addAlias(name)
      case SLiteralPattern(const @ SConst(_)) => eqCheck(matchCase, state, const)
      case SStableIdPattern(id @ SIdent(_)) => eqCheck(matchCase, state, id)
      case SSelPattern(sel, name) => eqCheck(matchCase, state, SSelect(sel, name))
//      case SAltPattern(alts) => throw new NotImplementedError("Alternative pattern is not supported.")
//      case STypedPattern(tpe) => typeCheck(tpe)
//      case SBindPattern(name, STypedPattern(tpe)) => typeCheckAndBind(name, tpe)
//      case SApplyPattern(fun, args) => extractor(fun, args)
      case _ => throw new NotImplementedError(s"updateMatchingState: matchCase = $matchCase")
    }
  }

  def getCaseBody(matchCase: SCase, state: MatchingState): MatchingState = {
    MatchingState(
      selector = state.selector,
      condExpr = SEmpty(), thenExpr = matchCase.body, elseExpr = SEmpty()
    )
  }

  def eqCheck(matchCase: SCase, state: MatchingState, eqExpr: SExpr): MatchingState = {
    MatchingState(
      selector = state.selector,
      condExpr = isEqual(state.selector, eqExpr),
      thenExpr = matchCase.body,
      elseExpr = state.get
    )
  }

  def isEqual(left: SExpr, right: SExpr): SExpr = {
    SApply(SSelect(left, "=="), List(), List(List(right)))
  }
}
