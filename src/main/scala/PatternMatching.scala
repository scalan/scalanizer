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

  def updateMatchingState(implicit matchCase: SCase, state: MatchingState): MatchingState = {
    matchCase.pat match {
      case SWildcardPattern() => getCaseBody
      case SLiteralPattern(const @ SConst(_)) => eqCheck(const)
      case SStableIdPattern(id @ SIdent(_)) => eqCheck(id)
      case SSelPattern(sel, name) => eqCheck(SSelect(sel, name))
      case SBindPattern(name, SWildcardPattern()) => bindVal(name)
//      case SAltPattern(alts) => throw new NotImplementedError("Alternative pattern is not supported.")
//      case STypedPattern(tpe) => typeCheck(tpe)
//      case SBindPattern(name, STypedPattern(tpe)) => typeCheckAndBind(name, tpe)
//      case SApplyPattern(fun, args) => extractor(fun, args)
      case _ => throw new NotImplementedError(s"updateMatchingState: matchCase = $matchCase")
    }
  }

  def getCaseBody(implicit matchCase: SCase, state: MatchingState) = {
    MatchingState(
      selector = state.selector,
      condExpr = SEmpty(), thenExpr = matchCase.body, elseExpr = SEmpty()
    )
  }

  def eqCheck(eqExpr: SExpr)(implicit matchCase: SCase, state: MatchingState) = {
    MatchingState(
      selector = state.selector,
      condExpr = isEqual(state.selector, eqExpr),
      thenExpr = matchCase.body,
      elseExpr = state.get
    )
  }

  def bindVal(name: String)(implicit matchCase: SCase, state: MatchingState) = {
    MatchingState(
      selector = state.selector,
      condExpr = SEmpty(),
      thenExpr = injectAlias(name, state.selector, matchCase.body),
      elseExpr = SEmpty()
    )
  }

  def isEqual(left: SExpr, right: SExpr) = {
    SApply(SSelect(left, "=="), List(), List(List(right)))
  }

  def makeAlias(name:String, right: SExpr) = {
    SValDef(name, None, false, false, right)
  }

  def injectAlias(name: String, target: SExpr, to: SExpr) = {
    val alias = makeAlias(name, target)

    SBlock(List(alias), to)
  }
}
