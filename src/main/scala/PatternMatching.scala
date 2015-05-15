package scalan.plugin

import scalan.meta.ScalanAst._

trait PatternMatching {

  case class MatchingState(selector: SExpr,
                            condExpr: SExpr, thenExpr: SExpr, elseExpr: SExpr,
                            forAll: Boolean = false) {
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
      case SWildcardPattern() => skipPattern
      case SLiteralPattern(const @ SConst(_)) => eqCheck(const)
      case SStableIdPattern(id @ SIdent(_)) => eqCheck(id)
      case SSelPattern(sel, name) => eqCheck(SSelect(sel, name))
      case SBindPattern(name, SWildcardPattern()) => bindVal(name)
      case STypedPattern(tpe) => typeCheck(tpe)
      case SBindPattern(name, STypedPattern(tpe)) => typeCheckAndBind(name, tpe)
      case SAltPattern(alts) => alternatives(alts)
      case SApplyPattern(fun, args) => extractor(fun, args)
      case _ => throw new NotImplementedError(s"updateMatchingState: matchCase = $matchCase")
    }
  }

  def skipPattern(implicit matchCase: SCase, state: MatchingState) = {
    if (state.forAll)
      state.copy(condExpr = SEmpty(), thenExpr = state.get)
    else
      state.copy(condExpr = SEmpty(), thenExpr = matchCase.body)
  }

  def eqCheck(eqExpr: SExpr)(implicit matchCase: SCase, state: MatchingState) = {
    checkCond(isEqual(state.selector, eqExpr))
  }

  def bindVal(name: String)(implicit matchCase: SCase, state: MatchingState) = {
    def injectTo(target: SExpr) = injectAlias(name, state.selector, target)

    if (state.forAll)
      state.copy(condExpr = SEmpty(), thenExpr = injectTo(state.get))
    else
      state.copy(condExpr = SEmpty(), thenExpr = injectTo(matchCase.body))
  }

  def typeCheck(tpe: STpeExpr)(implicit matchCase: SCase, state: MatchingState) = {
    checkCond(isInstance(state.selector, tpe))
  }

  def checkCond(cond: SExpr)(implicit matchCase: SCase, state: MatchingState) = {
    if (state.forAll)
      state.copy(condExpr = cond, thenExpr = state.get)
    else {
      state.copy(condExpr = cond, thenExpr = matchCase.body, elseExpr = state.get)
    }
  }

  def typeCheckAndBind(name: String, tpe: STpeExpr)
                      (implicit matchCase: SCase, state: MatchingState) = {
    MatchingState(
      selector = state.selector,
      condExpr = isInstance(state.selector, tpe),
      thenExpr = injectAlias(name, state.selector, matchCase.body),
      elseExpr = state.get
    )
  }

  def alternatives(alts: List[SPattern])(implicit matchCase: SCase, state: MatchingState) = {
    val fakeCases = alts.map(alt => matchCase.copy(pat = alt))
    val initState = MatchingState(
      selector = state.selector,
      condExpr = SEmpty(),
      thenExpr = state.get,
      elseExpr = SEmpty()
    )
    val finalState = fakeCases.foldRight(initState){
      (mcase: SCase, st: MatchingState) => updateMatchingState(mcase, st)
    }

    finalState
  }

  def extractor(fun: SExpr, pats: List[SPattern])
               (implicit matchCase: SCase, state: MatchingState) = {
    val optName = "opt42"
    val fakeCases = pats.map(pat => SCase(pat, SEmpty(), SEmpty()))
    val initState = MatchingState(
      selector = getSel(optName, pats.length),
      condExpr = SEmpty(),
      thenExpr = matchCase.body,
      elseExpr = state.get,
      forAll = true
    )
    val (_, accumState) = fakeCases.foldRight((pats.length, initState)){
      (mcase: SCase, st: (Int, MatchingState)) => {
        val (off, s) = st
        val currSelector = getSel(optName, off)
        val updatedState = updateMatchingState(mcase, s.copy(selector = currSelector))
        (off - 1, updatedState)
      }
    }

    state.copy(
      condExpr = isInstance(state.selector, convStableIdToType(fun)), // isInstanceOf[CaseClass]
      thenExpr = {
        val opt = makeAlias(optName, unapplyFrom(fun, state.selector))
        val checkOpt = checkNotEmpty(optName, accumState.get, accumState.elseExpr)
        SBlock(List(opt), checkOpt) // val o = CaseClass.unapply(selector); if (!o.isEmpty) {...}
      }
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

  def isInstance(expr: SExpr, tpe: STpeExpr): SExpr = {
    STypeApply(SSelect(expr, "isInstanceOf"), List(tpe))
  }

  def unapplyFrom(stableId: SExpr, param: SExpr) = {
    SApply(SSelect(stableId, "unapply"), List(), List(List(param)))
  }

  def checkNotEmpty(stableId: String, thenexpr: SExpr, elseexpr: SExpr) = {
    val isEmpty = SApply(SSelect(SIdent(stableId), "isEmpty"), List(), Nil)

    SIf(isEmpty, elseexpr, thenexpr)
  }

  def convStableIdToType(stableId: SExpr): STpeExpr = stableId match {
    case SIdent(name) => STraitCall(name, List())
  }

  def getSel(optName: String, offset: Int): SExpr = {
    val optVal = SApply(SSelect(SIdent(optName), "get"), List(), Nil) // opt42.get

    SApply(SSelect(optVal, "_" + offset.toString), List(), Nil) // opt42.get._offset
  }
}
