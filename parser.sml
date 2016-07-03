structure Parser :> PARSER = struct
  type identifier = string
  exception ParserError of string

  datatype value = INTEGER of int
                 | BOOL of bool
                 | IDENTIFIER of identifier
                 | UNDEFINED

  datatype exp = ASSIGN of identifier * exp * int
               | LIT of value * int
               | VAR of identifier * int
               | CALL of identifier * exp list

  (*datatype topLevel = DEFN of identifier * bool * identifier list * exp list * line
                    | EXP of exp
                    *)
  fun expression t = NONE

  (* PARSER UTILITY FUNCTIONS *)
  fun fst (x, _) = x
  fun snd (_, y) = y
  val getLabel = fst
  val getLine = snd
  fun intToString n =
    if n >= 0 then Int.toString n
    else ("-" ^ (Int.toString (~1 * n)))

  fun peekTokenLabel [] = NONE
    | peekTokenLabel (t::ts) = SOME (getLabel t)

  fun raiseError(message, line) =
    raise ParserError(message ^ "; reported on line " ^ (intToString line))
  fun lazyGetOpt(NONE, f) = f()
    | lazyGetOpt(SOME t, _) = t

  (* OPERATOR PRECEDENCE/ASSOCIATIVITY UTILS *)
  datatype associativity = LEFT | RIGHT
  datatype precedence = ONE | TWO
  type operatorMap = (string * (associativity * precedence)) list
  val newOperatorMap : operatorMap = []
  val addNewOperator = op ::
  (*
  fun addNewOperators(newEntries, existing) =
    List.foldl (op ::) existing newEntries
    *)
  (*val addNewOperators = (List.foldl (op ::))*)
  val addNewOperators = op @

  fun getOpAssoc oper opMap line =
  let
    val resultOpt = List.find (fn (testOper, (_, _)) => testOper = oper) opMap
  in
    case resultOpt of
         NONE => raiseError("Unknown operator " ^ oper, line)
       | SOME (_, (assoc, _)) => assoc
  end

  fun getOpPrecedence oper opMap line =
  let
    val resultOpt = List.find (fn (testOper, (_, _)) => testOper = oper) opMap
  in
    case resultOpt of
         NONE => raiseError("Unknown operator " ^ oper, line)
       | SOME (_, (_, prec)) => prec
  end

  fun valueToString (INTEGER i) = ("<value int " ^ (intToString i) ^ ">")
    | valueToString (BOOL b) = ("<value bool " ^ (if b then "t" else "f") ^
    ">")
    | valueToString (IDENTIFIER i) = ("<value ident " ^ i ^ ">")
    | valueToString UNDEFINED = ("<value undefined>")

  val rec expToString = fn
        (ASSIGN (i, e, line)) => ("{assign " ^ i ^ " := " ^ (expToString e) ^ "}")
    | (LIT (v, _)) => ("{literal " ^ (valueToString v) ^ "}")
    | (VAR (i, _)) => ("{var " ^ i ^ "}")
    | (CALL (i, eList)) => ("{function call " ^ i ^ "}")


  (* MAIN PARSING *)
  fun parseFactor([], fail) =
        if fail then
          raiseError("Expected term but got EOF", ~1)
        else (NONE, [])
    | parseFactor(t::ts, fail) = let val _ = print "factor\n" in case getLabel t of
        Lexer.INTEGER i =>
          (SOME(LIT(INTEGER i, getLine t)), ts)
      | Lexer.IDENTIFIER i =>
          (SOME(LIT(IDENTIFIER i, getLine t)), ts)
      | _ =>
          if fail then
            raiseError("Expected expression factor, got " ^ (Lexer.tokenToString (getLabel t)), getLine t)
          else (NONE, t::ts)
                                 end

  fun parseTerm([], _, fail) =
        if fail then
          raiseError("Expected term but got EOF", ~1)
        else (NONE, [])
    | parseTerm(tokens as (t::ts), opMap, fail) =
        let
          val _ = print "term entering\n"
          val (lhsOpt, lhsState) = parseFactor(tokens, fail)
          val lhs = lazyGetOpt(lhsOpt, fn () => raiseError("Problem getting lhs of term", getLine t))
        in
          (case peekTokenLabel lhsState of
               NONE => (SOME lhs, lhsState)
             | SOME(Lexer.OPERATOR oper) =>
                 (case getOpPrecedence oper opMap (getLine (hd lhsState)) of
                      ONE => (SOME lhs, lhsState)
                    | TWO =>
                        let
                          val afterOp = tl lhsState
                          val (rhsOpt, rhsState) = parseFactor(afterOp, fail)
                          val rhs = lazyGetOpt(rhsOpt, fn () => raiseError("Problem getting rhs of term", getLine (hd rhsState)))
                        in
                          (SOME (CALL(oper, [lhs, rhs])), rhsState)
                        end)
             | _ => (SOME lhs, lhsState))
        end

  fun parseExpression([], opMap, fail) =
        if fail then
          raiseError("Expected expression but got EOF", ~1)
        else (NONE, [])
    | parseExpression(tokens as (t::ts), opMap, fail) =
        let
          val _ = print "expr entering\n"
          val (lhsOpt, lhsState) = parseTerm(tokens, opMap, fail)
          val lhs = lazyGetOpt(lhsOpt, fn () => raiseError("Problem getting lhs of expr", getLine t))
          val _ = print ("return to expr from term: " ^ (expToString lhs) ^ "\n")
        in
          (case peekTokenLabel lhsState of
               NONE => (SOME lhs, lhsState)
             | SOME(Lexer.OPERATOR oper) =>
                 (case getOpPrecedence oper opMap (getLine (hd lhsState)) of
                      TWO => (SOME lhs, lhsState)
                    | ONE =>
                        let
                          val afterOp = tl lhsState
                          val (rhsOpt, rhsState) = parseTerm(afterOp, opMap, fail)
                          val rhs = lazyGetOpt(rhsOpt, fn () => raiseError("Problem getting rhs of term", getLine (hd rhsState)))
                        in
                          (SOME(CALL(oper, [lhs, rhs])), rhsState)
                        end)
             | _ => (SOME lhs, lhsState))
        end

  fun parse([], _) = []
    | parse((t::ts), opMap) =
        [parseExpression(t::ts, opMap, true)]
end
