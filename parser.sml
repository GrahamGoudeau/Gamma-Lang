structure Parser :> PARSER = struct
  type identifier = string
  exception ParserError of string

  datatype value = INTEGER of int
                 | BOOL of bool
                 | IDENTIFIER of identifier
                 | UNDEFINED

  datatype exp = DEFINE of identifier * identifier list * exp list * int
               | LIT of value * int
               | VAR of identifier * int
               | CALL of identifier * exp list * int

  type module = string * exp list

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

  fun peekTokenLine [] = NONE
    | peekTokenLine (t::ts) = SOME (getLine t)

  fun raiseError(message, line) =
    raise ParserError(message ^ "; reported on line " ^ (intToString line))

  fun lazyGetOpt(NONE, f) = f()
    | lazyGetOpt(SOME t, _) = t

  (* OPERATOR PRECEDENCE/ASSOCIATIVITY UTILS *)
  datatype associativity = LEFT | RIGHT
  datatype arity = UNARY | BINARY

  type operatorMap = (string * (associativity * arity * int)) list

  val MIN_OP_PRECEDENCE = 1

  val newOperatorMap : operatorMap = []
  val addNewOperator = op ::
  val addNewOperators = op @

  fun getOpAssoc oper opMap line =
  let
    val resultOpt = List.find (fn (testOper, _) => testOper = oper) opMap
  in
    case resultOpt of
         NONE => raiseError("Unknown operator " ^ oper, line)
       | SOME (_, (assoc, _, _)) => assoc
  end

  fun getOpPrecedence oper opMap line =
  let
    val resultOpt = List.find (fn (testOper, _) => testOper = oper) opMap
  in
    case resultOpt of
         NONE => raiseError("Unknown operator " ^ oper, line)
       | (SOME (_, (_, _, prec))) => prec
  end

  fun getOpArity oper opMap line =
  let
    val resultOpt = List.find (fn (testOper, _) => testOper = oper) opMap
  in
    case resultOpt of
         NONE => raiseError("Unknown operator " ^ oper, line)
       | SOME (_, (_, arity, _)) => arity
  end

  fun valueToString (INTEGER i) = ("<value int " ^ (intToString i) ^ ">")
    | valueToString (BOOL b) = ("<value bool " ^ (if b then "t" else "f") ^
    ">")
    | valueToString (IDENTIFIER i) = ("<value ident " ^ i ^ ">")
    | valueToString UNDEFINED = ("<value undefined>")

  val rec expToString = fn
      (LIT (v, _)) => ("{literal " ^ (valueToString v) ^ "}")
    | (DEFINE (i, iList, eList, _)) =>
        ("{define function " ^ i ^ "}")
    | (VAR (i, _)) => ("{var " ^ i ^ "}")
    | (CALL (i, eList, _)) => ("{call " ^ i ^ " params: (" ^
        (String.concat (List.map (fn e => expToString e) eList)) ^ ")}")


  (* MAIN PARSING *)

  (* source: http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing *)
  fun parseExpression([], _, _, fail) =
        if fail then
          raiseError("Expected expression, got EOF", ~1)
        else (NONE, [])
    | parseExpression(tokens as (t::ts), opMap, minPrecedence, fail) =
      let
        val (lhsOpt, lhsState) = parseAtom(tokens, opMap, true)
        val lhs = lazyGetOpt(lhsOpt, fn () => raiseError("Expected term of expression", getLine t))

        (* expects the first token to be a binary operator -
         * gathers "branches" of the expression tree *)
        val rec gatherBranches = fn
            (currentLeftHandSide, []) => (SOME currentLeftHandSide, [])
          | (currentLeftHandSide, (tokens as ((Lexer.OPERATOR oper, line)::ts'))) =>
              if (getOpArity oper opMap line) <> BINARY orelse
                     (getOpPrecedence oper opMap line) < minPrecedence then
                (SOME currentLeftHandSide, tokens)
              else
                let
                  val currentPrec = getOpPrecedence oper opMap line
                  val currentAssoc = getOpAssoc oper opMap line
                  val newMinPrec = if currentAssoc = LEFT then currentPrec + 1 else currentPrec

                  val (rhsOpt, rhsState) = parseExpression(ts', opMap, newMinPrec, fail)
                in
                  case rhsOpt of
                       NONE => (SOME currentLeftHandSide, tokens)
                     | SOME rhs => gatherBranches(CALL (oper, [currentLeftHandSide, rhs], line), rhsState)
                end

          | (currentLeftHandSide, t'::ts') =>
              (SOME currentLeftHandSide, t'::ts')
      in
        gatherBranches (lhs, lhsState)
      end
  and
      (* TODO: change the fail parameter to be unit -> 'a *)
      parseAtom([], opMap, fail) =
        if fail then
          raiseError("Expected expression but got EOF", ~1)
        else (NONE, [])
    | parseAtom(t::ts, opMap, fail) = (case getLabel t of
        Lexer.INTEGER i =>
          (SOME(LIT(INTEGER i, getLine t)), ts)
      | Lexer.OPEN_PAREN =>
          let
            val (exp, expState) = parseExpression(ts, opMap, MIN_OP_PRECEDENCE, false)
          in
            case exp of
                 NONE => raiseError("Expected parenthesized expression", getLine t)
               | SOME e =>
                   (case expState of
                        [] => raiseError("Unmatched \"(\"", getLine t)
                      | ((Lexer.CLOSE_PAREN, l)::ts) => (SOME e, ts)
                      | ((t, l)::ts) =>
                          raiseError("Expected \")\", got " ^
                          (Lexer.tokenToString t) ^ " in expression " ^ (expToString e), l))
          end
      | Lexer.IDENTIFIER i =>
          let
            fun gatherArgs([], _) =
                  raiseError("Expected closing parenthesis in function call", getLine t)
              | gatherArgs(t::ts, args) = (case getLabel t of
                  Lexer.COMMA => gatherArgs(ts, args)
                | Lexer.CLOSE_PAREN => (args, ts)
                | _ =>
                    let
                      val (exprOpt, exprState) = parseExpression((t::ts), opMap, MIN_OP_PRECEDENCE, true)
                      val exp = lazyGetOpt(exprOpt, fn () => raiseError("Error while parsing function argument", getLine t))
                    in
                      gatherArgs(exprState, args @ [exp])
                    end)
          in
            (case ts of
                  ((Lexer.OPEN_PAREN, _)::ts') =>
                    let
                      val (args, argsState) = gatherArgs(ts', [])
                    in
                      (SOME(CALL(i, args, getLine t)), argsState)
                    end
                | _ => (SOME(LIT(IDENTIFIER i, getLine t)), ts))
          end

      | Lexer.OPERATOR oper =>
          (case getOpArity oper opMap (getLine t) of
               BINARY =>
                 raiseError("Expected unary operator, got " ^ oper, getLine t)
             | UNARY =>
                 let
                   val (nextAtomOpt, nextAtomState) = parseAtom(ts, opMap, fail)
                   val nextAtom = lazyGetOpt(nextAtomOpt, fn () => raiseError("Expected atom while parsing unary operator", getLine t))
                 in (SOME (CALL(oper, [nextAtom], getLine t)), nextAtomState)
                 end)
      | _ =>
          if fail then
            raiseError("Expected expression atom, got " ^ (Lexer.tokenToString (getLabel t)), getLine t)
          else (NONE, t::ts))

  fun parse([], _) = []
    | parse(tokens as (t::ts), opMap) =
        (*[parseExpression(t::ts, opMap, MIN_OP_PRECEDENCE, true)]*)
        let
          val (exprOpt, exprState) = parseExpression(tokens, opMap, MIN_OP_PRECEDENCE, true)
          val expr = lazyGetOpt(exprOpt, fn () => raiseError("Unexpected error while parsing expression", (getLine t)))
          val _ = print ((expToString expr) ^ "\n")
        in
          expr :: parse(exprState, opMap)
        end
end
