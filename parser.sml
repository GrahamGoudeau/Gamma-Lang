structure Parser :> PARSER = struct
  type identifier = string
  exception ParserError of string

  datatype value = INTEGER of int
                 | BOOL of bool
                 | IDENTIFIER of identifier
                 | UNDEFINED

  datatype exp = DEFINE of definition * int
               | LIT of value * int
               | VAR of identifier * int
               | CALL of identifier * exp list * int
     withtype definition = identifier * identifier list * exp list * bool

  datatype topLevel = TOP_DEFINE of definition * int
                    | CONSTANT of exp * int

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
    | (DEFINE ((i, iList, eList, _), _)) =>
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
    | parseExpression((Lexer.FUNCTION_START, line)::ts, opMap, _, fail) =
        let
          val (func, funcState) = parseFunction(ts, opMap, false)
        in
          (SOME (DEFINE (func, line)), funcState)
        end
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
            fun gatherArgs([], _, _, _) =
                  raiseError("Expected closing parenthesis in function call", getLine t)
              (* TODO: this allows `f(a, b, ) *)
              | gatherArgs(t::ts, args, expectExpression, firstParam) = (case getLabel t of
                  Lexer.COMMA =>
                    if expectExpression then
                      raiseError("Expected expression in function argument list", getLine t)
                    else
                      gatherArgs(ts, args, true, false)
                | Lexer.CLOSE_PAREN =>
                    if expectExpression andalso (not firstParam) then
                      raiseError("Expected expression in function argument list", getLine t)
                    else
                      (args, ts)
                | _ =>
                    let
                      val (exprOpt, exprState) = parseExpression((t::ts), opMap, MIN_OP_PRECEDENCE, true)
                      val exp = lazyGetOpt(exprOpt, fn () => raiseError("Error while parsing function argument", getLine t))
                    in
                      gatherArgs(exprState, args @ [exp], false, false)
                    end)
          in
            (case ts of
                  ((Lexer.OPEN_PAREN, _)::ts') =>
                    let
                      val (args, argsState) = gatherArgs(ts', [], true, true)
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
            raiseError("Expected expression identifier, literal, or operator; got " ^ (Lexer.tokenToString (getLabel t)), getLine t)
          else (NONE, t::ts))

  (* expects the tokens to start immediately after the FUNCTION_START token *)
  and parseFunction([], _, _) =
        raiseError("Expected function definition, got EOF", ~1)
    | parseFunction((Lexer.IDENTIFIER funcName, line)::ts, opMap, isPure) =
      let
        fun expect(expectToken, []) = raiseError("Expected token " ^ (Lexer.tokenToString expectToken) ^ ", got EOF", ~1)
          | expect(expectToken, t::ts) =
              if expectToken = (getLabel t) then ts
              else raiseError("Expected token " ^ (Lexer.tokenToString expectToken) ^ ", got " ^ ((Lexer.tokenToString o getLabel) t), getLine t)

        fun getParams [] _ =
              raiseError("Expected parameter list, got EOF", ~1)
          | getParams ((Lexer.CLOSE_PAREN, line)::ts') acc = (acc, ts')
          | getParams ((Lexer.IDENTIFIER i, line)::(Lexer.CLOSE_PAREN, line2)::ts') acc = (acc @ [i], ts')
          | getParams ((Lexer.IDENTIFIER i, line)::(Lexer.COMMA, line2)::(Lexer.IDENTIFIER i2, line3)::ts') acc =
              getParams ((Lexer.IDENTIFIER i2, line3)::ts') (acc @ [i])
          | getParams (t::ts) acc =
              raiseError("Syntax error parsing parameter list- expected identifier and comma or identifier and close parenthesis", getLine t)

        fun getExps ([], _) =
              raiseError("Expected expression or end of function block, got EOF", line)
          | getExps (tokens as ((Lexer.BLOCK_END, _)::ts), acc) =
              (acc, tokens)
          | getExps (t::ts, acc) =
              let
                val (exprOpt, expState) = parseExpression(t::ts, opMap, MIN_OP_PRECEDENCE, true)
                val exp = lazyGetOpt(exprOpt, fn () => raiseError("Unexpected error while parsing expression in function body", getLine t))
              in
                getExps(expState, acc @ [exp])
              end

        val openingParenState = expect(Lexer.OPEN_PAREN, ts)
        val (paramList, paramListState) = getParams openingParenState []
        val equalsSignState = expect(Lexer.BLOCK_BEGIN, paramListState)
        val (exps, expsState) = getExps(equalsSignState, [])
        val afterFuncState = expect(Lexer.BLOCK_END, expsState)
      in ((funcName, paramList, exps, isPure), afterFuncState)
      end
    | parseFunction(t::ts, _, _) =
        raiseError("Expected function name in declaration, got " ^ ((Lexer.tokenToString o getLabel) t), getLine t)

  val parseExpression : (Lexer.token list * operatorMap * int * bool) -> (exp option * Lexer.token list) = parseExpression

  val parseAtom : (Lexer.token list * operatorMap * bool) -> (exp option * Lexer.token list) = parseAtom

  val parseFunction : (Lexer.token list * operatorMap * bool) -> (definition * Lexer.token list) = parseFunction

  fun parse([], _) = []
    | parse(t::ts, opMap) = (case getLabel t of
          Lexer.IMPURE => (case ts of
             [] => raiseError("Got \"impure\" keyword without function definition", getLine t)
           | ((Lexer.FUNCTION_START, line)::ts') =>
               let
                 val (func, funcState) = parseFunction(ts', opMap, false)
               in
                 (TOP_DEFINE(func, getLine t)) :: parse(funcState, opMap)
               end
           | (t'::ts') => raiseError("Got \"impure\" keyword without function definition", getLine t))
        | Lexer.FUNCTION_START =>
               let
                 val (func, funcState) = parseFunction(ts, opMap, true)
               in
                 (TOP_DEFINE(func, getLine t)) :: parse(funcState, opMap)
               end
        | Lexer.CONSTANT =>
            let
              val (exprOpt, exprState) = parseExpression(ts, opMap, MIN_OP_PRECEDENCE, true)
              val exp = lazyGetOpt(exprOpt, fn () => raiseError("Unexpected error getting constant expression", getLine t))
            in
              (CONSTANT(exp, getLine t)) :: parse(exprState, opMap)
            end
        | label =>
            raiseError("Expected function definition or constant definition, got " ^ (Lexer.tokenToString label), getLine t))
end
