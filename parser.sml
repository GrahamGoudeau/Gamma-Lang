structure Parser :> PARSER = struct
  type identifier = string
  exception ParserError of string

  datatype value = INTEGER of int
                 | BOOL of bool
                 | STRING_LITERAL of string
                 | IDENTIFIER of identifier
                 | OPERATOR of identifier
                 | UNDEFINED

  datatype exp = DEFINE of definition * int
               | LIT of value * int
               | VAR of identifier * int
               | CALL of exp * exp list * int
               | MODULE_CALL of string * string * exp list * int
               | MODULE_VAR of string * string * int
               | LAMBDA of identifier list * exp
     withtype definition = identifier * identifier list * exp list * bool

  datatype topLevel = TOP_DEFINE of definition * int
                    | CONSTANT of exp * int

  type module = string * exp list

  (* PARSER UTILITY FUNCTIONS *)
  fun fst (x, _) = x
  fun snd (_, y) = y
  val getLabel = fst
  val getLine = snd

  fun peekTokenLabel [] = NONE
    | peekTokenLabel (t::ts) = SOME (getLabel t)

  fun peekTokenLine [] = NONE
    | peekTokenLine (t::ts) = SOME (getLine t)

  fun raiseError(message, line, fileName) =
    Utils.error(Utils.PARSER(fileName, line), message)

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

  fun getOpAssoc oper opMap line fileName =
  let
    val resultOpt = List.find (fn (testOper, _) => testOper = oper) opMap
  in
    case resultOpt of
         NONE => raiseError("Unknown operator " ^ oper, line, fileName)
       | SOME (_, (assoc, _, _)) => assoc
  end

  fun getOpPrecedence oper opMap line fileName =
  let
    val resultOpt = List.find (fn (testOper, _) => testOper = oper) opMap
  in
    case resultOpt of
         NONE => raiseError("Unknown operator " ^ oper, line, fileName)
       | (SOME (_, (_, _, prec))) => prec
  end

  fun getOpArity oper opMap line fileName =
  let
    val resultOpt = List.find (fn (testOper, _) => testOper = oper) opMap
  in
    case resultOpt of
         NONE => raiseError("Unknown operator " ^ oper, line, fileName)
       | SOME (_, (_, arity, _)) => arity
  end

  fun valueToString (INTEGER i) = ("<int " ^ (Utils.intToString i) ^ ">")
    | valueToString (BOOL b) = ("<bool " ^ (if b then "t" else "f") ^ ">")
    | valueToString (STRING_LITERAL s) = ("<string \"" ^ s ^ "\">")
    | valueToString (IDENTIFIER i) = ("<ident " ^ i ^ ">")
    | valueToString (OPERATOR i) = ("<operator " ^ i ^ ">")
    | valueToString UNDEFINED = ("<undefined>")

  val rec expToString = fn
      (LIT (v, _)) => ("{literal " ^ (valueToString v) ^ "}")
    | (DEFINE ((i, iList, eList, _), _)) =>
        ("{define function " ^ i ^ "}")
    | (VAR (i, _)) => ("{var " ^ i ^ "}")
    | (CALL (i, eList, _)) => ("{call " ^ (expToString i) ^ " params: (" ^
        (String.concat (List.map (fn e => expToString e) eList)) ^ ")}")
    | (MODULE_CALL (moduleName, funcName, eList, _)) =>
        ("{call " ^ moduleName ^ "." ^ funcName ^ " params: (" ^
        (String.concat (List.map (fn e => expToString e) eList)) ^ ")}")
    | (MODULE_VAR (moduleName, i, _)) =>
        ("{var " ^ moduleName ^ "." ^ i ^ "}")
    | (LAMBDA (is, exp)) => ("{lambda " ^
        ((String.concat o (List.map (fn i => (i ^ " ")))) is) ^ "-> " ^ (expToString exp) ^ "}")

  fun expect(expectToken, [], fileName) = raiseError("Expected token " ^ (Lexer.tokenToString expectToken) ^ ", got EOF", ~1, fileName)
    | expect(expectToken, t::ts, fileName) =
        if expectToken = (getLabel t) then ts
        else raiseError("Expected token " ^ (Lexer.tokenToString expectToken) ^
          ", got " ^ ((Lexer.tokenToString o getLabel) t), getLine t, fileName)

  (* MAIN PARSING *)

  fun gatherArgs([], _, _, _, _, fileName) =
        raiseError("Expected closing parenthesis in function call, got EOF", ~1, fileName)
    | gatherArgs(t::ts, args, expectExpression, firstParam, opMap, fileName) = (case getLabel t of
        Lexer.COMMA =>
          if expectExpression then
            raiseError("Expected expression in function argument list", getLine t, fileName)
          else
            gatherArgs(ts, args, true, false, opMap, fileName)
      | Lexer.CLOSE_PAREN =>
          if expectExpression andalso (not firstParam) then
            raiseError("Expected expression in function argument list", getLine t, fileName)
          else
            (args, ts)
      | _ =>
          let
            val (exprOpt, exprState) = parseExpression((t::ts), opMap, MIN_OP_PRECEDENCE, fileName, true)
            val exp = lazyGetOpt(exprOpt, fn () => raiseError("Error while parsing function argument", getLine t, fileName))
          in
            gatherArgs(exprState, args @ [exp], false, false, opMap, fileName)
          end)

  and getParams [] _ fileName =
        raiseError("Expected parameter list, got EOF", ~1, fileName)
    | getParams ((Lexer.CLOSE_PAREN, line)::ts') acc fileName = (acc, ts')
    | getParams ((Lexer.IDENTIFIER i, line)::(Lexer.CLOSE_PAREN, line2)::ts') acc fileName =
        (acc @ [i], ts')
    | getParams ((Lexer.IDENTIFIER i, line)::(Lexer.COMMA, line2)::(Lexer.IDENTIFIER i2, line3)::ts')
                acc
                fileName =
        getParams ((Lexer.IDENTIFIER i2, line3)::ts') (acc @ [i]) fileName
    | getParams (t::ts) acc fileName =
        raiseError("Syntax error parsing parameter list- expected identifier and comma or identifier and close parenthesis", getLine t, fileName)

  and gatherLambdaParams([], _, _, fileName) =
        raiseError("Expected closing '|' in lambda definition, got EOF", ~1, fileName)
    | gatherLambdaParams(t::ts, args, expectIdent, fileName) = (case getLabel t of
        Lexer.COMMA =>
          if expectIdent then
            raiseError("Expected identifier in lambda argument list", getLine t, fileName)
          else
            gatherLambdaParams(ts, args, true, fileName)
      | Lexer.LAMBDA_BAR =>
          if expectIdent then
            raiseError("Expected identifier in lambda argument list", getLine t, fileName)
          else
            (args, (t::ts))
      | Lexer.IDENTIFIER i =>
          if (expectIdent) then
            gatherLambdaParams(ts, args @ [i], false, fileName)
          else
            raiseError("Expected comma in lambda argument list", getLine t, fileName)
      | label =>
          raiseError("Expected identifier or comma in lambda argument list, " ^
              "got " ^ (Lexer.tokenToString label), getLine t, fileName))

  (* source: http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing *)
  and parseExpression([], _, _, fileName, fail) =
        if fail then
          raiseError("Expected expression, got EOF", ~1, fileName)
        else (NONE, [])
    | parseExpression((Lexer.FUNCTION_START, line)::ts, opMap, _, fileName, fail) =
        let
          val (func, funcState) = parseFunction(ts, opMap, true, fileName)
        in
          (SOME (DEFINE (func, line)), funcState)
        end
    | parseExpression((Lexer.LAMBDA_BAR, line)::ts, opMap, _, fileName, fail) =
        let
          val (idents, identsState) = gatherLambdaParams(ts, [], true, fileName)
          val afterBar = expect(Lexer.LAMBDA_BAR, identsState, fileName)
          val afterArrow = expect(Lexer.OPERATOR "->", afterBar, fileName)
          val (expOpt, expState) = parseExpression(afterArrow, opMap, MIN_OP_PRECEDENCE, fileName, fail)
          val exp = lazyGetOpt(expOpt, fn () => raiseError("Unexpected error getting expression optional value", line, fileName))
        in
          (SOME (LAMBDA (idents, exp)), expState)
        end
    | parseExpression(tokens as (t::ts), opMap, minPrecedence, fileName, fail) =
      let
        val (lhsOpt, lhsState) = parseAtom(tokens, opMap, fileName, true)
        val lhs = lazyGetOpt(lhsOpt, fn () => raiseError("Expected term of expression", getLine t, fileName))

        (* expects the first token to be a binary operator -
         * gathers "branches" of the expression tree *)
        val rec gatherBranches = fn
            (currentLeftHandSide, []) => (SOME currentLeftHandSide, [])
          | (currentLeftHandSide, (tokens as ((Lexer.OPERATOR oper, line)::ts'))) =>
              if (getOpArity oper opMap line fileName) <> BINARY orelse
                     (getOpPrecedence oper opMap line fileName) < minPrecedence then
                (SOME currentLeftHandSide, tokens)
              else
                let
                  val currentPrec = getOpPrecedence oper opMap line fileName
                  val currentAssoc = getOpAssoc oper opMap line fileName
                  val newMinPrec = if currentAssoc = LEFT then currentPrec + 1 else currentPrec

                  val (rhsOpt, rhsState) = parseExpression(ts', opMap, newMinPrec, fileName, fail)
                in
                  case rhsOpt of
                       NONE => (SOME currentLeftHandSide, tokens)
                     | SOME rhs => gatherBranches(CALL (LIT (OPERATOR oper, line), [currentLeftHandSide, rhs], line), rhsState)
                end

          | (currentLeftHandSide, t'::ts') =>
              (SOME currentLeftHandSide, t'::ts')
      in
        gatherBranches (lhs, lhsState)
      end

      (* TODO: change the fail parameter to be unit -> 'a *)
  and parseAtom([], opMap, fileName, fail) =
        if fail then
          raiseError("Expected expression but got EOF", ~1, fileName)
        else (NONE, [])
    | parseAtom(t::ts, opMap, fileName, fail) = (case getLabel t of
        Lexer.INTEGER i =>
          (SOME(LIT(INTEGER i, getLine t)), ts)
      | Lexer.STRING_LITERAL s =>
          (SOME(LIT(STRING_LITERAL s, getLine t)), ts)
      | Lexer.OPEN_PAREN =>
          let
            val (exp, expState) = parseExpression(ts, opMap, MIN_OP_PRECEDENCE, fileName, false)
          in
            case exp of
                 NONE => raiseError("Expected parenthesized expression", getLine t, fileName)
               | SOME e =>
                   (case expState of
                        [] => raiseError("Unmatched \"(\"", getLine t, fileName)
                      | ((Lexer.CLOSE_PAREN, l)::ts) =>
                          (case ts of
                               [] => (SOME e, ts)
                             | ((Lexer.OPEN_PAREN, l')::ts') =>
                                 let
                                   val (exps, expsState) = gatherArgs(ts', [], true, true, opMap, fileName)
                                 in
                                   (SOME(CALL(e, exps, getLine t)), expsState)
                                 end
                             | _ => (SOME e, ts))
                      | ((t, l)::ts) =>
                          raiseError("Expected \")\", got " ^
                            (Lexer.tokenToString t) ^ " in expression " ^ (expToString e), l, fileName))
          end
      | Lexer.IDENTIFIER i =>
          (case ts of
                ((Lexer.OPEN_PAREN, _)::ts') =>
                  let
                    val (args, argsState) = gatherArgs(ts', [], true, true, opMap, fileName)
                  in
                    (SOME(CALL(LIT (IDENTIFIER i, getLine t), args, getLine t)), argsState)
                  end
              | ((Lexer.DOT, _)::(Lexer.IDENTIFIER i2, _)::(Lexer.OPEN_PAREN, line)::ts') =>
                  let
                    val (args, argsState) = gatherArgs(ts', [], true, true, opMap, fileName)
                  in
                    (SOME(MODULE_CALL(i, i2, args, line)), argsState)
                  end
              | ((Lexer.DOT, _)::(Lexer.IDENTIFIER i2, line)::ts') =>
                  (SOME(MODULE_VAR(i, i2, line)), ts')
              | _ => (SOME(LIT(IDENTIFIER i, getLine t)), ts))

      | Lexer.OPERATOR oper =>
          (case getOpArity oper opMap (getLine t) fileName of
               BINARY =>
                 raiseError("Expected unary operator, got " ^ oper, getLine t, fileName)
             | UNARY =>
                 let
                   val (nextAtomOpt, nextAtomState) = parseAtom(ts, opMap, fileName, fail)
                   val nextAtom = lazyGetOpt(nextAtomOpt, fn () =>
                   raiseError("Expected atom while parsing unary operator", getLine t, fileName))
                 in (SOME (CALL(LIT (OPERATOR oper, getLine t), [nextAtom], getLine t)), nextAtomState)
                 end)
      | _ =>
          if fail then
            raiseError("Expected expression identifier, literal, or operator; got " ^ (Lexer.tokenToString (getLabel t)), getLine t, fileName)
          else (NONE, t::ts))

  (* expects the tokens to start immediately after the FUNCTION_START token *)
  and parseFunction([], _, _, fileName) =
        raiseError("Expected function definition, got EOF", ~1, fileName)
    | parseFunction((Lexer.IDENTIFIER funcName, line)::ts, opMap, isPure, fileName) =
      let
        fun getExps ([], _) =
              raiseError("Expected expression or end of function block, got EOF", line, fileName)
          | getExps (tokens as ((Lexer.BLOCK_END, _)::ts), acc) =
              (acc, tokens)
          | getExps (t::ts, acc) =
              let
                val (exprOpt, expState) = parseExpression(t::ts, opMap, MIN_OP_PRECEDENCE, fileName, true)
                val exp = lazyGetOpt(exprOpt, fn () => raiseError("Unexpected error while parsing expression in function body", getLine t, fileName))
              in
                getExps(expState, acc @ [exp])
              end

        val openingParenState = expect(Lexer.OPEN_PAREN, ts, fileName)
        val (paramList, paramListState) = getParams openingParenState [] fileName
        val equalsSignState = expect(Lexer.BLOCK_BEGIN, paramListState, fileName)
        val (exps, expsState) = getExps(equalsSignState, [])
        val afterFuncState = expect(Lexer.BLOCK_END, expsState, fileName)
      in ((funcName, paramList, exps, isPure), afterFuncState)
      end
    | parseFunction(t::ts, _, _, fileName) =
        raiseError("Expected function name in declaration, got " ^ ((Lexer.tokenToString o getLabel) t), getLine t, fileName)

  val parseExpression : (Lexer.token list * operatorMap * int * string * bool) -> (exp option * Lexer.token list) = parseExpression

  val parseAtom : (Lexer.token list * operatorMap * string * bool) -> (exp option * Lexer.token list) = parseAtom

  val parseFunction : (Lexer.token list * operatorMap * bool * string) -> (definition * Lexer.token list) = parseFunction

  fun parse([], _, fileName) =
        raiseError("Empty file", ~1, fileName)
    | parse((Lexer.MODULE_BEGIN, line1)::
            (Lexer.IDENTIFIER moduleName, line2)::ts,
            opMap,
            fileName) =
      let
        fun innerParse [] =
              raiseError("Module ended without \"" ^ (Lexer.tokenToString Lexer.BLOCK_END) ^ "\" keyword", ~1, fileName)
          | innerParse [(Lexer.BLOCK_END, endLine)] = []
          | innerParse (t::ts) = (case getLabel t of
              Lexer.IMPURE => (case ts of
                 [] => raiseError("Got \"impure\" keyword without function definition", getLine t, fileName)
               | ((Lexer.FUNCTION_START, line)::ts') =>
                   let
                     val (func, funcState) = parseFunction(ts', opMap, false, fileName)
                   in
                     (TOP_DEFINE(func, getLine t)) :: innerParse(funcState)
                   end
               | (t'::ts') => raiseError("Got \"impure\" keyword without function definition", getLine t, fileName))
            | Lexer.FUNCTION_START =>
                   let
                     val (func, funcState) = parseFunction(ts, opMap, true, fileName)
                   in
                     (TOP_DEFINE(func, getLine t)) :: innerParse(funcState)
                   end
            | Lexer.CONSTANT =>
                let
                  val (exprOpt, exprState) = parseExpression(ts, opMap, MIN_OP_PRECEDENCE, fileName, true)
                  val exp = lazyGetOpt(exprOpt, fn () => raiseError("Unexpected error getting constant expression", getLine t, fileName))
                in
                  (CONSTANT(exp, getLine t)) :: innerParse(exprState)
                end
            | label =>
                raiseError("Expected function definition or constant definition, got " ^ (Lexer.tokenToString label), getLine t, fileName))
      in (moduleName, innerParse ts)
      end

    | parse(t::ts, _, fileName) =
        raiseError("Syntax error at start of module (expected " ^
          (Lexer.tokenToString Lexer.MODULE_BEGIN) ^ " {module name})",
          getLine t, fileName)
end
