structure TypeCheck :> TYPE_CHECK = struct
  open Parser
  type typeCheckerContext = {
                              moduleName: string,
                              fileName: string
                            }

  fun newTypeCheckerContext(moduleName, fileName) = {
                                                      moduleName=moduleName,
                                                      fileName=fileName
                                                    }

  fun raiseTypeError(message, line, context: typeCheckerContext) =
    Utils.error(Utils.TYPE_CHECK(#fileName context, line), message)

  fun checkAssignmentSyntax([], context) = ()
    | checkAssignmentSyntax(t::ts, context) =
      let
        fun checkAssignAndContinue(e as (CALL(func, _, line)), continuation) =
          if Parser.isExpressionAssignOp func andalso (not (Parser.isWellFormedAssignment e)) then
            raiseTypeError("Bad LHS of assignment; expected variable assigned to expression", line, context)
          else
            continuation()
          | checkAssignAndContinue(_, continuation) = continuation()

        fun checkFunctionBody ([], continuation) = continuation()
          | checkFunctionBody ((e::es), continuation) =
              checkAssignAndContinue(e, fn () => checkFunctionBody(es, continuation))

      in (case t of

            (* check if a single variable is being assigned the result of some other expression *)
            CONSTANT(e as (CALL(func, args, _)), line) =>
              checkAssignAndContinue(e, fn () => checkAssignmentSyntax(ts, context))

          | CONSTANT(e, line) => raiseTypeError("Expected constant definition to be variable assignment", line, context)

          | TOP_DEFINE(definition, _) =>
              checkFunctionBody(getExpListFromDefinition definition, fn () => checkAssignmentSyntax(ts, context)))
        end

  (* expects assignments to be syntactically correct by this point *)
  fun checkRepeatAssignment([], context) = ()
    | checkRepeatAssignment(topLevels, context) =
      let
        fun updateSet(expression, varSet) =
            if Parser.isWellFormedAssignment expression then
              let
                val ident = Parser.getVarAssigned(expression, #fileName context)
                val identLine = Parser.getExpLine expression
              in
                if Set.contains varSet ident then
                  raiseTypeError("Redefinition of variable '" ^ ident ^ "' not allowed", identLine, context)
                else Set.put varSet ident
              end
            else
              varSet

        fun checkTopLevel([], _) = ()
          | checkTopLevel(t::ts, varSet) = (case t of
              CONSTANT(e, line) =>
                checkTopLevel(ts, updateSet(e, varSet))
            | TOP_DEFINE(definition, line) =>
                let
                  val expList = Parser.getExpListFromDefinition definition
                  val setWithParams =
                    List.foldl (fn (param, set) => Set.put set param) varSet (Parser.getParamsFromDefinition definition)
                  val _ =
                    List.foldl updateSet setWithParams expList

                (* be sure that function bodies do not leak variable names into broader scope *)
                in checkTopLevel(ts, Set.put varSet (Parser.getNameFromDefinition definition))
              end)
      in checkTopLevel(topLevels, Set.newStringSet) end

  fun typeCheck(topLevelList, context) =
  let
    val () = checkAssignmentSyntax(topLevelList, context)
    val () = checkRepeatAssignment(topLevelList, context)
  in () end
end
