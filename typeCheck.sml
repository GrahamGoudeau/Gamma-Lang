structure TypeCheck :> TYPE_CHECK = struct
  open Parser
  type typeCheckerContext = {moduleName: string,
                             fileName: string
                            }

  fun newTypeCheckerContext(moduleName, fileName) = {moduleName=moduleName,
                                                     fileName=fileName
                                                    }

  fun raiseTypeError(message, line, context: typeCheckerContext) =
    Utils.error(Utils.TYPE_CHECK(#fileName context, line), message)

  fun checkAssignments([], context) = ()
    | checkAssignments(t::ts, context) =
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
              checkAssignAndContinue(e, fn () => checkAssignments(ts, context))

          | CONSTANT(e, line) => raiseTypeError("Expected constant definition to be variable assignment", line, context)

          | TOP_DEFINE(definition, _) =>
              checkFunctionBody(getExpListFromDefinition definition, fn () => checkAssignments(ts, context)))
        end

  fun typeCheck(topLevelList, context) =
  let
    val _ = checkAssignments(topLevelList, context)
  in () end
end
