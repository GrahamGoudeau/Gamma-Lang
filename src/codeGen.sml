structure CodeGen :> CODE_GEN = struct
  type codeGenContext = {
                          moduleName: string,
                          fileName: string,
                          indentLevel: int
                        }

  fun newCodeGenContext(module, file) =
    {
      moduleName = module,
      fileName = file,
      indentLevel = 0
    }

  (* private function to replace the indentation level in a context *)
  fun buildNewContext ({moduleName=module, fileName=file, indentLevel=indent}: codeGenContext, newIndentLevel) =
    {moduleName=module, fileName=file, indentLevel=newIndentLevel}

  val TEMP_DIR = "/tmp/"
  val INCLUDE_IO = "#include <stdio.h>"
  val INCLUDE_STD = "#include <stdlib.h>"

  val std_includes = [INCLUDE_IO,
                      INCLUDE_STD
                     ]

  val output = TextIO.output
  fun outputLine(outStream, line) = output(outStream, line ^ "\n")
  fun lineBreak outStream = output(outStream, "\n")

  fun includeLibs outStream =
    List.map (fn includeLib => outputLine(outStream, includeLib)) std_includes

  fun getOutFileName context = TEMP_DIR ^ (#moduleName (context: codeGenContext)) ^ ".c"

  fun createOutFile context = (TextIO.openOut o getOutFileName) context

  fun getModuleName (context: codeGenContext) =
    Utils.RESERVED_PREFIX ^ (#moduleName context)

  fun getSafeOperatorName(oper, context: codeGenContext) =
  let
    fun convertChars ([], acc) = String.concat acc
      | convertChars (c::cs, acc) =
        let
          val (_, opStr) =
            Utils.safeFind
              (fn chData => (Utils.fst chData) = c)
              Utils.opCharData
              (fn _ => Utils.unexpectedError ("Problem converting operator '" ^ oper ^ "' to C-safe function name"))
        in
          convertChars (cs, acc @ [opStr, "_"])
        end
  in ((getModuleName context) ^ "_" ^ (convertChars(String.explode oper, [])))
  end

  fun writeLiteral(v, outStream, context: codeGenContext) =
    let
      val curriedOutput = fn str => output(outStream, str)
      val writeInt = curriedOutput o Utils.intToString
      val writeBool = curriedOutput o Bool.toString
      fun writeOperator oper = (curriedOutput o getSafeOperatorName)(oper, context)

      open Parser
    in case v of
          INTEGER i => writeInt i
        | BOOL b => writeBool b
        | STRING_LITERAL str => curriedOutput ("\"" ^ str ^ "\"")
        | IDENTIFIER i => curriedOutput i
        | OPERATOR oper => writeOperator oper
        | UNDEFINED => Utils.unexpectedError ("Attempted to write an undefined value to file")
    end

  fun cleanup (outStream, context: codeGenContext) =
  let
    val () = TextIO.closeOut outStream
    val () = (OS.FileSys.remove o getOutFileName) context
  in ()
  end

  fun writeExpression (e, outStream, context: codeGenContext) =
    let
      open Parser
    in case e of
        _ => ()
    end

  fun codeGen([], _) = ()
    | codeGen(t::ts, context) =
  let
    val outStream = createOutFile context
    val _ = includeLibs outStream
    val () = cleanup(outStream, context)
    open Parser
  in case t of
      CONSTANT (e, _) => writeExpression(e, outStream, context)
    | _ => ()
  end
end
