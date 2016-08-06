structure CodeGen :> CODE_GEN = struct
  type codeGenContext = {
                          moduleName: string,
                          fileName: string
                        }

  fun newCodeGenContext(module, file) =
    {
      moduleName = module,
      fileName = file
    }

  val TEMP_DIR = "/tmp/"
  val INCLUDE_IO = "#include <stdio.h>"
  val INCLUDE_STD = "#include <stdlib.h>"

  val std_includes = [INCLUDE_IO,
                      INCLUDE_STD
                     ]

  fun outputLine(outStream, line) = TextIO.output(outStream, line ^ "\n")
  fun lineBreak outStream = TextIO.output(outStream, "\n")

  fun includeLibs outStream =
    List.map (fn includeLib => outputLine(outStream, includeLib)) std_includes

  fun getOutFileName context = TEMP_DIR ^ (#moduleName (context: codeGenContext)) ^ ".c"

  fun createOutFile context = (TextIO.openOut o getOutFileName) context

  fun codeGen(ts, context) =
  let
    val outStream = createOutFile context
    val _ = includeLibs outStream
  in ()
  end
end
