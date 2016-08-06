fun printUsage() =
  (Utils.printLn ("Usage: gamma {input file name}"); Utils.exit Utils.FAIL)

fun parseArgs [fileName] = fileName
  | parseArgs _ = printUsage()

fun getInputChars fileName =
let
  val inputStream = TextIO.openIn fileName
    handle Io => Utils.error(Utils.INTERNAL, "Problem opening file '" ^ fileName ^ "'")

  fun getChars stream =
  let
    fun accumulate NONE = []
      | accumulate c = (Option.valOf c) :: (accumulate (TextIO.input1 stream))
  in
    accumulate (TextIO.input1 stream)
      handle _ => Utils.error(Utils.INTERNAL, "Problem reading from file '" ^ fileName ^ "'")
  end
  val result = getChars inputStream
  val _ = TextIO.closeIn inputStream
in
  result
end

fun buildTokens inputChars builtInOperators fileName =
let
  val lexer = Lexer.newLexer(inputChars, builtInOperators, fileName)
  fun buildTokens ((Lexer.EOF, _), _) = []
    | buildTokens (token, newLexer) =
        token :: (buildTokens (Lexer.getToken newLexer))
in buildTokens (Lexer.getToken lexer)
end

fun main() =
let
  val fileName = parseArgs (CommandLine.arguments())
  val operators =
      [("+", (Parser.LEFT, Parser.BINARY, 1)),
       ("-", (Parser.LEFT, Parser.BINARY, 1)),
       ("*", (Parser.LEFT, Parser.BINARY, 2)),
       ("/", (Parser.LEFT, Parser.BINARY, 2)),
       ("~", (Parser.LEFT, Parser.UNARY, 2)),
       ("^", (Parser.RIGHT, Parser.BINARY, 3)),
       ("and", (Parser.LEFT, Parser.BINARY, 4)),
       ("or", (Parser.LEFT, Parser.BINARY, 4)),
       ("not", (Parser.LEFT, Parser.UNARY, 4)),
       (Utils.ASSIGN_OP_STR, (Parser.RIGHT, Parser.BINARY, 1))
       ]
  val builtInOpStrs = List.map (fn (str, _) => str) operators
  val opMap = Parser.addNewOperators(Parser.newOperatorMap, operators)
  val inputChars = getInputChars fileName
  val tokens = buildTokens inputChars builtInOpStrs fileName
  val _ = Parser.reportParenErrors(tokens, fileName)
  val (moduleName, parseForest) = Parser.parse(tokens, opMap, fileName)
  val typeCheckContext = TypeCheck.newTypeCheckerContext(moduleName, fileName)
  val _ = TypeCheck.typeCheck(parseForest, typeCheckContext)
  val codeGenContext = CodeGen.newCodeGenContext(moduleName, fileName)
  val _ = CodeGen.codeGen(parseForest, codeGenContext)
in Utils.exit Utils.SUCCESS
end

val _ = main()
