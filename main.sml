fun printUsage() =
  (Utils.printLn ("Usage: gamma {input file name}"); exit FAIL)

fun parseArgs [fileName] = fileName
  | parseArgs _ = printUsage()

fun getInputChars fileName =
let
  val inputStream = TextIO.openIn fileName
    handle Io => Utils.error(INTERNAL, "Problem opening file '" ^ fileName ^ "'")

  fun getChars stream =
  let
    fun accumulate NONE = []
      | accumulate c = (Option.valOf c) :: (accumulate (TextIO.input1 stream))
  in
    accumulate (TextIO.input1 stream)
      handle _ => Utils.error(INTERNAL, "Problem reading from file '" ^ fileName ^ "'")
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
       (":=", (Parser.RIGHT, Parser.BINARY, 1))
       ]
  val builtInOpStrs = List.map (fn (str, _) => str) operators
  val opMap = Parser.addNewOperators(Parser.newOperatorMap, operators)
  val inputChars = getInputChars fileName
  val tokens = buildTokens inputChars builtInOpStrs fileName
  val parseForest = Parser.parse(tokens, opMap)
in exit SUCCESS
end

val _ = main()
