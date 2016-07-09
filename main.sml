datatype exitCode = FAIL | SUCCESS

fun exit FAIL = OS.Process.exit OS.Process.failure
  | exit SUCCESS = OS.Process.exit OS.Process.success

fun println s = print (s ^ "\n")

fun exitError message =
  (println message; exit FAIL)

fun printUsage() =
  (println ("Usage: gamma {input file name}"); exit FAIL)

fun parseArgs [fileName] = fileName
  | parseArgs _ = printUsage()

fun getInputChars fileName =
let
  val inputStream = TextIO.openIn fileName
    handle Io => exitError ("Problem opening file '" ^ fileName ^ "'")

  fun getChars stream =
  let
    fun accumulate NONE = []
      | accumulate c = (Option.valOf c) :: (accumulate (TextIO.input1 stream))
  in
    accumulate (TextIO.input1 stream)
      handle _ => exitError ("Problem reading from file '" ^ fileName ^ "'")
  end
  val result = getChars inputStream
  val _ = TextIO.closeIn inputStream
in
  result
end

exception UnexpectedError of string

fun handleErrors f = f()
  handle (Lexer.LexerError s) => exitError ("Lexer error:\n\t'" ^ s ^ "'")
    | (Parser.ParserError s) => exitError ("Parser error:\n\t'" ^ s ^ "'")
    | (UnexpectedError s) => exitError ("Found a bug...\n\t'" ^ s ^ "'")

fun buildTokens inputChars builtInOperators =
let
  val lexer = Lexer.newLexer(inputChars, builtInOperators)
  fun buildTokens (Lexer.OK (Lexer.EOF, _), _) = []
    | buildTokens (Lexer.OK token, newLexer) =
        token :: (buildTokens (Lexer.getToken newLexer))
    | buildTokens ((Lexer.ERROR reason), _) = raise (Lexer.LexerError reason)
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
  val tokens = buildTokens inputChars builtInOpStrs
  val parseForest = Parser.parse(tokens, opMap)
in exit SUCCESS
end

val _ = handleErrors main
