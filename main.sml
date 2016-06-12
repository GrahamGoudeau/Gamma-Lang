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
in
  getChars inputStream
end

exception LexerError of string
exception ParserError of string

fun handleErrors f = f()
  handle (LexerError s) => exitError ("Lexer error:\n\t'" ^ s ^ "'")
    | (ParserError s) => exitError ("Parser error:\n\t'" ^ s ^ "'")

fun buildTokens inputChars =
let
  val lexer = Lexer.newLexer inputChars
  fun buildTokens (Lexer.OK (Lexer.EOF, _), _) = []
    | buildTokens (Lexer.OK (result, line), newLexer) =
        (result, line) :: (buildTokens (Lexer.getToken newLexer))
    | buildTokens ((Lexer.ERROR reason), _) = raise (LexerError reason)
in buildTokens (Lexer.getToken lexer)
end

fun main() =
let
  val fileName = parseArgs (CommandLine.arguments())
  val inputChars = getInputChars fileName
  val tokens = buildTokens inputChars
in exit SUCCESS
end

val _ = handleErrors main
