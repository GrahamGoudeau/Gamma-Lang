structure Utils = struct
  exception UnexpectedError

  val github = "github.com/GrahamGoudeau/Gamma-Lang"

  fun printLn s = print (s ^ "\n")

  fun unexpectedError message =
    (printLn ("An unexpected error occurred or you found a bug; please report at " ^ github ^
        "\n\"" ^ message ^ "\""); raise UnexpectedError)

  fun intToString n =
    if n > 0 then Int.toString n
    else ("-" ^ (Int.toString (~1 * n)))

  datatype module = LEXER of string * int
                  | PARSER of string * int
                  | INTERNAL

  fun moduleToString INTERNAL = "Internal"
    | moduleToString (LEXER _) = "Lexer"
    | moduleToString (PARSER _) = "Parser"

  datatype exitCode = FAIL | SUCCESS

  fun exit FAIL = OS.Process.exit OS.Process.failure
    | exit SUCCESS = OS.Process.exit OS.Process.success

  fun error(module, message) =
  let
    fun printAndFail str =
      (printLn str; exit FAIL)

    fun errorWithFileLine(module, fileName, line) =
      ((moduleToString module) ^ " error reported:\n\t" ^ message ^ "\n" ^
        "\nOccurred at " ^ fileName ^ ":" ^ (intToString line))

  in (case module of
      INTERNAL => printAndFail ("Internal error reported:\n\t" ^ message)
    | PARSER(fileName, line) =>
        ((printAndFail o errorWithFileLine) (module, fileName, line))
    | LEXER(fileName, line) =>
        ((printAndFail o errorWithFileLine) (module, fileName, line)))
  end

end
