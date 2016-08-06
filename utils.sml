structure Utils = struct
  exception UnexpectedError

  val github = "github.com/GrahamGoudeau/Gamma-Lang"

  val ASSIGN_OP_STR = ":="

  val RESERVED_PREFIX = "___"

  fun printLn s = print (s ^ "\n")

  fun unexpectedError message =
    (printLn ("An unexpected error occurred or you found a bug; please report at " ^ github ^
        "\n\"" ^ message ^ "\""); raise UnexpectedError)

  fun intToString n =
    if n > 0 then Int.toString n
    else ("-" ^ (Int.toString (~1 * n)))

  datatype module = LEXER of string * int
                  | PARSER of string * int
                  | TYPE_CHECK of string * int
                  | INTERNAL

  fun moduleToString INTERNAL = "Internal"
    | moduleToString (LEXER _) = "Lexer"
    | moduleToString (PARSER _) = "Parser"
    | moduleToString (TYPE_CHECK _) = "Type checking"

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
    val doExit = (printAndFail o errorWithFileLine)

  in (case module of
      INTERNAL => printAndFail ("Internal error reported:\n\t" ^ message)
    | PARSER(fileName, line) =>
        (doExit (module, fileName, line))
    | TYPE_CHECK(fileName, line) =>
        (doExit (module, fileName, line))
    | LEXER(fileName, line) =>
        (doExit (module, fileName, line)))
  end

  fun member list elem = List.exists (fn x => x = elem) list
end
