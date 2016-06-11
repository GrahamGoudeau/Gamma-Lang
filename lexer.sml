signature LEXER = sig
  datatype tokenResult =
      EOF
    | IDENTIFIER of string
    | INTEGER of int
    | OPERATOR of string
    | OPEN_PAREN
    | CLOSE_PAREN

  type line = int
  type token = tokenResult * line
  type lexer

  datatype lexerResult = OK of token | ERROR of string

  val newLexer : char list -> lexer
  val getToken : lexer -> (lexerResult * lexer)

end

structure Lexer :> LEXER = struct
  datatype tokenResult =
      EOF
    | IDENTIFIER of string
    | INTEGER of int
    | OPERATOR of string
    | OPEN_PAREN
    | CLOSE_PAREN

  type line = int
  type token = tokenResult * line
  type lexer = (char list * line)

  datatype lexerResult = OK of token | ERROR of string

  fun errorReport(message, lineNo, lexer) =
    (ERROR (message ^ " on line " ^ (Int.toString lineNo)), lexer)

  fun newLexer cs = (cs, 1)

  fun member(elem, list) = List.exists (fn x => x = elem) list

  val opChars = [#"+", #"-", #"/", #"*"]

  fun getToken ([], r) = (OK (EOF, r), ([], r))
    | getToken (#" "::cs, r) = getToken(cs, r)
    | getToken (#"\t"::cs, r) = getToken(cs, r)
    | getToken (#"("::cs, r) = (OK (OPEN_PAREN, r), (cs, r))
    | getToken (#")"::cs, r) = (OK (CLOSE_PAREN, r), (cs, r))
    | getToken (c::cs, r) =
      if Char.isSpace c then getToken(cs, r + 1)
      (*else errorReport(("Unrecognized input character " ^ (Char.toString c)),
      * r, (c::cs, r))*)
      else if member(c, opChars) then (OK (EOF, r), ([], r))
      else (OK (EOF, r), ([], r))
end
