signature LEXER = sig
  type lexer
  datatype tokenLabel =
      EOF
    | IDENTIFIER
    | INTEGER
    | OPERATOR
    | OPEN_PAREN
    | CLOSE_PAREN

  type token = string * tokenLabel

  type rule = lexer -> (token * lexer)

  val newLexer : (char list * rule list) -> lexer
  val getCurrentChar : lexer -> char option
  val advanceChar : lexer -> lexer

end

structure Lexer :> LEXER = struct
  type text = char list
  type line = int
  type column = int

  type lexer = (text * line * column)

  datatype tokenLabel =
      EOF
    | IDENTIFIER
    | INTEGER
    | OPERATOR
    | OPEN_PAREN
    | CLOSE_PAREN

  type token = string * tokenLabel
  type rule = lexer -> (token * lexer)

  (* represent the lexer state *)

  fun newLexer(charList, ruleList) = (charList, 1, 1)
  fun getCurrentChar ([], r, c) = NONE
    | getCurrentChar (c::cs, 1, 1) = SOME c

  fun advanceChar ([], r, c) = ([], r, c)
    | advanceChar (#"\n"::cs, r, c) = (cs, r + 1, 1)
    | advanceChar (_::cs, r, c) = (cs, r, c + 1)
end
