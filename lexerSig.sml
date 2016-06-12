signature LEXER = sig
  datatype tokenResult =
      EOF
    | IDENTIFIER of string
    | KEYWORD of string
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

  val printTokenList : token list -> unit
end
