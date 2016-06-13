signature LEXER = sig
  datatype tokenLabel =
      EOF
    | IDENTIFIER of string
    | KEYWORD of string
    | FUNCTION_START
    | FUNCTION_END
    | IMPURE
    | LAMBDA
    | INTEGER of int
    | OPERATOR of string
    | OPEN_PAREN
    | CLOSE_PAREN
    | ANNOTATION

  type line = int
  type token = tokenLabel * line
  type lexer

  datatype lexerResult = OK of token | ERROR of string

  val newLexer : char list -> lexer
  val getToken : lexer -> (lexerResult * lexer)

  val printTokenList : token list -> unit
end
