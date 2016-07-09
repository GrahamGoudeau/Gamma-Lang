signature LEXER = sig
  datatype tokenLabel =
      EOF
    | IDENTIFIER of string
    | KEYWORD of string
    | DECLARE_VAR
    | FUNCTION_START
    | BLOCK_END
    | BLOCK_BEGIN
    | IMPURE
    | LAMBDA
    | INTEGER of int
    | OPERATOR of string
    | OPEN_PAREN
    | CLOSE_PAREN
    | ANNOTATION
    | COMMA
    | MODULE_BEGIN
    | CONSTANT
    | STRING_LITERAL of string

  type line = int
  type token = tokenLabel * line
  type lexer

  datatype lexerResult = OK of token | ERROR of string

  val newLexer : char list -> lexer
  val getToken : lexer -> (lexerResult * lexer)

  val tokenToString : tokenLabel -> string
  exception LexerError of string
end
