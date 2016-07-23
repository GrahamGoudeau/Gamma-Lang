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
    | LAMBDA_BAR
    | INTEGER of int
    | OPERATOR of string
    | OPEN_PAREN
    | CLOSE_PAREN
    | ANNOTATION
    | COMMA
    | MODULE_BEGIN
    | DOT
    | CONSTANT
    | INFIX
    | INFIXR
    | STRING_LITERAL of string

  type line = int
  type token = tokenLabel * line
  type lexer

  (* input characters * built in operators * file name *)
  val newLexer : (char list * string list * string) -> lexer
  val getToken : lexer -> (token * lexer)

  val tokenToString : tokenLabel -> string
end
