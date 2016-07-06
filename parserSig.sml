signature PARSER = sig
  type identifier = string

  datatype value = INTEGER of int
                 | BOOL of bool
                 | IDENTIFIER of identifier
                 | UNDEFINED

  datatype exp = DEFINE of identifier * identifier list * exp list * int
               | LIT of value * int
               | VAR of identifier * int
               | CALL of identifier * exp list * int

  val expression : Lexer.token list -> exp option

  datatype associativity = LEFT | RIGHT
  datatype arity = UNARY | BINARY
  type operatorMap

  val parse : (Lexer.token list * operatorMap) -> exp list

  val newOperatorMap : operatorMap

  val addNewOperator : (string * (associativity * arity * int)) * operatorMap -> operatorMap

  val addNewOperators : operatorMap * ((string * (associativity * arity * int)) list) -> operatorMap

  exception ParserError of string
end
