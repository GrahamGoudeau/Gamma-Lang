signature PARSER = sig
  type identifier = string

  datatype value = INTEGER of int
                 | BOOL of bool
                 | STRING_LITERAL of string
                 | IDENTIFIER of identifier
                 | UNDEFINED

  type definition
  datatype exp = DEFINE of definition * int(*DEFINE of identifier * identifier list * exp list * int*)
               | LIT of value * int
               | VAR of identifier * int
               | CALL of identifier * exp list * int
               | LAMBDA of identifier list * exp

  datatype topLevel = TOP_DEFINE of definition * int
                    | CONSTANT of exp * int

  datatype associativity = LEFT | RIGHT
  datatype arity = UNARY | BINARY
  type operatorMap

  type module

  val parse : (Lexer.token list * operatorMap * string) -> topLevel list

  val newOperatorMap : operatorMap

  val addNewOperator : (string * (associativity * arity * int)) * operatorMap -> operatorMap

  val addNewOperators : operatorMap * ((string * (associativity * arity * int)) list) -> operatorMap

  exception ParserError of string
end
