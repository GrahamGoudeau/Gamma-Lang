signature PARSER = sig
  type identifier = string

  datatype value = INTEGER of int
                 | BOOL of bool
                 | STRING_LITERAL of string
                 | IDENTIFIER of identifier
                 | OPERATOR of identifier
                 | UNDEFINED

  type definition
  datatype exp = DEFINE of definition * int(*DEFINE of identifier * identifier list * exp list * int*)
               | LIT of value * int
               | VAR of identifier * int
               | CALL of exp * exp list * int
               | MODULE_CALL of string * string * exp list * int
               | MODULE_VAR of string * string * int
               | LAMBDA of identifier list * exp
               | IF of exp * exp * exp * int

  datatype topLevel = TOP_DEFINE of definition * int
                    | CONSTANT of exp * int

  val getExpListFromDefinition : definition -> exp list

  val getNameFromDefinition : definition -> string

  val getParamsFromDefinition : definition -> string list

  datatype associativity = LEFT | RIGHT
  datatype arity = UNARY | BINARY
  type operatorMap

  type module

  (* returns module name and AST forest *)
  val parse : (Lexer.token list * operatorMap * string) -> (string * (topLevel list))

  val newOperatorMap : operatorMap

  val addNewOperator : (string * (associativity * arity * int)) * operatorMap -> operatorMap

  val addNewOperators : operatorMap * ((string * (associativity * arity * int)) list) -> operatorMap

  val reportParenErrors : Lexer.token list * string -> unit

  val isExpressionAssignOp : exp -> bool

  val isWellFormedAssignment : exp -> bool

  val getVarAssigned : (exp * string) -> string

  val getExpLine : exp -> int

  (* TODO: remove *)
  exception ParserError of string
end
