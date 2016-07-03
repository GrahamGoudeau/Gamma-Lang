signature PARSER = sig
  type identifier = string

  datatype value = INTEGER of int
                 | BOOL of bool
                 | IDENTIFIER of identifier
                 | UNDEFINED

                 (*
  datatype exp = LIT of value * line
               | VAR of identifier * line
               *)
  datatype exp = ASSIGN of identifier * exp * int
               | LIT of value * int
               | VAR of identifier * int
               | CALL of identifier * exp list

  (*type expContext*)

  (*datatype topLevel = DEFN of identifier * bool * identifier list * exp list * line
                    | EXP of exp
                    *)

  (*val parse : Lexer.token list -> topLevel list*)
  val expression : Lexer.token list -> exp option

  datatype associativity = LEFT | RIGHT
  datatype precedence = ONE | TWO
  type operatorMap

  val parse : (Lexer.token list * operatorMap) -> (exp option * (Lexer.tokenLabel * int) list) list

  val newOperatorMap : operatorMap

  val addNewOperator : (string * (associativity * precedence)) * operatorMap -> operatorMap

  val addNewOperators : operatorMap * ((string * (associativity * precedence)) list) -> operatorMap

  exception ParserError of string
end
