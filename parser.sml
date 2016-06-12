structure Parser :> PARSER = struct
  type identifier = string

  datatype value = INTEGER of int
                 | BOOL of bool
                 | UNDEFINED

  datatype exp = LIT of value
               | VAR of identifier

  datatype topLevel = DEFN of identifier * bool * identifier list * exp list
                    | EXP of exp

  local
    open Lexer
  in
  fun buildAst EOF = 1
  end
end
