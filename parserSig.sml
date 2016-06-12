signature PARSER = sig
  type identifier = string
  type line = int

  datatype value = INTEGER of int
                 | BOOL of bool
                 | UNDEFINED

  datatype exp = LIT of value * line
               | VAR of identifier * line

  datatype topLevel = DEFN of identifier * bool * identifier list * exp list * line
                    | EXP of exp
end
