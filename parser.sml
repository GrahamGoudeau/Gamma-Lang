structure Parser :> PARSER = struct
  type identifier = string
  type line = int

  datatype value = INTEGER of int
                 | BOOL of bool
                 | UNDEFINED

  datatype exp = LIT of value * line
               | VAR of identifier * line

  datatype topLevel = DEFN of identifier * bool * identifier list * exp list * line
                    | EXP of exp

  datatype parserResult = OK of topLevel list | ERROR of string

  (* an invocation of parse should begin at the start of a function
   * definition or the start of an expression
   *)
end
