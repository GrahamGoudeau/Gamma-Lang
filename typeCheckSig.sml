signature TYPE_CHECK = sig
  type typeCheckerContext

  val newTypeCheckerContext : (string * string) -> typeCheckerContext

  val typeCheck : Parser.topLevel list * typeCheckerContext -> unit
end
