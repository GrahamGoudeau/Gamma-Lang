signature CODE_GEN = sig
  type codeGenContext
  val newCodeGenContext : string * string -> codeGenContext

  val codeGen : Parser.topLevel list * codeGenContext -> unit
end
