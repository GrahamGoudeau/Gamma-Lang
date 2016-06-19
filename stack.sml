structure Stack :> STACK = struct
  type 'a stack = 'a list
  val isEmpty = List.null
  val newStack = []

  fun push(elem, stack) = elem :: stack

  fun pop([], errorFunc) = (errorFunc(), [])
    | pop(e::es, _) = (e, es)

  fun reverseStack [] = []
    | reverseStack (e::es) = (reverseStack es) @ [e]
end
