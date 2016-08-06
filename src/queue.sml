structure Queue :> QUEUE = struct
  type 'a queue = ('a Stack.stack * 'a Stack.stack)

  fun addLast(elem, (addStack, getStack)) = (Stack.push(elem, addStack), getStack)

  val newQueue = (Stack.newStack, Stack.newStack)

  fun getFirst((addStack, getStack), errorFunc) =
    if Stack.isEmpty addStack andalso Stack.isEmpty getStack then
      (errorFunc(), newQueue)
    else if Stack.isEmpty getStack then
      getFirst((Stack.newStack, Stack.reverseStack addStack), errorFunc)
    else
      let
        val (top, rest) = Stack.pop(getStack, errorFunc)
      in (top, (addStack, rest))
      end

  fun isEmpty(s1, s2) = Stack.isEmpty s1 andalso Stack.isEmpty s2
end
