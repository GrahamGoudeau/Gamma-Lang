signature STACK = sig
  type 'a stack
  val newStack : 'a stack
  val push : ('a * 'a stack) -> 'a stack
  val pop : ('a stack * (unit -> 'a)) -> ('a * 'a stack)
  val isEmpty : 'a stack -> bool
  val reverseStack : 'a stack -> 'a stack
end
