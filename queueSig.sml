signature QUEUE = sig
  type 'a queue
  val newQueue : 'a queue
  val addLast : ('a * 'a queue) -> 'a queue
  val getFirst : ('a queue * (unit -> 'a)) -> ('a * 'a queue)
  val isEmpty : 'a queue -> bool
end
