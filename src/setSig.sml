signature SET = sig
  type 'a set

  val newSet : ('a -> int) -> ('a * 'a -> bool) -> 'a set
  val newStringSet : string set
  val put : 'a set -> 'a -> 'a set
  val contains : 'a set -> 'a -> bool
end
