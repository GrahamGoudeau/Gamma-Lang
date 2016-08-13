signature HASH_TABLE = sig
  type ('k, 'v) hashTable

  val jenkinsHash : string -> int

  val newHashTable : ('k -> int) -> ('k * 'k -> bool) -> 'v -> ('k, 'v) hashTable
  val newStringHashTable : 'v -> (string, 'v) hashTable
  val put : ('k, 'v) hashTable -> 'k -> 'v -> ('k, 'v) hashTable
  val get : ('k, 'v) hashTable -> 'k -> 'v option
end
