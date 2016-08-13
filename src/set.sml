structure Set :> SET = struct
  type 'a set = ('a, bool) HashTable.hashTable
  val boolType = true

  fun newSet hashFunction eqFunction =
    HashTable.newHashTable hashFunction eqFunction boolType

  val newStringSet = HashTable.newStringHashTable boolType
  fun put set value = HashTable.put set value boolType
  fun contains set value = case HashTable.get set value of
                                SOME _ => true
                              | NONE => false
end
