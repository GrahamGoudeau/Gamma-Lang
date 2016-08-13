structure HashTable :> HASH_TABLE = struct
  type ('k, 'v) hashTable = {
                              table: ('k * 'v) list array,
                              defaultTuple: 'v,
                              tableSize: int,
                              hashFunction: 'k -> int,
                              eqFunction: 'k * 'k -> bool,
                              numElements: int
                            }

  val DEFAULT_SIZE = 1000
  val MAX_LOAD_FACTOR = 0.8

  fun jenkinsHash str =
  let
    val chars = String.explode str
    val maxInt = case Int.maxInt of
                      SOME n => Word.fromInt n
                    | NONE => Utils.unexpectedError "Unable to get maximum integer size"
    val startHash = Word.fromInt 0
    val ten = Word.fromInt 10
    val six = Word.fromInt 6
    val three = Word.fromInt 3
    val eleven = Word.fromInt 11
    val fifteen = Word.fromInt 15
    fun hashProcedure([], hash) = hash
      | hashProcedure(c::cs, hash) =
          let
            val addChar = Word.+(hash, Word.fromInt(Char.ord c))
            val shlTenAdd = Word.+(addChar, Word.<<(addChar, ten))
            val shrSixXor = Word.xorb(shlTenAdd, Word.>>(shlTenAdd, six))
          in hashProcedure(cs, shrSixXor)
          end
    val afterLoop = hashProcedure(chars, startHash)
    val shlThreeAdd = Word.+(afterLoop, Word.<<(afterLoop, three))
    val shrElevenXor = Word.xorb(shlThreeAdd, Word.>>(shlThreeAdd, eleven))
    val shlFifteenAdd = Word.+(shrElevenXor, Word.<<(shrElevenXor, fifteen))
    val resultInRange = Word.andb(shlFifteenAdd, maxInt)
  in Word.toInt resultInRange
  end

  fun newHashTable hashFunction eqFunction defaultTuple =
    {
      table = Array.tabulate(DEFAULT_SIZE, (fn _ => [])),
      defaultTuple = defaultTuple,
      tableSize = DEFAULT_SIZE,
      hashFunction = hashFunction,
      eqFunction = eqFunction,
      numElements = 0
    }

  fun getIndex(tableSize, f, key) =
    (f key) mod tableSize

  fun getChain(table, index) =
    Array.sub(table, index)
      handle Subscript => Utils.unexpectedError "Array out of bounds in hash table"

  fun newStringHashTable defaultTuple =
    newHashTable
        jenkinsHash
        (fn (str1, str2) => String.compare(str1, str2) = EQUAL)
        defaultTuple

  fun put {table=table, defaultTuple=defaultTuple, tableSize=tableSize, hashFunction=hashFunction, eqFunction=eqFunction, numElements=numElements} key value =
  let
    val index = getIndex(tableSize, hashFunction, key)
    val chain = getChain(table, index)
    val () = Array.update(table, index, (key, value)::chain)
    val realNumElements = Real.fromInt(numElements)
    val realTableSize = Real.fromInt(tableSize)
    val loadFactor = realNumElements / realTableSize
    val (newTable, newTableSize) =
      if Real.>=(loadFactor, MAX_LOAD_FACTOR) then
        let
          val () = Utils.warn "Hash table expand not implemented; load factor degrading performance"
        in (table, tableSize)
        end
      else (table, tableSize)
  in {table=newTable,
      defaultTuple=defaultTuple,
      tableSize=newTableSize,
      hashFunction=hashFunction,
      eqFunction=eqFunction,
      numElements=numElements + 1
     }
  end

  fun get {
            table=table,
            defaultTuple=_,
            tableSize=tableSize,
            hashFunction=hashFunction,
            eqFunction=eqFunction,
            numElements=numElements
          }
          key =
  let
    val index = getIndex(tableSize, hashFunction, key)
    val chain = getChain(table, index)
    fun find [] = NONE
      | find ((k, v)::kvs) =
          if eqFunction(k, key) then SOME v
          else find kvs
  in find chain
  end
end
