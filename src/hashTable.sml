structure HashTable :> HASH_TABLE = struct
  type ('k, 'v) hashTable = {
                              table: ('k * 'v) list array,
                              defaultValue: 'v,
                              tableSize: int,
                              hashFunction: 'k -> int,
                              eqFunction: 'k * 'k -> bool,
                              numElements: int ref
                            }

  val DEFAULT_SIZE = 1000
  val MAX_LOAD_FACTOR = 0.8

  fun jenkinsHash str =
  let
    val chars = String.explode str
    val maxInt = case Int.maxInt of
                      SOME n => Word.fromInt n
                    | NONE => Utils.unexpectedError("Unable to get maximum integer size", __FILE__, __LINE__)
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

  fun buildHashTable hashFunction eqFunction defaultValue size =
    {
      table = Array.tabulate(size, (fn _ => [])),
      defaultValue = defaultValue,
      tableSize = size,
      hashFunction = hashFunction,
      eqFunction = eqFunction,
      numElements = ref 0
    }

  fun newHashTable hashFunction eqFunction defaultValue =
    buildHashTable hashFunction eqFunction defaultValue DEFAULT_SIZE

  fun getIndex(tableSize, f, key) =
    (f key) mod tableSize

  fun getChain(table, index) =
    Array.sub(table, index)
      handle Subscript => Utils.unexpectedError("Array out of bounds in hash table", __FILE__, __LINE__)

  fun newStringHashTable defaultValue =
    newHashTable
        jenkinsHash
        (fn (str1, str2) => String.compare(str1, str2) = EQUAL)
        defaultValue

  fun expandHashTable(
        (originalTable as {table=table, defaultValue=defaultValue,
          tableSize=tableSize, hashFunction=hashFunction, eqFunction=eqFunction,
          numElements=numElements}),
        newSize) =
    let
      val newTable = buildHashTable hashFunction eqFunction defaultValue newSize
      fun rehashList [] = ()
        | rehashList ((k, v)::kvs) =
            (put newTable k v;
             rehashList kvs)

      val () = Array.app rehashList table
    in
      newTable
    end

  and put (originalTable as {table=table, defaultValue=defaultValue, tableSize=tableSize, hashFunction=hashFunction, eqFunction=eqFunction, numElements=numElements}) key value =
  let
    val index = getIndex(tableSize, hashFunction, key)
    val chain = getChain(table, index)
    val () = Array.update(table, index, (key, value)::chain)
    val realNumElements = Real.fromInt(!numElements)
    val realTableSize = Real.fromInt(tableSize)
    val loadFactor = realNumElements / realTableSize
    val () = numElements := (!numElements) + 1
    val newTable =
      if Real.>=(loadFactor, MAX_LOAD_FACTOR) then
        expandHashTable(originalTable, tableSize * 2)
      else originalTable
  in newTable
  end

  fun get {
            table=table,
            defaultValue=_,
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
