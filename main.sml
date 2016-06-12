datatype exitCode = FAIL | SUCCESS

fun exit FAIL = OS.Process.exit OS.Process.failure
  | exit SUCCESS = OS.Process.exit OS.Process.success

fun println s = print (s ^ "\n")

fun exitError message =
  (println message; exit FAIL)

fun printUsage() =
  (println ("Usage: gamma {input file name}"); exit FAIL)

fun parseArgs [fileName] = fileName
  | parseArgs _ = printUsage()

fun getInputChars fileName =
let
  val inputStream = TextIO.openIn fileName
    handle Io => exitError ("Problem opening file '" ^ fileName ^ "'")

  fun getChars stream =
  let
    fun error() = exitError ("Problem reading from file '" ^ fileName ^ "'")
    fun accumulate NONE = []
      | accumulate c = (Option.valOf c) :: (accumulate (TextIO.input1 stream))
          handle Option => error()
  in
    accumulate (TextIO.input1 stream)
      handle Io => error()
  end

in
  getChars inputStream
end

fun main() =
let
  val fileName = parseArgs (CommandLine.arguments())
  val inputChars = getInputChars fileName
in exit SUCCESS
end

val _ = main()
