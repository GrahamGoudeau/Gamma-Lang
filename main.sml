datatype exitCode = FAIL | SUCCESS

fun exit FAIL = OS.Process.exit OS.Process.failure
  | exit SUCCESS = OS.Process.exit OS.Process.success

fun printUsage() =
  (print ("Usage string..."); exit FAIL)

fun parseArgs [fileName] = fileName
  | parseArgs _ = printUsage()

fun main() =
let
  val fileName = parseArgs (CommandLine.arguments())
in exit SUCCESS
end

val _ = main()
