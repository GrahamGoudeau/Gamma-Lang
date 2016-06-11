signature LEXER = sig
  datatype tokenResult =
      EOF
    | IDENTIFIER of string
    | INTEGER of int
    | OPERATOR of string
    | OPEN_PAREN
    | CLOSE_PAREN

  type line = int
  type token = tokenResult * line
  type lexer

  datatype lexerResult = OK of token | ERROR of string

  val newLexer : char list -> lexer
  val getToken : lexer -> (lexerResult * lexer)

end

structure Lexer :> LEXER = struct
  datatype tokenResult =
      EOF
    | IDENTIFIER of string
    | INTEGER of int
    | OPERATOR of string
    | OPEN_PAREN
    | CLOSE_PAREN

  type line = int
  type token = tokenResult * line
  type lexer = (char list * line)

  datatype lexerResult = OK of token | ERROR of string

  fun errorReport(message, lineNo, lexer) =
    (ERROR (message ^ " on line " ^ (Int.toString lineNo)), lexer)

  fun newLexer cs = (cs, 1)

  fun member(elem, list) = List.exists (fn x => x = elem) list

  val opChars = [#"+", #"-", #"/", #"*"]

  fun peekChar [] = NONE
    | peekChar (c::cs) = SOME c

  val charListToString = String.concat o (List.map (fn c => Char.toString c))

  fun accumulateChars predicate chars =
  let
    fun getChars [] acc = ([], acc)
      | getChars (c::cs) acc =
          if predicate c then getChars cs (acc @ [c])
          else (c::cs, acc)
  in getChars chars []
  end

  fun getToken ([], r) = (OK (EOF, r), ([], r))
    | getToken (#" "::cs, r) = getToken(cs, r)
    | getToken (#"\t"::cs, r) = getToken(cs, r)
    | getToken (#"("::cs, r) = (OK (OPEN_PAREN, r), (cs, r))
    | getToken (#")"::cs, r) = (OK (CLOSE_PAREN, r), (cs, r))
    | getToken (c::cs, r) =

      (* NEW LINE LEXING *)
      if Char.isSpace c then getToken(cs, r + 1)

      (* OPERATOR LEXING *)
      else if member(c, opChars) then
        let
          val (remainingChars, result) =
            accumulateChars (fn c => member(c, opChars)) (c::cs)
          val resultString = charListToString result
        in
          (OK (OPERATOR resultString, r), (remainingChars, r))
        end

      else if Char.isDigit c then
        let
          val (remainingChars, result) =
            accumulateChars (Char.isDigit) (c::cs)
          val resultNumOption = (Int.fromString o charListToString) result
        in
          (case resultNumOption of
                NONE => errorReport("Unexpected error", r, ((c::cs), r))
              | SOME(i) =>
                  (OK ((INTEGER i), r), (cs, r)))
        end

      else (OK (EOF, r), ([], r))
      (*else errorReport(("Unrecognized input character " ^ (Char.toString c)),
      * r, (c::cs, r))*)
end

val s = String.explode
val l = Lexer.newLexer (s "   \n 1+1+++2 9 - 8")
val (e, l1) = Lexer.getToken l
val (t, l2) = Lexer.getToken l1
val (f, l3) = Lexer.getToken l2
val (g, l4) = Lexer.getToken l3
