structure Lexer :> LEXER = struct
  datatype tokenLabel =
      EOF
    | IDENTIFIER of string
    | KEYWORD of string
    | DECLARE_VAR
    | FUNCTION_START
    | BLOCK_END
    | BLOCK_BEGIN
    | IMPURE
    | LAMBDA_BAR
    | INTEGER of int
    | OPERATOR of string
    | OPEN_PAREN
    | CLOSE_PAREN
    | ANNOTATION
    | COMMA
    | MODULE_BEGIN
    | DOT
    | CONSTANT
    | INFIX
    | INFIXR
    | OPERATOR_DEFINE
    | STRING_LITERAL of string

  type line = int
  type token = tokenLabel * line
  type lexer = {builtInOperators: string list,
                  currentLine: line,
                  fileName: string,
                  lexerState: char list}

  fun getState (cs, _, _) = cs
  fun getLine (_, line, _) = line
  fun getBuiltInOperators (_, _, ops) = ops

  val keywordMappings = [("function", FUNCTION_START),
                         ("end", BLOCK_END),
                         ("impure", IMPURE),
                         ("var", DECLARE_VAR),
                         ("module", MODULE_BEGIN),
                         ("constant", CONSTANT),
                         ("infix", INFIX),
                         ("infixr", INFIXR),
                         ("operator", OPERATOR_DEFINE),
                         ("do", BLOCK_BEGIN)
                        ]

  val keywords = List.map (fn (keyword, _) => keyword) keywordMappings

  val commentChar = #"#"

  fun getKeywordLabel keyword =
  let
    val tupleOption = List.find (fn (str, _) => str = keyword) keywordMappings
    val tupleOption : (string * tokenLabel) option = tupleOption
  in (case tupleOption of
           NONE => NONE
         | (SOME (_, label)) => (SOME label))
  end

  fun getLabelString label =
  let
    val tupleOption = List.find (fn (_, possibleLabel) => possibleLabel = label) keywordMappings
  in (case tupleOption of
           NONE => NONE
         | (SOME (str, _)) => (SOME str))
  end

  val getKeywordLabel : string -> tokenLabel option = getKeywordLabel

  fun tokenToString EOF = "EOF"
    | tokenToString (IDENTIFIER s) = ("{ident " ^ s ^ "}")
    | tokenToString (KEYWORD s) = ("{keyword " ^ s ^ "}")
    | tokenToString (INTEGER s) = ("{int " ^ (Int.toString s) ^ "}")
    | tokenToString (OPERATOR s) = ("{operator " ^ s ^ "}")
    | tokenToString OPEN_PAREN = "("
    | tokenToString CLOSE_PAREN = ")"
    | tokenToString ANNOTATION = "@ (annotation)"
    | tokenToString COMMA = "{comma ','}"
    | tokenToString LAMBDA_BAR = "|"
    | tokenToString DOT = "."
    | tokenToString (STRING_LITERAL s) = ("\"" ^ s ^ "\"")
    | tokenToString label =
        (case getLabelString label of
              NONE => Utils.unexpectedError("Unexpected error getting label string")
            | SOME(l) => ("{keyword '" ^ l ^ "'}"))

  fun errorReport(message, lexer: lexer) =
    Utils.error(Utils.LEXER(#fileName lexer, #currentLine lexer), message)

  fun buildNewLexer(cs, line, builtInOperators, fileName) = {lexerState=cs, fileName=fileName, currentLine=line, builtInOperators=builtInOperators}

  fun newLexer(cs, builtInOperators, fileName) = buildNewLexer(cs, 1, builtInOperators, fileName)

  val opChars = [#"+", #"-", #"/", #"*", #"$", #"<", #">",
                 #"=", #":", #"~", #"^"]

  fun peekChar [] = NONE
    | peekChar (c::cs) = SOME c

  val charListToString = String.concat o (List.map Char.toString)

  fun isCharLeadIdentifier c = Char.isAlpha c orelse c = #"_"
  fun isCharInRestOfIdentifier c = isCharLeadIdentifier c orelse Char.isDigit c



  (* collect all chars until predicate is false,
   * and apply f to the resulting sub-list *)
  fun accumulateChars predicate chars f =
  let
    fun getChars [] acc = ([], f acc)
      | getChars (c::cs) acc =
          if predicate c then getChars cs (acc @ [c])
          else (c::cs, f acc)
  in getChars chars []
  end

  val accumulateChars : (char -> bool) -> char list -> (char list -> 'c) -> (char list * 'c) = accumulateChars

  fun isNextTokenIdentifier chars =
  let
    val (_, potentialIdent) = accumulateChars isCharInRestOfIdentifier chars charListToString
    val isNotEmpty = potentialIdent <> ""
  in isNotEmpty andalso (isCharLeadIdentifier (String.sub(potentialIdent, 0)))
        andalso (not (Utils.member keywords potentialIdent))
  end

  fun convertCommandChars _ [] = []
    | convertCommandChars _ [c] = [c]
    | convertCommandChars context (#"\\"::c'::cs) =
        let
          fun continue() = convertCommandChars context cs
        in (case c' of
            #"n" => #"\n"::(continue())
          | #"t" => #"\t"::(continue())
          | #"r" => #"\r"::(continue())
          | #"\\" => #"\\"::(continue())
          | #"\"" => #"\""::(continue())
          | c => Utils.error(Utils.LEXER(context), "Unknown command character '\\" ^ (String.implode [c]) ^ "'"))
        end
    | convertCommandChars context (c::c'::cs) = c :: convertCommandChars context (c'::cs)

  fun isNextTokenKeyword chars =
  let
    val (_, potentialKeyword) =
      accumulateChars isCharLeadIdentifier chars charListToString
    val isNotEmpty = potentialKeyword <> ""
  in isNotEmpty andalso (Utils.member keywords potentialKeyword)
  end

  fun getToken lexer =
  let
    val state = #lexerState lexer
    val r = #currentLine lexer
    val builtInOperators = #builtInOperators lexer
    val fileName = #fileName lexer
    fun getNewLexer(state, line) =
      buildNewLexer(state, line, builtInOperators, fileName)

    fun getString([], r) =
          Utils.error(Utils.LEXER(fileName, r), "Got EOF while reading string literal on line " ^ (Int.toString r))
      | getString(c::cs, r) =
          let
            val context = (fileName, r)
            fun accumulate(#"\""::cs, acc) = (cs, (String.implode o (convertCommandChars context) o List.rev) acc)
              | accumulate(c::cs, acc) = accumulate(cs, c::acc)
              | accumulate([], acc) =
                  Utils.error(Utils.LEXER context, "Got EOF while reading string literal on line " ^ (Int.toString r))
          in
            accumulate(c::cs, [])
          end

  in (case state of
      [] => ((EOF, r), getNewLexer([], r))
    | (#" "::cs) => getToken(getNewLexer(cs, r))
    | (#"\t"::cs) => getToken(getNewLexer(cs, r))
    | (#"("::cs) => ((OPEN_PAREN, r), getNewLexer(cs, r))
    | (#")"::cs) => ((CLOSE_PAREN, r), getNewLexer(cs, r))
    | (c::cs) =>
      (* NEW LINE LEXING *)
      if Char.isSpace c then getToken(getNewLexer(cs, r + 1))

      (* COMMA LEXING *)
      else if c = #"," then
        ((COMMA, r), getNewLexer(cs, r))

      (* MODULE REFERENCE LEXING *)
      else if c = #"." then
        ((DOT, r), getNewLexer(cs, r))

      (* STRING LEXING *)
      else if c = #"\"" then
        let
          val (remainingChars, result) = getString(cs, r)
        in
          ((STRING_LITERAL result, r), getNewLexer(remainingChars, r))
        end

      (* LAMBDA BAR LEXING *)
      else if c = #"|" then
        ((LAMBDA_BAR, r), getNewLexer(cs, r))

      (* COMMENT LEXING *)
      else if c = commentChar then
        let
          val (afterComment, _) =
            accumulateChars (fn c => c <> #"\n") cs (fn x => x)
        in
          getToken(getNewLexer(afterComment, r))
        end

      (* OPERATOR LEXING *)
      else if Utils.member opChars c then
        let
          val (remainingChars, result) =
            accumulateChars (Utils.member opChars) (c::cs) charListToString
        in
          ((OPERATOR result, r), getNewLexer(remainingChars, r))
        end

      (* INTEGER LEXING *)
      else if Char.isDigit c then
        let
          val (remainingChars, result) =
            accumulateChars (Char.isDigit) (c::cs) (Int.fromString o charListToString)
        in
          (case result of
                NONE => errorReport("Unexpected error while lexing integer", lexer)
              | SOME i =>
                  (((INTEGER i), r), getNewLexer(remainingChars, r)))
        end

      (* IDENTIFIER LEXING *)
      else if isNextTokenIdentifier (c::cs) then
        let
          val (remainingChars, result) = accumulateChars isCharInRestOfIdentifier (c::cs) charListToString
          val restOfLexer = getNewLexer(remainingChars, r)
        in
          if (Utils.member builtInOperators result) then
            ((OPERATOR result, r), restOfLexer)
          else if String.isPrefix Utils.RESERVED_PREFIX result then
            Utils.error(Utils.LEXER(fileName, r), "Identifiers beginning with '" ^
              Utils.RESERVED_PREFIX ^ "' are reserved for internal use")
          else
            ((IDENTIFIER result, r), restOfLexer)
        end

      (* ANNOTATION LEXING *)
      else if c = #"@" then
        ((ANNOTATION, r), getNewLexer(cs, r))

      (* KEYWORD LEXING *)
      else if isNextTokenKeyword (c::cs) then
        let
          val (remainingChars, result) = accumulateChars isCharLeadIdentifier (c::cs) (getKeywordLabel o charListToString)
          val remainingLexer = getNewLexer(remainingChars, r)
        in (case result of
                 NONE => Utils.unexpectedError("Unexpected error getting keyword")
               | SOME(k) => ((k, r), remainingLexer))
        end


        else errorReport(("Unrecognized input character \"" ^ (Char.toString c) ^ "\""), lexer))
  end

end
