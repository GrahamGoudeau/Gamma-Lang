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
    | LAMBDA
    | INTEGER of int
    | OPERATOR of string
    | OPEN_PAREN
    | CLOSE_PAREN
    | ANNOTATION
    | COMMA
    | MODULE_BEGIN
    | CONSTANT

  type line = int
  type token = tokenLabel * line
  type lexer = (char list * line)
  exception UnexpectedLexerError of string

  datatype lexerResult = OK of token | ERROR of string

  val keywordMappings = [("function", FUNCTION_START),
                         ("end", BLOCK_END),
                         ("lambda", LAMBDA),
                         ("impure", IMPURE),
                         ("var", DECLARE_VAR),
                         ("module", MODULE_BEGIN),
                         ("constant", CONSTANT),
                         ("do", BLOCK_BEGIN)
                        ]

  val keywords = List.map (fn (keyword, _) => keyword) keywordMappings

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
    | tokenToString label =
        (case getLabelString label of
              NONE => raise (UnexpectedLexerError "Unexpected error getting label string")
            | SOME(l) => ("{keyword '" ^ l ^ "'}"))

  fun errorReport(message, lineNo, lexer) =
    (ERROR (message ^ " on line " ^ (Int.toString lineNo)), lexer)

  fun newLexer cs = (cs, 1)

  fun member list elem = List.exists (fn x => x = elem) list

  val opChars = [#"+", #"-", #"/", #"*", #"$", #"<", #">",
                 #"=", #".", #":", #"~", #"^"]

  fun peekChar [] = NONE
    | peekChar (c::cs) = SOME c

  val charListToString = String.concat o (List.map Char.toString)

  fun id x = x

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
        andalso (not (member keywords potentialIdent))
  end

  fun isNextTokenKeyword chars =
  let
    val (_, potentialKeyword) =
      accumulateChars isCharLeadIdentifier chars charListToString
    val isNotEmpty = potentialKeyword <> ""
  in isNotEmpty andalso (member keywords potentialKeyword)
  end

  fun getToken ([], r) = (OK (EOF, r), ([], r))
    | getToken (#" "::cs, r) = getToken(cs, r)
    | getToken (#"\t"::cs, r) = getToken(cs, r)
    | getToken (#"("::cs, r) = (OK (OPEN_PAREN, r), (cs, r))
    | getToken (#")"::cs, r) = (OK (CLOSE_PAREN, r), (cs, r))
    | getToken (c::cs, r) =

      (* NEW LINE LEXING *)
      if Char.isSpace c then getToken(cs, r + 1)

      (* COMMA LEXING *)
      else if c = #"," then
        (OK (COMMA, r), (cs, r))

      (* OPERATOR LEXING *)
      else if member opChars c then
        let
          val (remainingChars, result) =
            accumulateChars (member opChars) (c::cs) charListToString
        in
          (OK (OPERATOR result, r), (remainingChars, r))
        end

      (* INTEGER LEXING *)
      else if Char.isDigit c then
        let
          val (remainingChars, result) =
            accumulateChars (Char.isDigit) (c::cs) (Int.fromString o charListToString)
        in
          (case result of
                NONE => errorReport("Unexpected error while lexing integer", r, ((c::cs), r))
              | SOME i =>
                  (OK ((INTEGER i), r), (cs, r)))
        end

      (* IDENTIFIER LEXING *)
      else if isNextTokenIdentifier (c::cs) then
        let
          val (remainingChars, result) = accumulateChars isCharInRestOfIdentifier (c::cs) charListToString
        in (OK (IDENTIFIER result, r), (remainingChars, r))
        end

      (* ANNOTATION LEXING *)
      else if c = #"@" then
        (OK (ANNOTATION, r), (cs, r))

      (* KEYWORD LEXING *)
      else if isNextTokenKeyword (c::cs) then
        let
          val (remainingChars, result) = accumulateChars isCharLeadIdentifier (c::cs) (getKeywordLabel o charListToString)
          val remainingLexer = (remainingChars, r)
        in (case result of
                 NONE => (ERROR "Unexpected error getting keyword", remainingLexer)
               | SOME(k) => (OK (k, r), remainingLexer))
        end


      else errorReport(("Unrecognized input character \"" ^ (Char.toString c) ^ "\""), r, (c::cs, r))
end
