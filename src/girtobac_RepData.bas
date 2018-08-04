/'* \file girtobac_RepData.bas
\brief Auxiliary classes for lists and replacements.

This file contains two auxiliary classes for a FIFO (first in first
out) stack and for key / value pairs (checks for a key and return a
value, if any). The file also contains the parser for the configuration
file.

'/


/'* \brief Class to store key / value pairs

This class stores key / value pairs. The key must not contain CHR(0) to
CHR(2) characters, because these characters are used internally.
Counters for the number of entries in the class and the number of
matches for each entry are provided.

'/
TYPE RepData
  AS CONST gchar PTR Na  '*< the name of the XML-tag
  AS STRING _
    Va, _                '*< buffer holding keys and match-counters
    Ke = MKL(0) & CHR(1) '*< buffer holding values and internal data
  DECLARE CONSTRUCTOR(BYVAL T AS CONST gchar PTR)
  DECLARE FUNCTION Az() AS LONG
  DECLARE SUB add(BYVAL AS CONST ZSTRING PTR, BYVAL AS CONST ZSTRING PTR)
  DECLARE FUNCTION rep(BYVAL T AS CONST gchar PTR) AS CONST ZSTRING PTR
  DECLARE SUB list(BYREF Li AS STRING)
END TYPE

'* \brief the constructor stores the tag name
CONSTRUCTOR RepData(BYVAL T AS CONST gchar PTR)
  Na = T
END CONSTRUCTOR

/'* \brief provide number of entries in the buffer
\returns entries in buffer

Each time a key / value pair gets added to the class, a counter gets
increased. This function provides the counter result. (An entry can get
added only once, a duplicate doesn't count.)

'/
FUNCTION RepData.Az() AS LONG : RETURN *CAST(INTEGER PTR, SADD(Ke))
END FUNCTION


/'* \brief Generate a list of un-used symbols
\param Li string variable to collect the un-used symbols

Each key match gets counted. This procedure adds the un-used (zero
counted) keys to the list in the string.

'/
SUB RepData.list(BYREF Li AS STRING)
  VAR r = "", a = 6, e = INSTR(a, Ke, CHR(2)), t = ""
  WHILE e > a
    VAR l = e - a : e += 1
    VAR x = INSTR(e, Ke, CHR(1)), z = SADD(Va) + VALINT("&h" & MID(Ke, e, x - e))
    VAR n = *CAST(LONG PTR, z - 4)
    IF 0 = n THEN Li &=  NL !"\t" & MID(Ke, a, l) & " <" & *Na & ">"
    a = x + 1 : e = INSTR(a, Ke, CHR(2))
  WEND
END SUB


/'* \brief Add entries to buffers
\param S The text to search for
\param E The replacement for this search text
\returns Zero on success (error text otherwise)

This function gets called to add some text to the class. It adds both,
the key and the value to the buffers and sets their counter to zero. In
case of duplicates or illegal characters in the search string an error
text gets returned, but zero on success. There's no checking of the
value string, it mustn't contain zero characters.

'/
SUB RepData.add(BYVAL S AS CONST ZSTRING PTR, BYVAL E AS CONST ZSTRING PTR)
  IF *S = "" THEN ?"Empty <" & *Na & "> search attribute!" : EXIT SUB
  IF INSTR(*S, ANY !"\000\001\002") THEN ?"Undefined char in <" & *Na & ">: " & *S : EXIT SUB
  VAR c = CHR(1) & *S & CHR(2) : IF INSTR(5, Ke, c) THEN ?"Duplicated  <" & *Na & ">: " & *S : EXIT SUB
  *CAST(LONG PTR, SADD(Ke)) += 1 : Ke &= MID(c, 2) & HEX(LEN(Va) + 4) & CHR(1)
  Va &= MKL(0) & *E & CHR(0)
END SUB


/'* \brief Get replacement for a text (if any))
\param T The text to search for
\returns The replacemnt (if any) or the original text

This function searches the value for a key. In case of a match
(case-sensitive) it returns a pointer to the value string. In case of
no match a pointer to the original value gets returned.

'/
FUNCTION RepData.rep(BYVAL T AS CONST gchar PTR) AS CONST ZSTRING PTR
  IF 0 = T THEN RETURN @"/'unknown'/"
  VAR a = INSTR(Ke, CHR(1) & *T & CHR(2)) : IF a THEN a += LEN(*T) + 2 ELSE RETURN T
  VAR e = INSTR(a, Ke, CHR(1)) + 1
  DIM AS ZSTRING PTR z = SADD(Va) + VALINT("&h" & MID(Ke, a, e - a))
  *CAST(LONG PTR, z - 4) += 1 : RETURN z
END FUNCTION


/'* \brief A simple fifo stack, used as an ordered list

This is a simple first in first out stack. It's used to store the
symbols for the ordered part of the header (passX). The entries are
separated by ; characters.

'/
TYPE Stack
  AS INTEGER A = 1    '*< Start position of next entry (-1)
  AS CONST STRING Sep = ";" '*< The separator character
  AS       STRING Dat = ";" '*< The buffer for entries (= data, initialized with separator)
  DECLARE SUB add(BYVAL AS CONST gchar PTR)
  DECLARE FUNCTION nxt() AS STRING
  DECLARE FUNCTION find(BYVAL T AS CONST gchar PTR) AS INTEGER
END TYPE


/'* \brief Add entries to Stack data
\param S The text to add

This SUB gets called to add a word to the Stack. It adds the text
and the separator character ';'. It's possible to add more than one
word, but the text must not contain the separator character. Here
it's used for symbol names (without white-spaces).

'/
SUB Stack.add(BYVAL S AS CONST gchar PTR)
  IF 0 = find(S) THEN Dat &= *S & Sep : EXIT SUB
  ?" found double <first>: " & *S
END SUB


/'* \brief The text of the next entry
\returns Retrieves the next entry (if any)

This function is used to read the next entry from the stack. If the
stack is empty or the end is reached, an empty STRING gets returned. In
order to retrieve the entries a second time, re-initialize the position
counter (A = 2).

'/
FUNCTION Stack.nxt() AS STRING
  VAR x = A : A = INSTR(A + 1, Dat, Sep) : IF 0 = A THEN RETURN ""
  x += 1 : RETURN MID(Dat, x, A - x)
END FUNCTION


/'* \brief Search the stack for a text
\param T Text to search for
\returns Text position in stack buffer (or zero)

This function is used to find a text in the text buffer. In case of a
found the text position (1 based) gets returned (zero otherwise).)

'/
FUNCTION Stack.find(BYVAL T AS CONST gchar PTR) AS INTEGER
  RETURN INSTR(Dat, Sep & *T & Sep)
END FUNCTION


DIM SHARED AS RepData _
  FB_TYP = @"type", _ '*< A RepData class to store replacements for type declarations
  FB_NAM = @"name"    '*< A RepData class to store replacements for symbols (names)
'&static RepData FB_TYP, FB_NAM;
DIM SHARED AS Stack _
  FIRST     '*< FiFo stack for ordered elements (pass X))


'* The \GMP for the configuration file `*.GirToBac`.
'& SUB_CDECL g2b_parser(){
'& RepData.add(); find_value();
G2b_parser:
_START_PARSER(G2b)

  SELECT CASE *element_name
  CASE "first"
    VAR n = find_value("search", AttNams, AttVals)  ' *< local variable
    IF 0 = n THEN EXIT SELECT
    FIRST.add(n)
  CASE *FB_TYP.Na
    VAR s = find_value("search", AttNams, AttVals)  ' *< local variable
    IF 0 = s THEN EXIT SELECT
    VAR r = find_value("replace", AttNams, AttVals) ' *< local variable
    VAR a = find_value("add", AttNams, AttVals)     ' *< local variable
    IF r THEN FB_TYP.add(s, r) : EXIT SELECT

    VAR e = *s & *a ' *< local variable
    FB_TYP.add(s, e)
  CASE *FB_NAM.Na
    VAR s = find_value("search", AttNams, AttVals)  ' *< local variable
    IF 0 = s THEN EXIT SELECT
    VAR r = find_value("replace", AttNams, AttVals) ' *< local variable
    VAR a = find_value("add", AttNams, AttVals)     ' *< local variable
    IF r THEN FB_NAM.add(s, r) : EXIT SELECT

    VAR e = *s & *a ' *< local variable
    FB_NAM.add(s, SADD(e))
  CASE "binary"
    VAR n = find_value("name", AttNams, AttVals)    ' *< local variable
    IF 0 = n THEN EXIT SELECT
    .NamDll = *n
  CASE "check"
    VAR n = find_value("name", AttNams, AttVals)    ' *< local variable
    IF 0 = n THEN EXIT SELECT
    .Check = *n
  CASE "pack"
    VAR n = find_value("name", AttNams, AttVals)    ' *< local variable
    IF 0 = n THEN EXIT SELECT
    .NamSpace = *n
  CASE "code" ' collect when tag is closing

_END_PARSER(G2b)

  SELECT CASE *element_name
  CASE *FB_TYP.Na, *FB_NAM.Na, "first", "binary", "check", "pack", "code"

  CASE ELSE
    g_markup_parse_context_pop(ctx)
  END SELECT
  END WITH
 END SUB

 SUB text_G2b CDECL( _
  BYVAL Ctx AS GMarkupParseContext PTR, _
  BYVAL Text AS CONST gchar PTR, _
  BYVAL Size AS gsize, _
  BYVAL UserData AS gpointer, _
  BYVAL Error_ AS GError PTR PTR)

  PEEK(Context, UserData).UserCode &= left(*Text, Size)
 END SUB

 STATIC SHARED AS GMarkupParser G2b_parser = TYPE(@start_G2b, @end_G2b, @text_G2b, NULL, NULL)
'& };
