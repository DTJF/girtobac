/'* \file girtobac_RepData.bas
\brief Auxiliary classes for lists and replacements.

This file contains two auxiliary classes for a list (first in first out
stack) and for a replacement (checks for a text and return a
replacement, if any). It also contains the parser for the configuration
file.

'/


/'* \brief Object to check texts (Su) and provide replacements (Er)

This object can get feeded by search and replacement texts. After
adding some pairs the RepData::rep() member function is used to
receive the replacement text for a match or the original text
otherwise. The search is done case-sensitive. Search text must not
contain CHR(0) to CHR(2), because these characters are used
internally.

The object counts the number of entries and the number of matches
for each entry. The list member function executes a callback
function on each entry.

'/
TYPE RepData
  AS CONST gchar PTR Na  '*< the name of the XML-tag
  AS STRING _
    Er, _                '*< buffer holding replacement texts and match-entry-counters
    Su = MKL(0) & CHR(1) '*< buffer holding the entry counter, search texts and the start of their replacements
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

Each time a text gets added to the UDT a counter gets increased.
This function provides the counter result. (An entry can get added
only once, a duplicate doesn't get counted.)

'/
FUNCTION RepData.Az() AS LONG : RETURN *CAST(INTEGER PTR, SADD(Su))
END FUNCTION


/'* \brief Generate a list of un-used symbols
\param Li string to list un-used symbols

Each symbol-found gets counted. This procedure adds the un-used
symbols to the list in the string.

'/
SUB RepData.list(BYREF Li AS STRING)
  VAR r = "", a = 6, e = INSTR(a, Su, CHR(2)), t = ""
  WHILE e > a
    VAR l = e - a : e += 1
    VAR x = INSTR(e, Su, CHR(1)), z = SADD(Er) + VALINT("&h" & MID(Su, e, x - e))
    VAR n = *CAST(LONG PTR, z - 4)
    IF 0 = n THEN Li &=  NL !"\t" & MID(Su, a, l) & " <" & *Na & ">"
    a = x + 1 : e = INSTR(a, Su, CHR(2))
  WEND
END SUB


/'* \brief Add entries to buffers
\param S The text to search for
\param E The replacement for this search text
\returns Zero on success (error text otherwise)

This function gets called to add some text to the UDT. It adds both,
the text to search for and the text replacement to the buffers and
sets their counter to zero. In case of duplicates or illegal
characters in the search string an error text gets returned (and
zero on success). There's no checking of the replacement string, it
mustn't contain zero characters.

'/
SUB RepData.add(BYVAL S AS CONST ZSTRING PTR, BYVAL E AS CONST ZSTRING PTR)
  IF *S = "" THEN ?"Empty <" & *Na & "> search attribute!" : EXIT SUB
  IF INSTR(*S, ANY !"\000\001\002") THEN ?"Undefined char in <" & *Na & ">: " & *S : EXIT SUB
  VAR c = CHR(1) & *S & CHR(2) : IF INSTR(5, Su, c) THEN ?"Duplicated  <" & *Na & ">: " & *S : EXIT SUB
  *CAST(LONG PTR, SADD(Su)) += 1 : Su &= MID(c, 2) & HEX(LEN(Er) + 4) & CHR(1)
  Er &= MKL(0) & *E & CHR(0)
END SUB


/'* \brief Get replacement for a text (if any))
\param T The text to search for
\returns The replacemnt (if any) or the original text

This function searches for a text in the search buffer. On found the
text (case-sensitive) it returns a pointer to the replacement string
(defined when calling the function RepData::add()). In case of no
match in the search buffer a pointer to the original text gets
returned.

'/
FUNCTION RepData.rep(BYVAL T AS CONST gchar PTR) AS CONST ZSTRING PTR
  IF 0 = T THEN RETURN @"/'unknown'/"
  VAR a = INSTR(Su, CHR(1) & *T & CHR(2)) : IF a THEN a += LEN(*T) + 2 ELSE RETURN T
  VAR e = INSTR(a, Su, CHR(1)) + 1
  DIM AS ZSTRING PTR z = SADD(Er) + VALINT("&h" & MID(Su, a, e - a))
  *CAST(LONG PTR, z - 4) += 1 : RETURN z
END FUNCTION

/'* \brief A simple fifo stack list

This is a simple first in first out stack. It's used to store the
symbols for the ordered part of the header (passX). The entries are
separated by ; characters.

'/
TYPE Stack
  AS STRING Dat = ";" '*< The buffer for entries, ; separated
  AS INTEGER A = 1    '*< Start position of next entry (-1)
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
  IF 0 = find(S) THEN Dat &= *S & ";" : EXIT SUB
  ?"Duplicated <first>: " & *S
END SUB


/'* \brief The text of the next entry
\returns Retrieves the next entry

The stack is meant to add symbols in the required order (read from
the configuration file) and retrieve the symbol names later on in
the given order, each entry once. To retrieve the entries a second
time the position counter needs to get re-initialized (A = 2).)

'/
FUNCTION Stack.nxt() AS STRING
  VAR x = A : A = INSTR(A + 1, Dat, ";") : IF 0 = A THEN RETURN ""
  x += 1 : RETURN MID(Dat, x, A - x)
END FUNCTION


/'* \brief Search the stack for a text
\param T Text to search for
\returns Text position in stack buffer (or zero)

This function is used to find a text in the text buffer. On found
the text position gets returned (zero otherwise).)

'/
FUNCTION Stack.find(BYVAL T AS CONST gchar PTR) AS INTEGER
  RETURN INSTR(Dat, ";" & *T & ";")
END FUNCTION


DIM SHARED AS RepData _
  FB_TYP = @"type", _ '*< Replacements for type declarations
  FB_NAM = @"name"    '*< Replacements for symbols (names)
DIM SHARED AS Stack _
  FIRST     '*< FiFo stack for ordered elements (pass X))


'* The \GMP for the configuration file \em *.GirToBac.
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

_END_PARSER(G2b)

  SELECT CASE *element_name
  CASE *FB_TYP.Na, *FB_NAM.Na, "first", "binary", "check", "pack"

_NEW_PARSER(G2b)
'& };
