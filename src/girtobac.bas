/'* \file girtobac.bas
\brief Main source code file

This file contains the main source code. Compile this file with the
FreeBasic compiler \em fbc to create an executable GirToBac tool.

The main content of this file are different parsers used by the <A
HREF="https://developer.gnome.org/glib/stable/glib-Simple-XML-Subset-Parser.html">
GMarkupParser </A>. Each parser is declared as a UDT containing five
function adresses. Only two are used here (\em start_element and \em
end_element). For better readability macros are used for the
relapsing part of the function declarations and the UDT
declarations of each parser. Additionally, labels are placed in
front of each parser to get a mark in the IDE labels list for easy
jumping between the parser definitions. (The labels aren't used in
the source code.)

'/


/'

GLib provides a simple XML parser, that is here used to read the
context of the input files (*.gir) and the control files (*.GirToBac).

'/

#INCLUDE ONCE "../Gir/GLib-2.0.bi"
'#INCLUDE ONCE "Gir/_GLibMacros-2.0.bi"

CONST OOP = 0 '*< a flag to generate classic (C-like) headers or OOP style (currently only classic style is supported since fbc isn't ready yet)


/'* \brief ENUM used to specify a type

These enums are used to specify a type. This may be an return value
from a function, a parameter in a parameter list or a member field
in a TYPE or UNION.

'/
ENUM TypeFlags
  TYPE_VAR    '*< a variable
  TYPE_SUB    '*< a SUB (function of type 'void')
  TYPE_FUN    '*< a FUNCTION
  TYPE_VARARG '*< a variadic parameter (in a parameter list)
  TYPE_ARRAY  '*< an Array (C-like)
  TYPE_LIST   '*< a list (GLib type)
END ENUM

/'* \brief TYPE struct used in the parsers

The fields of this TYPE are used in the different parsers to exchain
the parsed context.

'/
TYPE UserUDT
  AS STRING _
    FunNam _    '*< The FB name of a SUB/FUNCTION/callback
  , FunTyp _    '*< The type of a FUNCTION/callback ("" = SUB)
  , FunDll _    '*< The function name in the dll
  , FieldNam _  '*< The FB name of a field
  , FieldVal _  '*< The vale of a field
  , OopDll _    '*< The FB variable for OOP ???
  , Check _     '*< The symbol to check for (skip binding if prersent)
  , NamSpace _  '*< The namespace of the library
  , NamDll _    '*< The name of the library to include
  , NextElm _   '*< The name of the next element in the ordered list
  , ArrayTyp _  '*< The type of an array
  , Typ _       '*< The type of a field
  , TypC _      '*< The type of a field in C-style
  , ParaStr     '*< A parameter list
  AS gint32 _
    ParaBy _    '*< The parameter passing (BYVAL/BYREF)
  , ParaCnt _   '*< The number of parameters in a list
  , BlockCnt _  '*< The number of entries in a block (ENUM/UNION/TYPE)
  , type_flg _  '*< The tye of a variable (see #TypeFlags)
  , ArrayLen _  '*< The length or size of an array
  , FieldBits _ '*< The number of bits (in a bitfield)
  , PropRW _    '*< The PROPERTY style (read/write)
  , FnrBi _     '*< The file number for output
  , SkipElem    '*< Wether to skip the current element (in ordered parsing process)
  AS CONST gchar PTR _
    FuncSkip _  '*< Used as flag to skip a function
  , GErrr       '*< Used as flag to add a GError PTR PTR in parameter lists

  AS STRING _
    Raus(15) _  '*< Strings for output (0-level, current level and 14 levels of nested blocks)
  , Nams(15)    '*< Names of the current block on each level (0 to 15)
  AS gint32 _
    Level = 1 _ '*< A counter for the current level
  , RausMax = 1 '*< The highest level of output (nested blocks)
END TYPE


/'* \brief Find an attribute by its name
\param Nam The attribute name
\param AttNams The GLib array of attribute names (zero terminated)
\param AttVals The GLib array of attribute values
\returns A pointer to the attribute value (or zero)

The GLib XML parser lists all attributes found in a tag and their
values in the arrays AttNams and AttVals. This function finds an
attribute by its name and returns its value. Otherwise it returns
zero if the specified attribute isn't present.

'/
FUNCTION find_value( _
  BYVAL Nam AS CONST gchar PTR, _
  BYVAL AttNams AS CONST gchar PTR PTR, _
  BYVAL AttVals AS CONST gchar PTR PTR) AS CONST gchar PTR

  VAR i = 0
  WHILE AttNams[i]
    IF *AttNams[i] = *Nam THEN RETURN AttVals[i]
    i += 1
  WEND : RETURN NULL
END FUNCTION


'* New line character macro (for better readability)
#DEFINE NL !"\n" &


/'* \brief Macro to start a parser

Each parser uses the same parameter list for the start function.
This macro generates the code for such a procedure (SUB) and opens a
WITH block to support access to the #UserUDT data.

It's designed to be used in combination with the _END_PARSER() macro.

'/
#MACRO _START_PARSER(_N_)
 SUB start_##_N_ CDECL( _
  BYVAL ctx AS GMarkupParseContext PTR, _
  BYVAL element_name AS CONST gchar PTR, _
  BYVAL AttNams AS CONST gchar PTR PTR, _
  BYVAL AttVals AS CONST gchar PTR PTR, _
  BYVAL UserData AS gpointer, _
  BYVAL error_ AS GError PTR PTR)
   WITH PEEK(UserUDT, UserData)
#ENDMACRO

/'* \brief Macro to complete a start parser and open an end parser

Each parser uses the same code for ending a start function and the
same parameter list for the end function. This macro generates the
code to finish the start procedure (END WITH/END SUB), open an end
procedure (SUB with constant parameter list) and opens a WITH block
to support access to the #UserUDT data.

It's designed to be used after the _START_PARSER() macro and in combination
with the _NEW_PARSER() macro.

'/
#MACRO _END_PARSER(_N_)
  CASE ELSE
    'PRINT #.FnrBi, NL "  ' " & __FB_FUNCTION__ & " Skipping " & *element_name _
        ' & " """ & *find_value("name", AttNams, AttVals) & """";
    g_markup_parse_context_push(ctx, @Skip_parser, UserData) '& skip_parser();
  END SELECT
  END WITH
 END SUB
 SUB end_##_N_ CDECL( _
  BYVAL ctx AS GMarkupParseContext PTR, _
  BYVAL element_name AS CONST gchar PTR, _
  BYVAL UserData AS gpointer, _
  BYVAL error_ AS GError PTR PTR)
   WITH PEEK(UserUDT, UserData)
#ENDMACRO


/'* \brief Macro to complete an end parser and initialize the \GMP UDT

Each parser uses the same code for ending an end procedure. This
macro generates the code to finish the end procedure and generates a
structure (TYPE) to use the parser. This TYPE contains two
functions (procedures for start and end of a XML tag).

It's designed to be used after the _END_PARSER() macro.

'/
#MACRO _NEW_PARSER(_N_)
  CASE ELSE
    g_markup_parse_context_pop(ctx)
  END SELECT
  END WITH
 END SUB
 STATIC SHARED AS GMarkupParser _N_##_parser = TYPE(@start_##_N_, @end_##_N_, NULL, NULL, NULL)
#ENDMACRO


'* The \GMP for skipping XML-tags
STATIC SHARED AS GMarkupParser Skip_parser = TYPE(NULL, NULL, NULL, NULL, NULL)

#INCLUDE ONCE "girtobac_RepData.bas"

START_FUNC:
/'* \brief Generate code to start parsing a function

This macro is used to start the function parser. The snipped is used
several times, so this macro makes it unique (single source).

'/
#MACRO _START_FUNC()
  .ParaCnt = 0 : .ParaStr = "(" : .FunTyp = ""
  .GErrr = find_value("throws", AttNams, AttVals)
  IF OOP THEN
    .FunNam = *FB_NAM.rep(find_value("name", AttNams, AttVals))
    .FunDll = *find_value("c:identifier", AttNams, AttVals)
  ELSE
    .FunNam = *FB_NAM.rep(find_value("c:identifier", AttNams, AttVals))
  END IF
  g_markup_parse_context_push(ctx, @Func_parser, UserData) '& func_parser();
#ENDMACRO

/'* \brief Generate code to end parsing a function

This macro is used to end the function parser. The snipped is used
several times, so this macro makes it unique (single source).

'/
#MACRO _END_FUNC()
  IF .GErrr THEN
    IF .ParaCnt THEN .ParaStr &= ", "
    .ParaStr &= "BYVAL AS GError PTR PTR" : .ParaCnt += 1
  END IF
  g_markup_parse_context_pop(ctx)
  .type_flg = IIF(LEN(.FunTyp), TYPE_FUN, TYPE_SUB)
  .ParaStr &= ")"
#ENDMACRO


/'* \brief Generate an FB type
\param Ud The data in the #UserUDT
\returns An FB type string

Type declarations may need adaptions. We have to follow the rules
defined in the *.GirToBac file to change some types. Also they may
contain '*' characters to be translated to 'PTR' strings for FB.

In seldom cases a <em>*.gir</em> declaration doesn't contain an
entry for a C type. Then we try repairing it by using the type used
in the namespace instead.

'/
FUNCTION fb_type(BYVAL Ud AS ANY PTR) AS STRING
  WITH PEEK(UserUDT, Ud)
    IF 0 = LEN(.TypC) THEN '               try to fix it (may be faulty)
      SELECT CASE .Typ
      CASE "utf8", "filename" : RETURN "/'!'/gchar PTR"
      CASE ELSE
        IF 0 = LEN(.Typ) THEN RETURN "/'!'/ any PTR"
        .TypC = .Typ
      END SELECT
    ELSE
      IF LEFT(.Typc, 9) = "volatile " THEN .TypC = MID(.TypC, 10)
      IF .Typc = "gchar" THEN .TypC = "gint8"
    END IF

    VAR ptrs = 0, i = LEN(.TypC) - 1
    FOR i = i TO 0 STEP -1
      IF .TypC[i] = ASC("*") THEN ptrs += 1 ELSE EXIT FOR
      .TypC[i] = 0
    NEXT

    IF OOP THEN .Typ = *FB_TYP.rep(.Typ) _
           ELSE .Typ = *FB_TYP.rep(.TypC)

    FOR p AS INTEGER = 1 TO ptrs
      .Typ &= " PTR"
    NEXT : RETURN .Typ
  END WITH
END FUNCTION

'* Forward declaration (due to circular references)
DECLARE SUB start_type CDECL( _
  BYVAL AS GMarkupParseContext PTR, _
  BYVAL AS CONST gchar PTR, _
  BYVAL AS CONST gchar PTR PTR, _
  BYVAL AS CONST gchar PTR PTR, _
  BYVAL AS gpointer, _
  BYVAL AS GError PTR PTR)

'* Forward declaration (due to circular references)
DECLARE SUB end_type CDECL( _
  BYVAL AS GMarkupParseContext PTR, _
  BYVAL AS CONST gchar PTR, _
  BYVAL AS gpointer, _
  BYVAL AS GError PTR PTR)

'* The \GMP for the type XML-tags
STATIC SHARED AS GMarkupParser _
  Type_parser = TYPE(@start_type, @end_type, NULL, NULL, NULL)


'* The \GMP for the parameter lists
'& SUB_CDECL para_parser(){
Para_parser:
_START_PARSER(Para)

  SELECT CASE *element_name
  CASE "parameter", "instance-parameter"
    'if OOP then
      'var d = find_value("direction", AttNams, AttVals)
      '.ParaBy = iif(*d <> "in", 1, 0)
    'end if
    g_markup_parse_context_push(ctx, @Type_parser, UserData) '& type_parser();

_END_PARSER(Para)

  SELECT CASE *element_name
  CASE "parameter", "instance-parameter"
    g_markup_parse_context_pop(ctx)
    IF .ParaCnt THEN .ParaStr &= ", "
    IF .type_flg = TYPE_VARARG THEN
      .ParaStr &= "..."
    ELSE
      IF .type_flg = TYPE_ARRAY ANDALSO LEN(.ArrayTyp) THEN .TypC = .ArrayTyp
      IF OOP THEN
        'if .ParaBy _
          'then .ParaStr &= "BYREF" : .ParaBy = 0 _
          'else .ParaStr &= "BYVAL"
        '.ParaStr &= " P" & .ParaCnt & " AS " & fb_type(UserData)
        .ParaStr &= "BYVAL P" & .ParaCnt & " AS " & fb_type(UserData)
      ELSE
        .ParaStr &= "BYVAL AS " & fb_type(UserData)
      END IF
    END IF : .type_flg = 0
    .ParaCnt += 1

_NEW_PARSER(Para)
'& };

'* The \GMP for the functions (function, method, constructor, callback)
'& SUB_CDECL func_parser(){
Func_parser:
_START_PARSER(Func)

  SELECT CASE *element_name
  CASE "return-value"
    g_markup_parse_context_push(ctx, @Type_parser, UserData) '& type_parser();
  CASE "parameters"
    g_markup_parse_context_push(ctx, @Para_parser, UserData) '& para_parser();

_END_PARSER(Func)

  SELECT CASE *element_name
  CASE "return-value"
    IF .type_flg = TYPE_ARRAY ANDALSO LEN(.ArrayTyp) THEN .TypC = .ArrayTyp
    IF .Typ <> "none" THEN .FunTyp = fb_type(UserData)
    g_markup_parse_context_pop(ctx)
  CASE "parameters"
    g_markup_parse_context_pop(ctx)

_NEW_PARSER(Func)
'& };

'* The \GMP for the types
'& SUB_CDECL type_parser(){
Type_parser:
_START_PARSER(TYPE)

  SELECT CASE *element_name
  CASE "type"
    .Typ = *find_value("name", AttNams, AttVals)
    VAR c = find_value("c:type", AttNams, AttVals) ' *< local variable
    IF c THEN .TypC = *c ELSE .TypC = ""
    SELECT CASE .Typ
    CASE "GLib.List", "GLib.SList", "GLib.Array"
      .type_flg = TYPE_LIST
      g_markup_parse_context_push(ctx, @Skip_parser, UserData) '& skip_parser();
    CASE ELSE
      .type_flg = TYPE_VAR
    END SELECT
  CASE "callback"
    _START_FUNC()
  CASE "array"
    VAR n = find_value("name", AttNams, AttVals)   ' *< local variable
    VAR t = find_value("c:type", AttNams, AttVals) ' *< local variable
    IF n THEN
      .Typ = *n
      IF t THEN .TypC = *t ELSE .TypC = ""
      .ArrayLen = -1
      g_markup_parse_context_push(ctx, @Skip_parser, UserData) '& skip_parser();
    ELSE
      n = find_value("length", AttNams, AttVals)
      IF 0 = n THEN n = find_value("fixed-size", AttNams, AttVals)
      IF t THEN .ArrayTyp = *t ELSE .ArrayTyp = ""
      .ArrayLen = IIF(n, CUINT(*n), 0)
      g_markup_parse_context_push(ctx, @Type_parser, UserData) '& type_parser();
    END IF
  CASE "varargs"

_END_PARSER(TYPE)

  SELECT CASE *element_name
  CASE "type"
    IF .Type_flg = TYPE_LIST THEN g_markup_parse_context_pop(ctx)
  CASE "callback"
    _END_FUNC()
  CASE "array"
    .type_flg = TYPE_ARRAY
    g_markup_parse_context_pop(ctx)
  CASE "varargs"
    .type_flg = TYPE_VARARG
  CASE ELSE
    g_markup_parse_context_pop(ctx)
  END SELECT

END WITH : END SUB ' no _P2() macro since we need the Type_parser above
'& };

'* The \GMP for the interfaces, records and classes (OOP style -> ToDo)
'& SUB_CDECL class_parser(){
Class_parser:
_START_PARSER(CLASS)

  SELECT CASE *element_name
  CASE "method", "function", "constructor"
    _START_FUNC()
  CASE "field"
    .FieldNam = *FB_NAM.rep(find_value("name", AttNams, AttVals))
    VAR bits = find_value("bits", AttNams, AttVals) ' *< local variable
    .FieldBits = IIF(bits, CUINT(*bits), 0)
    g_markup_parse_context_push(ctx, @Type_parser, UserData) '& type_parser();
  CASE "property"
    .FunNam = *FB_NAM.rep(find_value("name", AttNams, AttVals))
    VAR r = find_value("readable", AttNams, AttVals) ' *< local variable
    VAR w = find_value("writable", AttNams, AttVals) ' *< local variable
    .PropRW  = IIF(r ANDALSO *r = "0", 0, 1)
    .PropRW += IIF(w ANDALSO *w = "0", 0, 2)
    g_markup_parse_context_push(ctx, @Type_parser, UserData) '& type_parser();
  CASE "virtual-method"
    .ParaCnt = 0 : .ParaStr = "(" : .FunTyp = ""
    .FunNam = *FB_NAM.rep(find_value("name", AttNams, AttVals))
    g_markup_parse_context_push(ctx, @Func_parser, UserData) '& func_parser();
  CASE "implements"

_END_PARSER(CLASS)

  SELECT CASE *element_name
  CASE "constructor"
    .BlockCnt += 1
    _END_FUNC()
    IF .type_flg <> TYPE_FUN THEN .Raus(.Level) &= NL "'' ??? CTOR w/o type"

    .Raus(.Level) &= NL "  DECLARE CONSTRUCTOR " & .ParaStr
    VAR dll = "__" & .BlockCnt ' *< local variable
    .Raus(.Level) &= NL "  " & dll & " AS FUNCTION" & .ParaStr & " AS " & .FunTyp _
                  & " = DYLIBSYMBOL(" & .OopDll & ", """ & .FunDll & """)"

    .Raus(0) &= NL "CONSTRUCTOR " & .Nams(.Level) & .ParaStr
    IF .ParaCnt THEN .ParaStr = "P1" ELSE .ParaStr = ""
    FOR i AS INTEGER = 2 TO .ParaCnt
      .ParaStr &= ", P" & i
    NEXT
    .Raus(0) &= NL "  __Obj = " & dll & "(" & .ParaStr & ")"
    .Raus(0) &= NL "END CONSTRUCTOR"
    .type_flg = 0
  CASE "field"
    .BlockCnt += 1
    SELECT CASE AS CONST .type_flg
    CASE TYPE_SUB
      .Raus(.Level) &= NL "  " & .FieldNam & " AS SUB CDECL" & .ParaStr
    CASE TYPE_FUN
      .Raus(.Level) &= NL "  " & .FieldNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp
    CASE ELSE
      .Raus(.Level) &= NL "  AS " & fb_type(UserData) & " " & .FieldNam
      IF .FieldBits THEN .Raus(.Level) &= " : " & .FieldBits
    END SELECT : .type_flg = 0
    g_markup_parse_context_pop(ctx)
  CASE "virtual-method"
    .BlockCnt += 1
    _END_FUNC()
    SELECT CASE AS CONST .type_flg
    CASE TYPE_SUB : .Raus(.Level) &= NL "  " & .FunNam & " AS SUB CDECL" & .ParaStr
    CASE ELSE : .Raus(.Level) &= NL "  " & .FunNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp
    END SELECT
  CASE "method", "function"
    .BlockCnt += 1
    _END_FUNC()
    VAR dll = "__" & .BlockCnt

    VAR p = INSTR(.ParaStr, ", ") ' *< local variable
    VAR par = "("                 ' *< local variable
    IF p THEN par &= MID(.ParaStr, p + 2) ELSE par &= ")"

    VAR parcall =  UCASE(.NamSpace & "_" & .Nams(.Level)) & "(@THIS)" ' *< local variable
    FOR i AS INTEGER = 1 TO .ParaCnt - 1
      parcall &= ", P" & i
    NEXT

    SELECT CASE AS CONST .type_flg
    CASE TYPE_SUB
      .Raus(.Level) &= NL "  DECLARE SUB " & .FunNam & par
      .Raus(.Level) &= NL "  " & dll & " AS SUB" & .ParaStr _
                    & " = DYLIBSYMBOL(" & .OopDll & ", """ & .FunDll & """)"
      .Raus(0) &= NL "SUB " & .Nams(.Level) & "." & .FunNam & par
      .Raus(0) &= NL "  " & dll & "(" & parcall & ")"
      .Raus(0) &= NL "END SUB"
    CASE ELSE
      .Raus(.Level) &= NL "  DECLARE FUNCTION " & .FunNam & par & " AS " & .FunTyp
      .Raus(.Level) &= NL "  " & dll & " AS FUNCTION" & .ParaStr & " AS " & .FunTyp _
                    & " = DYLIBSYMBOL(" & .OopDll & ", """ & .FunDll & """)"
      .Raus(0) &= NL "FUNCTION " & .Nams(.Level) & "." & .FunNam & par & " AS " & .FunTyp
      .Raus(0) &= NL "  RETURN " & dll & "(" & parcall & ")"
      .Raus(0) &= NL "END FUNCTION"
    END SELECT : .type_flg = 0
  CASE "property" ' !!!
    FOR i AS INTEGER = 0 TO LEN(.FunNam) - 1
      IF .FunNam[i] = ASC("-") THEN .FunNam[i] = ASC("_")
    NEXT
    IF BIT(.PropRW, 0) THEN
      .Raus(.Level) &= NL "  DECLARE PROPERTY " & .FunNam & "() AS " & .Typ
      .Raus(0) &= NL "PROPERTY " & .Nams(.Level) & "." & .FunNam & "() AS " & .Typ
      .Raus(0) &= NL "  DIM AS " & .Typ & " __r"
      .Raus(0) &= NL "  g_object_get(__Obj, """ & .FunNam & """, @__r, NULL)"
      .Raus(0) &= NL "  RETURN __r"
      .Raus(0) &= NL "END PROPERTY"
    END IF
    IF BIT(.PropRW, 1) THEN
      .Raus(.Level) &= NL "  DECLARE PROPERTY " & .FunNam & "(BYVAL P1 AS" & .Typ & ")"
      .Raus(0) &= NL "PROPERTY " & .Nams(.Level) & "." & .FunNam & "(BYVAL AS" & .Typ & ")"
      .Raus(0) &= NL "  g_object_set(__Obj, """ & .FunNam & """, P1, NULL)"
      .Raus(0) &= NL "END PROPERTY"
    END IF
    g_markup_parse_context_pop(ctx)
  CASE "implements"

_NEW_PARSER(CLASS)
'& };

'* The \GMP for the interfaces, records and classes (C-like style)
'& SUB_CDECL udt_parser(){
Udt_parser:
_START_PARSER(Udt)

  SELECT CASE *element_name
  CASE "field"
    VAR n = find_value("name", AttNams, AttVals)
    VAR c = find_value("c:identifier", AttNams, AttVals)
    .FieldNam = *FB_NAM.rep(IIF(c, c, n))
    VAR bits = find_value("bits", AttNams, AttVals)
    .FieldBits = IIF(bits, CUINT(*bits), 0)
    g_markup_parse_context_push(ctx, @Type_parser, UserData) '& type_parser();
  CASE "virtual-method"
    g_markup_parse_context_push(ctx, @Skip_parser, UserData) '& skip_parser();
  CASE "constructor", "method", "function"
    _START_FUNC()
  CASE "property"

_END_PARSER(Udt)

  SELECT CASE *element_name
  CASE "field"
    .BlockCnt += 1
    SELECT CASE AS CONST .type_flg
    CASE TYPE_SUB
      .Raus(.Level) &= NL "  " & .FieldNam & " AS SUB CDECL" & .ParaStr
    CASE TYPE_FUN
      .Raus(.Level) &= NL "  " & .FieldNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp
    CASE ELSE
      .Raus(.Level) &= NL "  AS " & fb_type(UserData) & " " & .FieldNam
      IF .type_flg = TYPE_ARRAY ANDALSO .ArrayLen > 0 _
        THEN .Raus(.Level) &= "(" & .ArrayLen - 1 & ")"
      IF .FieldBits THEN .Raus(.Level) &= " : " & .FieldBits
    END SELECT : .type_flg = 0
    g_markup_parse_context_pop(ctx)
  CASE "virtual-method"
    g_markup_parse_context_pop(ctx)
  CASE "method", "function", "constructor"
    _END_FUNC()
    SELECT CASE AS CONST .type_flg
    CASE TYPE_SUB
      .Raus(0) &= NL "DECLARE SUB " & .FunNam & .ParaStr
    CASE ELSE
      .Raus(0) &= NL "DECLARE FUNCTION " & .FunNam & .ParaStr & " AS " & .FunTyp
    END SELECT : .type_flg = 0
  CASE "property" ' !!!

_NEW_PARSER(Udt)
'& };

'* The \GMP for the union blocks
'& SUB_CDECL unio_parser(){
Unio_parser:
_START_PARSER(Unio)

  SELECT CASE *element_name
  CASE "field"
    VAR n = find_value("name", AttNams, AttVals)         ' *< local variable
    VAR c = find_value("c:identifier", AttNams, AttVals) ' *< local variable
    .FieldNam = *FB_NAM.rep(IIF(OOP, n, IIF(c, c, n)))
    VAR bits = find_value("bits", AttNams, AttVals)      ' *< local variable
    .FieldBits = IIF(bits, CUINT(*bits), 0)
    g_markup_parse_context_push(ctx, @Type_parser, UserData) '& type_parser();
  CASE "record"
    .Level += 1 : IF .Level > UBOUND(.Raus) THEN ?"Raus maximum exeded!"
    IF .Level > .RausMax THEN .RausMax = .Level
    VAR nam = *FB_NAM.rep(find_value("name", AttNams, AttVals)) ' *< local variable
    .Nams(.Level) = "__G2B_" & .Nams(.Level - 1) & "_" & nam
    .Raus(.Level - 1) &= NL "  AS " & .Nams(.Level) & " " & nam
    .Raus(.Level) &= NL "TYPE " & .Nams(.Level)
    g_markup_parse_context_push(ctx, IIF(OOP, @Class_parser, @Udt_parser), UserData) '& class_parser(); Udt_parser();

_END_PARSER(Unio)

  SELECT CASE *element_name
  CASE "field"
    .BlockCnt += 1
    SELECT CASE AS CONST .type_flg
    CASE TYPE_SUB
      .Raus(.Level) &= NL "  " & .FieldNam & " AS SUB CDECL" & .ParaStr
    CASE TYPE_FUN
      .Raus(.Level) &= NL "  " & .FieldNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp
    CASE ELSE
      .Raus(.Level) &= NL "  AS " & fb_type(UserData) & " " & .FieldNam
      IF .FieldBits THEN .Raus(.Level) &= " : " & .FieldBits
    END SELECT : .type_flg = 0
    g_markup_parse_context_pop(ctx)
  CASE "record"
    .BlockCnt += 1
    .Raus(.Level) &= NL "END TYPE"
    .Level -= 1
    g_markup_parse_context_pop(ctx)

_NEW_PARSER(Unio)
'& };

'* The \GMP for the enum blocks
'& SUB_CDECL enum_parser(){
Enum_parser:
_START_PARSER(ENUM)

  SELECT CASE *element_name
  CASE "member"
    VAR nam = *FB_NAM.rep(find_value(*IIF(OOP, @"name", @"c:identifier"), AttNams, AttVals)) ' *< local variable
    VAR value = find_value("value", AttNams, AttVals) ' *< local variable
    PRINT #.FnrBi, NL "  " & nam;
    IF value THEN PRINT #.FnrBi, " = " & CINT(*value);

_END_PARSER(ENUM)

  SELECT CASE *element_name
  CASE "member"

_NEW_PARSER(ENUM)
'& };

'* The \GMP for the first pass
'& SUB_CDECL pass1_parser(){
Pass1_parser:
_START_PARSER(pass1)

  SELECT CASE *element_name
  CASE "constant"
    VAR n = find_value(IIF(OOP, @"name", @"c:type"), AttNams, AttVals) ' *< local variable
    IF 0 = n THEN n = find_value("c:identifier", AttNams, AttVals)
    .FieldNam = *FB_NAM.rep(n)
    .FieldVal = *find_value("value", AttNams, AttVals)
    g_markup_parse_context_push(ctx, @Type_parser, UserData) '& type_parser();
  CASE "enumeration", "bitfield"
    VAR nam = *FB_NAM.rep(find_value(IIF(OOP, @"name", @"c:type"), AttNams, AttVals))

    IF LEN(nam) THEN PRINT #.FnrBi, NL "TYPE AS LONG " & nam;
    PRINT #.FnrBi, NL "ENUM";
    'PRINT #.FnrBi, NL "ENUM " & nam;

    IF OOP THEN PRINT #.FnrBi, " EXPLICIT";
    g_markup_parse_context_push(ctx, @Enum_parser, UserData) '& enum_parser();
  CASE "alias"
    VAR n = find_value(IIF(OOP, @"name", @"c:type"), AttNams, AttVals) ' *< local variable
    IF 0 = n THEN n = find_value("glib:type-name", AttNams, AttVals)
    IF n THEN .FieldNam = *FB_NAM.rep(n) ELSE .FieldNam = ""
    g_markup_parse_context_push(ctx, @Type_parser, UserData) '& type_parser();
  CASE "class", "record", "interface"
    VAR n = find_value(IIF(OOP, @"name", @"c:type"), AttNams, AttVals) ' *< local variable
    IF 0 = n THEN n = find_value("glib:type-name", AttNams, AttVals)
    VAR nam = *FB_NAM.rep(n) ' *< local variable
    PRINT #.FnrBi, NL "TYPE AS _" & nam & " " & nam;
  CASE "include"
    PRINT #.FnrBi, NL "#INCLUDE ONCE """ & _
       *find_value("name", AttNams, AttVals) & "-" & _
       *find_value("version", AttNams, AttVals) & ".bi""";
  CASE "repository"
    PRINT #.FnrBi, NL "' Repository version " & *find_value("version", AttNams, AttVals);
  CASE "namespace"
    IF 0 = LEN(.NamSpace) THEN .NamSpace = *find_value("name", AttNams, AttVals)
    'SELECT CASE .NamSpace
    'CASE "GLib", "GObject", "GModule", "Gio" : .NamSpace = "G"
    'END SELECT
    VAR dll = find_value("shared-library", AttNams, AttVals) + 3 ' *< local variable
    IF dll > 3 ANDALSO 0 = LEN(.NamDLL) THEN .NamDll = LEFT(*dll, INSTR(*dll, ".so") - 1)

    IF 0 = OOP THEN EXIT SELECT
    IF dll > 3 THEN
      .OopDll = "__G2B_" & *find_value("name", AttNams, AttVals)
      PRINT #.FnrBi, NL "DIM SHARED AS ANY PTR " & .OopDll;
      PRINT #.FnrBi, NL .OopDll & " = DYLIBLOAD(""" & .NamDll & """)";
    END IF
    PRINT #.FnrBi, NL "NAMESPACE " & .NamSpace

_END_PARSER(pass1)

  SELECT CASE *element_name
  CASE "constant"
    PRINT #.FnrBi, NL "#DEFINE " & .FieldNam;
    SELECT CASE .Typ
    CASE "utf8", "filename"
      PRINT #.FnrBi, " @!""" & .FieldVal & """";
    CASE ELSE
      PRINT #.FnrBi, " " & .FieldVal;
    END SELECT
    g_markup_parse_context_pop(ctx)
  CASE "enumeration", "bitfield"
    PRINT #.FnrBi, NL "END ENUM";
    g_markup_parse_context_pop(ctx)
  CASE "alias"
  IF LEN(.FieldNam) _
    THEN PRINT #.FnrBi, NL "TYPE AS " & fb_type(UserData) & " " & .FieldNam; _
    ELSE PRINT #.FnrBi, NL "TYPE AS ANY " & fb_type(UserData);
    g_markup_parse_context_pop(ctx)
  CASE "class", "record", "interface"
  CASE "include", "repository", "namespace"

_NEW_PARSER(pass1)
'& };

'* The \GMP for the second pass
'& SUB_CDECL passX_parser(){
PassX_parser:
_START_PARSER(passX)
  SELECT CASE *element_name
  CASE "repository", "namespace" : EXIT SUB
  END SELECT

  VAR n = find_value(*IIF(OOP, @"name", @"c:type"), AttNams, AttVals)
  IF 0 = n THEN
    n = find_value("glib:type-name", AttNams, AttVals)
    IF 0 = n THEN n = find_value("name", AttNams, AttVals)
  END IF
  IF FIRST.A = -1 THEN '                                    we're in P_3
    IF FIRST.find(n) THEN '                       element allready done?
      .SkipElem = 1
      g_markup_parse_context_push(ctx, @Skip_parser, UserData) '& skip_parser();
      EXIT SUB
    END IF
  ELSE '                                                    we're in P_X
    IF *n <> .NextElm THEN '                             not our element
      .SkipElem = 1
      g_markup_parse_context_push(ctx, @Skip_parser, UserData) '& skip_parser();
      EXIT SUB
    END IF
    .NextElm = FIRST.nxt()
  END IF

  SELECT CASE *element_name
  CASE "interface", "class"
    IF 0 = OOP THEN '                              generate class macros
      VAR g = find_value("glib:get-type", AttNams, AttVals) ' *< local variable
      IF g ANDALSO *g <> "intern" THEN
        VAR s = find_value("glib:type-struct", AttNams, AttVals) _' *< local variable
         , t1 = UCASE(.NamSpace) & "_" _                 ' *< local variable
          , p = INSTR(*g, "_") _                         ' *< local variable
         , t2 = UCASE(MID(*g, p + 1, LEN(*g) - p - 9)) _ ' *< local variable
         , t3 = t1 & "TYPE_" & t2 _                      ' *< local variable
         , t4 = t3 & ", " & *g _                         ' *< local variable
         , t5 = .NamSpace & *s                           ' *< local variable

        .Raus(0) &= NL "DECLARE FUNCTION " & *g & "() AS GType"
        .Raus(0) &= NL "#DEFINE " & t3 & " (" & *g & "())"
        .Raus(0) &= NL "#DEFINE " & t1 & t2 & "(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), " & t3 & ", " & *n & "))"
        .Raus(0) &= NL "#DEFINE " & t1 & t2 & "_CLASS(obj) (G_TYPE_CHECK_CLASS_CAST((obj), " & t3 & ", " & t5 & "))"
        .Raus(0) &= NL "#DEFINE " & t1 & "IS_" & t2 & "(obj) (G_TYPE_CHECK_INSTANCE_TYPE((obj), " & t3 & "))"
        IF element_name[0] = ASC("i") THEN
          .Raus(0) &= NL "#DEFINE " & t1 & t2 & "_GET_IFACE(obj) (G_TYPE_INSTANCE_GET_INTERFACE((obj), " & t3 & ", " & t5 &"))"
        ELSE
          .Raus(0) &= NL "#DEFINE " & t1 & "IS_CLASS_" & t2 & "(obj) (G_TYPE_CHECK_CLASS_TYPE((obj), " & t3 & "))"
          .Raus(0) &= NL "#DEFINE " & t1 & t2 & "_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS((obj), " & t3 & ", " & t5 &"))"
        END IF
      END IF
    END IF
    .BlockCnt = 0
    .Nams(.Level) = *FB_NAM.rep(n)
    .Raus(.Level) &= NL "TYPE _" & .Nams(.Level)
    g_markup_parse_context_push(ctx, IIF(OOP, @Class_parser, @Udt_parser), UserData) '& class_parser(); Udt_parser();
  CASE "record"
    .BlockCnt = 0
    .Nams(.Level) = *FB_NAM.rep(n)
    .Raus(.Level) &= NL "TYPE _" & .Nams(.Level)
    g_markup_parse_context_push(ctx, IIF(OOP, @Class_parser, @Udt_parser), UserData) '& class_parser(); Udt_parser();
  CASE "union"
    .Nams(.Level) = *FB_NAM.rep(n)
    .Raus(.Level) = NL "UNION " & .Nams(.Level)
    g_markup_parse_context_push(ctx, @Unio_parser, UserData) '& unio_parser();
  CASE "callback"
    .ParaCnt = 0 : .ParaStr = "(" : .FunTyp = ""
    .FunNam = *FB_NAM.rep(find_value(*IIF(OOP, @"name", @"c:type"), AttNams, AttVals))
    g_markup_parse_context_push(ctx, @Func_parser, UserData) '& func_parser();

_END_PARSER(passX)

  IF .SkipElem THEN
    .SkipElem = 0
    g_markup_parse_context_pop(ctx)
    EXIT SUB
  END IF

  SELECT CASE *element_name
  CASE "record", "interface", "class"
    IF .Level <> 1 THEN ?"Raus level <> 1"
    IF .BlockCnt _
      THEN .Raus(1) &= NL "END TYPE" : .BlockCnt = 0 _
      ELSE .Raus(1)  = ""
    FOR i AS INTEGER = .RausMax TO 0 STEP -1
      PRINT #.FnrBi, .Raus(i);
      .Raus(i) = ""
    NEXT : .RausMax = 1
    g_markup_parse_context_pop(ctx)
  CASE "union"
    IF .Level <> 1 THEN ?"Raus level <> 1"
    IF .BlockCnt _
      THEN .Raus(1) &= NL "END UNION" : .BlockCnt = 0 _
      ELSE .Raus(1)  = "TYPE AS _" & .Nams(.Level) & " " & .Nams(.Level)
    FOR i AS INTEGER = .RausMax TO 0 STEP -1
      PRINT #.FnrBi, .Raus(i);
      .Raus(i) = ""
    NEXT : .RausMax = 1
    g_markup_parse_context_pop(ctx)
  CASE "callback"
    _END_FUNC()
    SELECT CASE AS CONST .type_flg
    CASE TYPE_SUB : PRINT #.FnrBi, NL "TYPE " & .FunNam & " AS SUB CDECL" & .ParaStr;
    CASE ELSE : PRINT #.FnrBi, NL "TYPE " & .FunNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp;
    END SELECT
  CASE "repository", "namespace"

_NEW_PARSER(passX)
'& };

'* The \GMP for last pass
'& SUB_CDECL pass4_parser(){
Pass4_parser:
_START_PARSER(pass4)

  SELECT CASE *element_name
  CASE "function"
    .FuncSkip = find_value("moved-to", AttNams, AttVals) '& find_value();
    IF .FuncSkip THEN EXIT SELECT
    _START_FUNC() '& func_parser();
  CASE "repository", "namespace"

_END_PARSER(pass4)

  SELECT CASE *element_name
  CASE "function"
    IF .FuncSkip THEN EXIT SELECT
    _END_FUNC()
    SELECT CASE AS CONST .type_flg
    CASE TYPE_SUB :  PRINT #.FnrBi, NL "DECLARE SUB " & .FunNam & .ParaStr;
    CASE ELSE :  PRINT #.FnrBi, NL "DECLARE FUNCTION " & .FunNam & .ParaStr & " AS " & .FunTyp;
    END SELECT : .type_flg = 0
  CASE "repository"
  CASE "namespace"
    PRINT #.FnrBi, NL "END EXTERN"
    IF OOP THEN PRINT #.FnrBi, NL "END NAMESPACE"

_NEW_PARSER(pass4)
'& };

/'* \brief Macro to run a parser

This macro creates a parser context and runs the given parser once.

'/
#MACRO PARSE(_N_)
 SCOPE
  VAR ctx = g_markup_parse_context_new(@pass##_N_##_parser, 0, @UDat, NULL)
  IF g_markup_parse_context_parse(ctx, buffer, length, @errr) THEN _
    IF 0 = g_markup_parse_context_end_parse(ctx, @errr) THEN _
      g_print(!"Cannot parse %s (invalid content)\n", filename, NULL)
  g_markup_parse_context_free(ctx)
 END SCOPE
#ENDMACRO

'& int main(){

IF __FB_ARGC__ <=1 THEN ?"Pass in filename as parameter!" : END
VAR filename = COMMAND(1) _  ' *< input file name
  , basename = MID(filename, INSTRREV(filename, ANY "\/") + 1) _ ' *< name without path
  ,    proto = "" ' *< protocol messages

IF g_str_has_suffix(filename, ".gir") _
  THEN basename = LEFT(basename, LEN(basename) - 4) _
  ELSE filename &= ".gir"

DIM AS GError PTR errr = NULL ' *< location for GLib errors
DIM AS UserUDT UDat           ' *< data to exchange between parsers

DIM AS gchar PTR buffer       ' *< buffer for input data
DIM AS gsize length           ' *< length of input data

IF 0 = g_file_get_contents(filename, @buffer, @length, @errr) THEN 
  ?"Cannot open " & filename
ELSE 
  ?"loading " & filename

WITH UDat
  .FnrBi = FREEFILE
  'if open cons(for output as .FnrBi) then
    '?"Cannot open console for output!"
  IF OPEN(basename & ".bi" FOR OUTPUT AS .FnrBi) THEN
    ?"Cannot open file for output!"
  ELSE
    PRINT #.FnrBi, "'            FreeBasic header file, auto-generated by"
    PRINT #.FnrBi, "'                       ### GirToBac ###"
    PRINT #.FnrBi, "' LGPLv2.1 (C) 2013-2015 by Thomas[ dot }Freiherr[ at ]gmx[ dot }net"
    PRINT #.FnrBi, "' Auto-translated from file " & filename

    DIM AS gchar PTR buff2 ' *< buffer for control file
    DIM AS gsize length2   ' *< length of control file
    basename &= ".GirToBac"
    IF g_file_get_contents(basename, @buff2, @length2, @errr) THEN
      ?"loading " & basename
      VAR ctx = g_markup_parse_context_new(@G2b_parser, 0, @UDat, NULL)
      IF g_markup_parse_context_parse(ctx, buff2, length2, @errr) THEN _ '& g2b_parser();
        IF 0 = g_markup_parse_context_end_parse(ctx, @errr) THEN _
          g_print(!"Cannot parse %s (invalid content)\n", basename, NULL)
      g_markup_parse_context_free(ctx)
      g_free(buff2)
    END IF
    ?NL "Translating ..."
    IF LEN(.Check) THEN PRINT #.FnrBi, "#IFNDEF " & .Check
    PRINT #.FnrBi, "#INCLUDE ONCE ""_GirToBac-0.0.bi"""

    PARSE(1) '& pass1_parser(); //                   #DEFINE, ENUM, TYPE

    IF OOP ORELSE 0 = LEN(.NamDll) THEN
      PRINT #.FnrBi, NL "EXTERN ""C""";
    ELSE
      PRINT #.FnrBi, NL "EXTERN ""C"" LIB """ & .NamDll & """";
    END IF

    PRINT #.FnrBi, NL "' P_X" '    UNIONs, CALLBACKs and TYPEs (ordered)
    .NextElm = FIRST.nxt()
    WHILE FIRST.A
      VAR xx = FIRST.A ' *< stack position
      PARSE(X) '& passX_parser();
      IF xx <> FIRST.A THEN CONTINUE WHILE
      proto &= NL !"\t" & .NextElm & " <first>"
      .NextElm = FIRST.nxt()
    WEND
    FIRST.A = -1

    PRINT #.FnrBi, NL "' P_3" '       UNIONs, CALLBACKs and TYPEs (rest)
    PARSE(X) '& passX_parser();

    PRINT #.FnrBi, NL "' P_4" '                       SUBs and FUNCTIONs
    PARSE(4) '& pass4_parser();

    PRINT #.FnrBi,
    IF LEN(.Check) THEN PRINT #.FnrBi, "#ENDIF ' " & .Check
    CLOSE #.FnrBi
  END IF
END WITH
g_free(buffer)
END IF

FB_NAM.list(proto) '& RepData.list();
FB_TYP.list(proto) '& RepData.list();
IF LEN(proto) THEN ?NL "Nonexistent symbols" & proto

'& };