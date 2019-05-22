/'* \file girtobac.bas
\brief Main source code file

This file contains the main source code. Compile this file with the
FreeBasic compiler `fbc` to create an executable \Proj tool.

The main content of this file are different parsers used by the
[GMarkupParser](https://developer.gnome.org/glib/stable/glib-Simple-XML-Subset-Parser.html).
Each parser is declared as a UDT containing five function adresses.
Only two are used here (*start_element* and *end_element*). For better
readability macros are used for the relapsing part of the function
declarations and the UDT declarations of each parser. Additionally,
labels are placed in front of each parser to get a mark in the IDE
labels list for easy jumping between the parser definitions. (The
labels aren't used in the source code.)

'/


/'

GLib provides a simple XML parser, that is here used to read the
context of the input files (*.gir) and the control files (*.GirToBac).

'/

#INCLUDE ONCE "../Gir/GLib-2.0.bi"
#INCLUDE ONCE "girtobac_text.bi"

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
TYPE Context
  AS STRING _
    FunNam _    '*< The FB name of a SUB/FUNCTION/callback
  , FunTyp _    '*< The type of a FUNCTION/callback ("" = SUB)
  , FunDll _    '*< The function name in the dll
  , FieldNam _  '*< The FB name of a field
  , FieldVal _  '*< The vale of a field
  , OopDll _    '*< The FB symbol for OOP ???
  , Check _     '*< The symbol to check for (skip binding if prersent)
  , NamSpace _  '*< The namespace of the library
  , UserCode _  '*< The user code to prepend the header
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
  , Type_flg _  '*< The tye of a variable (see #TypeFlags)
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


/'* \brief Macro to read data from parser context

This is an essential macro to find and read data in the parser #Context
data.

\since 0.4.2
'/
#DEFINE fetch(_T_) find_value(_T_, AttNams, AttVals) 

/'* \brief Macro to start a parser

Each parser uses the same parameter list for the start function.
This macro generates the code for such a procedure (SUB) and opens a
WITH block to support access to the #Context data.

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
   WITH PEEK(Context, UserData)
#ENDMACRO

/'* \brief Macro to complete a start parser and open an end parser

Each parser uses the same code for ending a start function and the
same parameter list for the end function. This macro generates the
code to finish the start procedure (END WITH/END SUB), open an end
procedure (SUB with constant parameter list) and opens a WITH block
to support access to the #Context data.

It's designed to be used after the _START_PARSER() macro and in combination
with the _NEW_PARSER() macro.

'/
#MACRO _END_PARSER(_N_)
  CASE ELSE
    'PRINT #.FnrBi, NL "  ' " & __FB_FUNCTION__ & " Skipping " & *element_name _
        ' & " """ & *fetch("name") & """";
    g_markup_parse_context_push(ctx, @Skip_parser, UserData) '& skip_parser();
  END SELECT
  END WITH
 END SUB
 SUB end_##_N_ CDECL( _
  BYVAL ctx AS GMarkupParseContext PTR, _
  BYVAL element_name AS CONST gchar PTR, _
  BYVAL UserData AS gpointer, _
  BYVAL error_ AS GError PTR PTR)
   WITH PEEK(Context, UserData)
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


'* This \GMP parser does nothing, used for skipping unused XML-tags
'& SUB_CDECL skip_parser(){}; /*
STATIC SHARED AS GMarkupParser skip_parser = TYPE(NULL, NULL, NULL, NULL, NULL)
'& */

#INCLUDE ONCE "girtobac_RepData.bas"

START_TYPE:
/'* \brief Generate code to start parsing a type tag

This macro is used to start the type parser. The snipped is used
several times, so this macro makes it unique (single source).

'/
#MACRO _START_TYPE()
  .Type_flg = 0
  g_markup_parse_context_push(ctx, @type_parser, UserData)
#ENDMACRO

/'* \brief Generate code to end parsing a type tag

This macro is used to end the type parser. The snipped is used
several times, so this macro makes it unique (single source).

'/
#MACRO _END_TYPE()
  g_markup_parse_context_pop(ctx)
#ENDMACRO


START_FUNC:
/'* \brief Generate code to start parsing a function tag

This macro is used to start the function parser. The snipped is used
several times, so this macro makes it unique (single source).

'/
#MACRO _START_FUNC() 
  .ParaCnt = 0 : .ParaStr = "(" : .FunTyp = ""
  .GErrr = fetch("throws")
  IF OOP THEN
    .FunNam = *FB_NAM.rep(fetch("name"))
    .FunDll = *fetch("c:identifier")
  ELSE
    .FunNam = *FB_NAM.rep(fetch("c:identifier"))
  END IF
  g_markup_parse_context_push(ctx, @func_parser, UserData)
#ENDMACRO

/'* \brief Generate code to end parsing a function tag

This macro is used to end the function parser. The snipped is used
several times, so this macro makes it unique (single source).

'/
#MACRO _END_FUNC()
  IF .GErrr THEN
    IF .ParaCnt THEN .ParaStr &= ", "
    .ParaStr &= "BYVAL AS GError PTR PTR" : .ParaCnt += 1
  END IF
  g_markup_parse_context_pop(ctx)
  .Type_flg = IIF(LEN(.FunTyp), TYPE_FUN, TYPE_SUB)
  .ParaStr &= ")"
#ENDMACRO


/'* \brief Generate an FB type
\param Ud The data in the #Context
\returns An FB type string

Type declarations may need adaptions. We have to follow the rules
defined in the `*.GirToBac` file to change some types. Also they may
contain '*' characters to be translated to 'PTR' strings for FB.

In seldom cases a `*.gir` declaration doesn't contain an entry for a C
type. Then we try repairing it by using the type used in the namespace
instead.

'/
FUNCTION fb_type(BYVAL Ud AS ANY PTR) AS STRING
  WITH PEEK(Context, Ud) '& Context Ud;
    IF 0 = LEN(.TypC) THEN '               try to fix it (may be faulty)
      SELECT CASE .Typ
      CASE "utf8", "filename" : RETURN "/'!'/ gchar PTR"
      CASE ELSE
        IF 0 = LEN(.Typ) THEN RETURN "/'!'/ any PTR"
        .TypC = .Typ
      END SELECT
    ELSE
      IF LEFT(.TypC, 9) = "volatile " THEN .TypC = MID(.TypC, 10)
      IF RIGHT(.TypC, 4) = "char" THEN .TypC &= " /'?'/"
    END IF

    VAR ptrs = 0, i = LEN(.TypC) - 1
    FOR i = i TO 0 STEP -1
      IF .TypC[i] = ASC("*") THEN ptrs += 1 ELSE EXIT FOR
      .TypC[i] = 0
    NEXT

    .Typ = *IIF(OOP, FB_TYP.rep(.Typ) _
                   , FB_TYP.rep(.TypC))

    FOR p AS INTEGER = 1 TO ptrs
      .Typ &= " PTR"
    NEXT : RETURN .Typ
  END WITH
END FUNCTION

'&/* Doxygen sghouldn't see this forward declarations (necessary for circular references)
DECLARE SUB start_TYPE CDECL( _
  BYVAL AS GMarkupParseContext PTR, _
  BYVAL AS CONST gchar PTR, _
  BYVAL AS CONST gchar PTR PTR, _
  BYVAL AS CONST gchar PTR PTR, _
  BYVAL AS gpointer, _
  BYVAL AS GError PTR PTR)

DECLARE SUB end_TYPE CDECL( _
  BYVAL AS GMarkupParseContext PTR, _
  BYVAL AS CONST gchar PTR, _
  BYVAL AS gpointer, _
  BYVAL AS GError PTR PTR)
'&*/

'* The \GMP for the type tags (necessary for circular references)
STATIC SHARED AS GMarkupParser _
  type_parser = TYPE(@start_TYPE, @end_TYPE, NULL, NULL, NULL)


'* The \GMP for the parameter lists
'& SUB_CDECL para_parser(){
'& type_parser(); fb_type(); skip_parser();
Para_parser:
_START_PARSER(para)

  SELECT CASE *element_name
  CASE "parameter", "instance-parameter"
    'if OOP then
      'var d = fetch("direction")
      '.ParaBy = iif(*d <> "in", 1, 0)
    'end if
    _START_TYPE()

_END_PARSER(para)

  SELECT CASE *element_name
  CASE "parameter", "instance-parameter"
    _END_TYPE()
    IF .ParaCnt THEN .ParaStr &= ", "
    IF .Type_flg = TYPE_VARARG THEN
      .ParaStr &= "..."
    ELSE
      IF .Type_flg = TYPE_ARRAY ANDALSO LEN(.ArrayTyp) THEN .TypC = .ArrayTyp
      IF OOP THEN
        'if .ParaBy _
          'then .ParaStr &= "BYREF" : .ParaBy = 0 _
          'else .ParaStr &= "BYVAL"
        '.ParaStr &= " P" & .ParaCnt & " AS " & fb_type(UserData)
        .ParaStr &= "BYVAL P" & .ParaCnt & " AS " & fb_type(UserData)
      ELSE
        .ParaStr &= "BYVAL AS " & fb_type(UserData) 
      END IF
    END IF : .Type_flg = 0
    .ParaCnt += 1

_NEW_PARSER(para)
'& };

'* The \GMP for functions (tags function, method, constructor, callback)
'& SUB_CDECL func_parser(){
'& type_parser(); para_parser(); fb_type(); skip_parser();
Func_parser:
_START_PARSER(func)

  SELECT CASE *element_name
  CASE "return-value"
    _START_TYPE()
  CASE "parameters"
    g_markup_parse_context_push(ctx, @para_parser, UserData) 

_END_PARSER(func)

  SELECT CASE *element_name
  CASE "return-value"
    IF .Type_flg = TYPE_ARRAY ANDALSO LEN(.ArrayTyp) THEN .TypC = .ArrayTyp
    IF .Typ <> "none" THEN .FunTyp = fb_type(UserData)
    _END_TYPE()
  CASE "parameters"
    g_markup_parse_context_pop(ctx)

_NEW_PARSER(func)
'& };

'* The \GMP for the types
'& SUB_CDECL type_parser(){
'& skip_parser(); type_parser(); func_parser(); FB_NAM.rep();
Type_parser:
_START_PARSER(type)

  SELECT CASE *element_name
  CASE "type"
    IF .Type_flg = TYPE_LIST THEN EXIT SELECT ' skip sub types
     .Typ = *fetch("name")
     VAR t = fetch("c:type")
     .TypC = *IIF(t, t, @"")
     SELECT CASE .Typ
     CASE "GLib.List", "GLib.SList", "GLib.HashTable", "GLib.Array"
       .Type_flg = TYPE_LIST
       g_markup_parse_context_push(ctx, @skip_parser, UserData) 
     CASE ELSE
       .Type_flg = TYPE_VAR
     END SELECT
  CASE "callback"
    _START_FUNC() 
  CASE "array"
    VAR n = fetch("name")   
    VAR t = fetch("c:type") 
    IF n THEN
      .Typ = *n
      .TypC = *IIF(t, t, @"")
      .ArrayTyp = *IIF(t, t, @"")
      .ArrayLen = -1
      g_markup_parse_context_push(ctx, @skip_parser, UserData) 
    ELSE
      n = fetch("length")
      IF 0 = n THEN n = fetch("fixed-size")
      .ArrayTyp = *IIF(t, t, @"")
      .ArrayLen = IIF(n, CUINT(*n), 0)
      .Type_flg = 0
      g_markup_parse_context_push(ctx, @type_parser, UserData) 
    END IF
  CASE "varargs"

_END_PARSER(type)

  SELECT CASE *element_name
  CASE "type"
    IF .Type_flg = TYPE_LIST THEN g_markup_parse_context_pop(ctx)
  CASE "callback"
    _END_FUNC()
  CASE "array"
    .Type_flg = TYPE_ARRAY
    g_markup_parse_context_pop(ctx)
  CASE "varargs"
    .Type_flg = TYPE_VARARG
  CASE ELSE
    g_markup_parse_context_pop(ctx)
  END SELECT

END WITH : END SUB ' no _P2() macro since we need the Type_parser declaration above
'& };

'* The \GMP for the interfaces, records and classes (OOP style -> ToDo)
'& SUB_CDECL class_parser(){
'& type_parser(); func_parser(); fb_type(); skip_parser(); FB_NAM.rep();
Class_parser:
_START_PARSER(class)

  SELECT CASE *element_name
  CASE "method", "function", "constructor"
    _START_FUNC()
  CASE "field"
    .FieldNam = *FB_NAM.rep(fetch("name"))
    VAR bits = fetch("bits") 
    .FieldBits = IIF(bits, CUINT(*bits), 0)
    _START_TYPE()
  CASE "property"
    .FunNam = *FB_NAM.rep(fetch("name"))
    VAR r = fetch("readable") 
    VAR w = fetch("writable") 
    .PropRW  = IIF(r ANDALSO *r = "0", 0, 1)
    .PropRW += IIF(w ANDALSO *w = "0", 0, 2)
    _START_TYPE()
  CASE "virtual-method"
    .ParaCnt = 0 : .ParaStr = "(" : .FunTyp = ""
    .FunNam = *FB_NAM.rep(fetch("name"))
    g_markup_parse_context_push(ctx, @func_parser, UserData) 
  CASE "implements"

_END_PARSER(class)

  SELECT CASE *element_name
  CASE "constructor"
    .BlockCnt += 1
    _END_FUNC()
    IF .Type_flg <> TYPE_FUN THEN .Raus(.Level) &= NL "'' ??? CTOR w/o type"

    .Raus(.Level) &= NL "  DECLARE CONSTRUCTOR " & .ParaStr
    VAR dll = "__" & .BlockCnt 
    .Raus(.Level) &= NL "  " & dll & " AS FUNCTION" & .ParaStr & " AS " & .FunTyp _
                  & " = DYLIBSYMBOL(" & .OopDll & ", """ & .FunDll & """)"

    .Raus(0) &= NL "CONSTRUCTOR " & .Nams(.Level) & .ParaStr
    IF .ParaCnt THEN .ParaStr = "P1" ELSE .ParaStr = ""
    FOR i AS INTEGER = 2 TO .ParaCnt
      .ParaStr &= ", P" & i
    NEXT
    .Raus(0) &= NL "  __Obj = " & dll & "(" & .ParaStr & ")"
    .Raus(0) &= NL "END CONSTRUCTOR"
    .Type_flg = 0
  CASE "field"
    .BlockCnt += 1
    SELECT CASE AS CONST .Type_flg
    CASE TYPE_SUB
      .Raus(.Level) &= NL "  " & .FieldNam & " AS SUB CDECL" & .ParaStr
    CASE TYPE_FUN
      .Raus(.Level) &= NL "  " & .FieldNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp
    CASE ELSE
      .Raus(.Level) &= NL "  AS " & fb_type(UserData) & " " & .FieldNam 
      IF .FieldBits THEN .Raus(.Level) &= " : " & .FieldBits
    END SELECT : .Type_flg = 0
    _END_TYPE()
  CASE "virtual-method"
    .BlockCnt += 1
    _END_FUNC()
    SELECT CASE AS CONST .Type_flg
    CASE TYPE_SUB : .Raus(.Level) &= NL "  " & .FunNam & " AS SUB CDECL" & .ParaStr
    CASE ELSE : .Raus(.Level) &= NL "  " & .FunNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp
    END SELECT
  CASE "method", "function"
    .BlockCnt += 1
    _END_FUNC()
    VAR dll = "__" & .BlockCnt

    VAR p = INSTR(.ParaStr, ", ") 
    VAR par = "("                 
    IF p THEN par &= MID(.ParaStr, p + 2) ELSE par &= ")"

    VAR parcall =  UCASE(.NamSpace & "_" & .Nams(.Level)) & "(@THIS)" 
    FOR i AS INTEGER = 1 TO .ParaCnt - 1
      parcall &= ", P" & i
    NEXT

    SELECT CASE AS CONST .Type_flg
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
    END SELECT : .Type_flg = 0
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
    _END_TYPE()
  CASE "implements"

_NEW_PARSER(class)
'& };

'* The \GMP for the interfaces, records and classes (C-like style)
'& SUB_CDECL udt_parser(){
'& type_parser(); skip_parser(); fb_type(); func_parser(); FB_NAM.rep();
Udt_parser:
_START_PARSER(udt)

  SELECT CASE *element_name
  CASE "field"
    VAR n = fetch("name")
    VAR c = fetch("c:identifier")
    .FieldNam = *FB_NAM.rep(IIF(c, c, n))
    VAR bits = fetch("bits")
    .FieldBits = IIF(bits, CUINT(*bits), 0)
    _START_TYPE()
  CASE "virtual-method"
    g_markup_parse_context_push(ctx, @skip_parser, UserData) 
  CASE "constructor", "method", "function"
    _START_FUNC()
  CASE "property"

_END_PARSER(udt)

  SELECT CASE *element_name
  CASE "field"
    .BlockCnt += 1
    SELECT CASE AS CONST .Type_flg
    CASE TYPE_SUB
      .Raus(.Level) &= NL "  " & .FieldNam & " AS SUB CDECL" & .ParaStr
    CASE TYPE_FUN
      .Raus(.Level) &= NL "  " & .FieldNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp
    CASE ELSE
      .Raus(.Level) &= NL "  AS " & fb_type(UserData) & " " & .FieldNam 
      IF .Type_flg = TYPE_ARRAY ANDALSO .ArrayLen > 0 _
        THEN .Raus(.Level) &= "(" & .ArrayLen - 1 & ")"
      IF .FieldBits THEN .Raus(.Level) &= " : " & .FieldBits
    END SELECT : .Type_flg = 0
    _END_TYPE()
  CASE "virtual-method"
    g_markup_parse_context_pop(ctx)
  CASE "method", "function", "constructor"
    _END_FUNC()
    SELECT CASE AS CONST .Type_flg
    CASE TYPE_SUB
      .Raus(0) &= NL "DECLARE SUB " & .FunNam & .ParaStr
    CASE ELSE
      .Raus(0) &= NL "DECLARE FUNCTION " & .FunNam & .ParaStr & " AS " & .FunTyp
    END SELECT : .Type_flg = 0
  CASE "property" ' !!!

_NEW_PARSER(udt)
'& };


'* The \GMP for the union blocks
'& SUB_CDECL unio_parser(){
'& type_parser(); RepData.rep(); find_value(); class_parser(); udt_parser(); fb_type(); skip_parser(); FB_NAM.rep();
Unio_parser:
_START_PARSER(unio)

  SELECT CASE *element_name
  CASE "field"
    VAR n = fetch("name")         
    VAR c = fetch("c:identifier") 
    .FieldNam = *FB_NAM.rep(IIF(OOP, n, IIF(c, c, n)))
    VAR bits = fetch("bits")      
    .FieldBits = IIF(bits, CUINT(*bits), 0)
    _START_TYPE()
  CASE "record"
    .Level += 1 : IF .Level > UBOUND(.Raus) THEN ?"Raus maximum exeded!"
    IF .Level > .RausMax THEN .RausMax = .Level
    VAR nam = *FB_NAM.rep(fetch("name")) 
    .Nams(.Level) = "__G2B_" & .Nams(.Level - 1) & "_" & nam
    .Raus(.Level - 1) &= NL "  AS " & .Nams(.Level) & " " & nam
    .Raus(.Level) &= NL "TYPE " & .Nams(.Level)
    g_markup_parse_context_push(ctx, IIF(OOP, @class_parser, @udt_parser), UserData) 

_END_PARSER(unio)

  SELECT CASE *element_name
  CASE "field"
    .BlockCnt += 1
    SELECT CASE AS CONST .Type_flg
    CASE TYPE_SUB
      .Raus(.Level) &= NL "  " & .FieldNam & " AS SUB CDECL" & .ParaStr
    CASE TYPE_FUN
      .Raus(.Level) &= NL "  " & .FieldNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp
    CASE ELSE
      .Raus(.Level) &= NL "  AS " & fb_type(UserData) & " " & .FieldNam 
      IF .FieldBits THEN .Raus(.Level) &= " : " & .FieldBits
    END SELECT : .Type_flg = 0
    _END_TYPE()
  CASE "record"
    .BlockCnt += 1
    .Raus(.Level) &= NL "END TYPE"
    .Level -= 1
    g_markup_parse_context_pop(ctx)

_NEW_PARSER(unio)
'& };

'* The \GMP for the enum blocks
'& SUB_CDECL enum_parser(){
'& RepData.rep(); find_value(); skip_parser(); FB_NAM.rep();
Enum_parser:
_START_PARSER(enum)

  SELECT CASE *element_name
  CASE "member"
    VAR nam = *FB_NAM.rep(fetch(*IIF(OOP, @"name", @"c:identifier"))) 
    VAR value = fetch("value") 
    PRINT #.FnrBi, NL "  " & nam;
    IF value THEN PRINT #.FnrBi, " = " & CINT(*value);

_END_PARSER(enum)

  SELECT CASE *element_name
  CASE "member"

_NEW_PARSER(enum)
'& };

'* The \GMP for the first pass
'& SUB_CDECL pass1_parser(){
'& type_parser(); RepData.rep(); find_value(); enum_parser(); fb_type(); skip_parser(); FB_NAM.rep();
Pass1_parser:
_START_PARSER(pass1)

  SELECT CASE *element_name
  CASE "constant"
    VAR n = fetch(IIF(OOP, @"name", @"c:type")) 
    IF 0 = n THEN n = fetch("c:identifier")
    .FieldNam = *FB_NAM.rep(n)
    .FieldVal = *fetch("value")
    _START_TYPE()
  CASE "enumeration", "bitfield"
    VAR nam = *FB_NAM.rep(fetch(IIF(OOP, @"name", @"c:type"))) 

    IF LEN(nam) THEN PRINT #.FnrBi, NL "TYPE AS LONG " & nam;
    PRINT #.FnrBi, NL "ENUM";
    'PRINT #.FnrBi, NL "ENUM " & nam;

    IF OOP THEN PRINT #.FnrBi, " EXPLICIT";
    g_markup_parse_context_push(ctx, @enum_parser, UserData) 
  CASE "alias"
    VAR n = fetch(IIF(OOP, @"name", @"c:type")) 
    IF 0 = n THEN n = fetch("glib:type-name")
    IF n THEN .FieldNam = *FB_NAM.rep(n) ELSE .FieldNam = ""
    _START_TYPE() 
  CASE "class", "record", "interface"
    VAR n = fetch(IIF(OOP, @"name", @"c:type")) 
    IF 0 = n THEN n = fetch("glib:type-name")
    VAR nam = *FB_NAM.rep(n) 
    PRINT #.FnrBi, NL "TYPE AS _" & nam & " " & nam;
  CASE "include"
    PRINT #.FnrBi, NL "#INCLUDE ONCE """ & _
       *fetch("name") & "-" & _
       *fetch("version") & ".bi""";
  CASE "repository"
    PRINT #.FnrBi, NL "' Repository version " & *fetch("version");
  CASE "namespace"
    IF 0 = LEN(.NamSpace) THEN .NamSpace = *fetch("name")
    VAR dll = fetch("shared-library") + 3 
    IF dll > 3 ANDALSO 0 = LEN(.NamDLL) THEN .NamDll = LEFT(*dll, INSTR(*dll, ".so") - 1)

    IF 0 = OOP THEN EXIT SELECT
    IF dll > 3 THEN
      .OopDll = "__G2B_" & *fetch("name")
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
    _END_TYPE()
  CASE "enumeration", "bitfield"
    PRINT #.FnrBi, NL "END ENUM";
    g_markup_parse_context_pop(ctx)
  CASE "alias"
    IF LEN(.FieldNam) _
      THEN PRINT #.FnrBi, NL "TYPE AS " & fb_type(UserData) & " " & .FieldNam; _
      ELSE PRINT #.FnrBi, NL "TYPE AS ANY " & fb_type(UserData); 
    _END_TYPE()
  CASE "class", "record", "interface"
  CASE "include", "repository", "namespace"

_NEW_PARSER(pass1)
'& };


'* The \GMP for the second pass
'& SUB_CDECL passX_parser(){
'& skip_parser(); class_parser(); udt_parser(); unio_parser(); RepData::rep(find_value()); func_parser(); FB_NAM.rep();
PassX_parser:
_START_PARSER(passX)
  SELECT CASE *element_name
  CASE "repository", "namespace" : EXIT SUB
  END SELECT

  VAR n = fetch(*IIF(OOP, @"name", @"c:type"))
  IF 0 = n THEN
    n = fetch("glib:type-name")
    IF 0 = n THEN n = fetch("name")
  END IF
  IF FIRST.A = -1 THEN '                                    we're in P_3
    IF FIRST.find(n) THEN '                       element allready done?
      .SkipElem = 1
      g_markup_parse_context_push(ctx, @skip_parser, UserData)
      EXIT SUB
    END IF
  ELSE '                                                    we're in P_X
    IF *n <> .NextElm THEN '                             not our element
      .SkipElem = 1
      g_markup_parse_context_push(ctx, @skip_parser, UserData) 
      EXIT SUB
    END IF
    .NextElm = FIRST.nxt()
  END IF

  SELECT CASE *element_name
  CASE "enumeration"
    IF 0 = OOP THEN '                       generate type macro/function
      VAR g = fetch("glib:get-type") : IF 0 = g THEN EXIT SELECT
      VAR p1 = INSTR(*g, "_"), p2 = INSTR(p1 + 1, *g, "_get_type") 
      VAR n = LEFT(*g, p1 - 1) & "_TYPE_" & MID(*g, p1 + 1, p2 - p1 - 1)
     .Raus(0) &= NL "DECLARE FUNCTION " & *g & "() AS GType"
     .Raus(0) &= NL "#DEFINE " & UCASE(n) & " (" & *g & "())"
    END IF  
  CASE "interface", "class"
    IF 0 = OOP THEN '                              generate class macros
      VAR g = fetch("glib:get-type") 
      IF g ANDALSO *g <> "intern" THEN
        VAR s = fetch("glib:type-struct") _
         , t1 = UCASE(.NamSpace) & "_" _                 
          , p = INSTR(*g, "_") _                         
         , t2 = UCASE(MID(*g, p + 1, LEN(*g) - p - 9)) _ 
         , t3 = t1 & "TYPE_" & t2 _                      
         , t4 = t3 & ", " & *g _                         
         , t5 = .NamSpace & *s                           

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
    g_markup_parse_context_push(ctx, IIF(OOP, @class_parser, @udt_parser), UserData) 
  CASE "record"
    .BlockCnt = 0
    .Nams(.Level) = *FB_NAM.rep(n)
    .Raus(.Level) &= NL "TYPE _" & .Nams(.Level)
    g_markup_parse_context_push(ctx, IIF(OOP, @class_parser, @udt_parser), UserData) 
  CASE "union"
    .Nams(.Level) = *FB_NAM.rep(n)
    .Raus(.Level) = NL "UNION " & .Nams(.Level)
    g_markup_parse_context_push(ctx, @unio_parser, UserData) 
  CASE "callback"
    .ParaCnt = 0 : .ParaStr = "(" : .FunTyp = ""
    .FunNam = *FB_NAM.rep(fetch(*IIF(OOP, @"name", @"c:type"))) 
    g_markup_parse_context_push(ctx, @func_parser, UserData) 

_END_PARSER(passX)

  IF .SkipElem THEN
    .SkipElem = 0
    g_markup_parse_context_pop(ctx)
    EXIT SUB
  END IF

  SELECT CASE *element_name
  CASE "enumeration"
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
    SELECT CASE AS CONST .Type_flg
    CASE TYPE_SUB : PRINT #.FnrBi, NL "TYPE " & .FunNam & " AS SUB CDECL" & .ParaStr;
    CASE ELSE : PRINT #.FnrBi, NL "TYPE " & .FunNam & " AS FUNCTION CDECL" & .ParaStr & " AS " & .FunTyp;
    END SELECT
  CASE "repository", "namespace"

_NEW_PARSER(passX)
'& };

'* The \GMP for last pass
'& SUB_CDECL pass4_parser(){
'& find_value(); func_parser(); skip_parser(); FB_NAM.rep();
Pass4_parser:
_START_PARSER(pass4)

  SELECT CASE *element_name
  CASE "function"
    .FuncSkip = fetch("moved-to")
    IF .FuncSkip THEN EXIT SELECT
    _START_FUNC() 
  CASE "repository", "namespace"

_END_PARSER(pass4)

  SELECT CASE *element_name
  CASE "function"
    IF .FuncSkip THEN EXIT SELECT
    _END_FUNC()
    SELECT CASE AS CONST .Type_flg
    CASE TYPE_SUB :  PRINT #.FnrBi, NL "DECLARE SUB " & .FunNam & .ParaStr;
    CASE ELSE :  PRINT #.FnrBi, NL "DECLARE FUNCTION " & .FunNam & .ParaStr & " AS " & .FunTyp;
    END SELECT : .Type_flg = 0
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
'& g2b_parser(); pass1_parser(); passX_parser(); pass4_parser(); RepData.list(); FB_NAM.list(); FB_TYP.list(proto;)

IF COMMAND(1) = "-v" ORELSE COMMAND(1) = "--version" THEN ?V_TEXT : END
IF __FB_ARGC__ <> 2  ORELSE _
   COMMAND(1) = "-h" ORELSE COMMAND(1) = "--help"    THEN ?H_TEXT : END

VAR filename = COMMAND(1) _  ' *< input file name
  , basename = MID(filename, INSTRREV(filename, ANY "\/") + 1) ' *< name without path

IF g_str_has_suffix(filename, ".gir") _
  THEN basename = LEFT(basename, LEN(basename) - 4) _
  ELSE filename &= ".gir"

VAR ofile = basename & ".bi" _       ' *< name of output file
  , ifile = basename & ".GirToBac" _ ' *< name of configuration file
  , proto = "" ' *< protocol messages

DIM AS GError PTR errr = NULL ' *< location for GLib errors
DIM AS Context UDat           ' *< data to exchange between parsers

DIM AS gchar PTR buffer       ' *< buffer for input data
DIM AS gsize length           ' *< length of input data

IF 0 = g_file_get_contents(filename, @buffer, @length, @errr) THEN 
  ?"Cannot open " & filename;
ELSE
  ?"loading " & filename;

WITH UDat
  .FnrBi = FREEFILE
  'if open cons(for output as .FnrBi) then
    '?NL "Cannot open console for output!";
  IF OPEN(ofile FOR OUTPUT AS .FnrBi) THEN
    ?NL "Cannot open file for output!";
  ELSE
    PRINT #.FnrBi, MSG_HEADERS & filename;

    DIM AS gchar PTR buff2 ' *< buffer for control file
    DIM AS gsize length2   ' *< length of control file
    IF g_file_get_contents(ifile, @buff2, @length2, @errr) THEN
      ?NL "loading " & ifile;
      VAR ctx = g_markup_parse_context_new(@G2b_parser, 0, @UDat, NULL)
      IF g_markup_parse_context_parse(ctx, buff2, length2, @errr) THEN _ 
        IF 0 = g_markup_parse_context_end_parse(ctx, @errr) THEN _
          g_print(!"Cannot parse %s (invalid content)\n", ifile, NULL)
      g_markup_parse_context_free(ctx)
      g_free(buff2)
    END IF
    ?NL "generating " & ofile & " ... ";
    IF LEN(.Check) THEN PRINT #.FnrBi, NL "#IFNDEF " & .Check;
    PRINT #.FnrBi, NL "#INCLUDE ONCE ""_GirToBac-0.0.bi""";
    IF LEN(.UserCode) THEN PRINT #.FnrBi, NL .UserCode;

    PARSE(1) '//                                     #DEFINE, ENUM, TYPE

    IF OOP ORELSE 0 = LEN(.NamDll) THEN  
      PRINT #.FnrBi, NL "EXTERN ""C""";
    ELSE
      PRINT #.FnrBi, NL "EXTERN ""C"" LIB """ & .NamDll & """";
    END IF

    PRINT #.FnrBi, NL "' P_X" '    UNIONs, CALLBACKs and TYPEs (ordered)
    .NextElm = FIRST.nxt()
    WHILE FIRST.A
      VAR xx = FIRST.A ' *< stack position
      PARSE(X) 
      IF xx <> FIRST.A THEN CONTINUE WHILE
      proto &= NL !"\t" & .NextElm & " <first>"
      .NextElm = FIRST.nxt()
    WEND
    FIRST.A = -1

    PRINT #.FnrBi, NL "' P_3" '       UNIONs, CALLBACKs and TYPEs (rest)
    PARSE(X)

    PRINT #.FnrBi, NL "' P_4" '          SUBs and FUNCTIONs (END EXTERN)
    PARSE(4)

    PRINT #.FnrBi,
    IF LEN(.Check) THEN PRINT #.FnrBi, "#ENDIF ' " & .Check
    CLOSE #.FnrBi
    ?"done";
  END IF
END WITH
  g_free(buffer)
END IF

FB_NAM.list(proto)
FB_TYP.list(proto)
IF LEN(proto) THEN ?NL "Symbols in " & ifile & ", but not in " & filename & !":\n" & proto;
?

'& };
