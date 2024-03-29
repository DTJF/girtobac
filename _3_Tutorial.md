Tutorial  {#PagTutorial}
========
\tableofcontents

# Classic vs. girtobac headers  {#SecDifferences}

There're small differences between the headers generated by \Proj
and to the headers shipped with FreeBasic (effective up to version
fbc-0.90). GI (in its `*.gir` files) only supports constant
macros (a string or numerical constant behind the #`define` keyword).
But the files don't contain any code-generating macros nor inline
functions.

So the \Proj generated FB headers just contain macros for
constants. In addition, some standard macros for the GObject-classes
(for C-styled headers) are generated by \Proj.

The lack of code creating macros seems to be no big deal, since the
GI-libraries are used without these macros in the above mentioned
high-level programming languages for years. (In the translation
process the missing macros are an advantage, since un-needed macros
and pre-processors need no manual removing.) But sometimes it may be
better to have some further macro support. In this cases it's
possible to extend the \Proj header with a manual translated
header-subset containing the missing macros.

\Proj currently generates headers in a classic (C-like) style.
Additionally it's prepared to generate OOP wrappers for GI libraries
as well. Most of the high-level languages are OOP languages and GI
is used for translating the GObject C code into real classes. The
`*.gir` files contain all informations to wrap a C library
in real FB objects with CONSTRUCTORs , SUBs, FUNCTIONs, PROPERTYs,
... This can help to make FB code more readable and to better
support memory management.

Unfortunatelly the inheritance support in FB is currently (fbc-0.90.1)
limited (ie. FB cannot extend a type by several interfaces). And
some other features are not well prepared for that style of library
bindings (ie. EXPLICIT statement in ENUM blocks). So the development
of the OOP features in \Proj is stopped ATM and we have to wait
for advanced features in the FB compiler.


# How to get started?  {#SecStart}

As an example the file `example/GLib-2.0.gir` is shipped with
this package. You can make your first translation using this file
and compare the output against the file `Gir/GLib-2.0.bi.org`
to check the \Proj executable and get familar with how to call
\Proj.

Follow these steps:

-# Jump to folder `Gir` and call \Proj (prepending the path if
   necessary) to create a new header from the example file
   ~~~
cd Gir
girtobac ../example/GLib-2.0
~~~
   and you'll see output like
   ~~~
loading ../example/GLib-2.0.gir
loading GLib-2.0.GirToBac
generating GLib-2.0.bi ... done
~~~

-# Compare the newly created file `GLib-2.0.bi` against the
   original one (also shipped in this package) by executing
   ~~~
diff GLib-2.0.bi GLib-2.0.bi.org
~~~
   This should output some differences, like
   ~~~
2,4c2,4
< '                       ### GirToBac ###
< ' LGPLv2.1 (C) 2013-2022 by Thomas[ dot }Freiherr[ at ]gmx[ dot }net
< ' Auto-translated from file ../example/GLib-2.0.gir
---
> '                       ### girtobac ###
> ' LGPLv2.1 (C) 2013-2022 by Thomas[ dot }Freiherr[ at ]gmx[ dot }net
> ' Auto-translated from file /usr/share/gir-1.0/GLib-2.0.gir
2526d2525
< DECLARE FUNCTION g_iconv(BYVAL AS GIConv PTR /'GIConv'/, BYVAL AS gchar PTR PTR, BYVAL AS gsize PTR, BYVAL AS gchar PTR PTR, BYVAL AS gsize PTR) AS gsize
~~~
   The first difference (three lines) is about comment lines in the top
   region of the file, they've no effect. The second difference is a
   `DECLARE` line in the left hand file, which is not present in the
   original file. The functions `g_iconv` is declared twice in the
   `*.gir` file. It's declared as a method in the record `IConv` at line
   8328 and also as a function at line 37091. By design, \Proj does
   neither find nor solve such conflicts. It's up to the user to
   validate the output by compiling with the FreeBASIC compiler and fix
   the error messages.

-# In order to validate the header, and since fbc doesn't compile
   header files directly, we first create a new file called `test.bas`
   which includes the header and then compile this file by executing
   ~~~
echo '#INCLUDE ONCE "GLib-2.0.bi"' > test.bas
fbc -e -w all test.bas
~~~
   and we get output like
   ~~~
GLib-2.0.bi(2526) error 4: Duplicated definition in 'DECLARE FUNCTION g_iconv(BYVAL AS GIConv PTR /'GIConv'/, BYVAL AS gchar PTR PTR, BYVAL AS gsize PTR, BYVAL AS gchar PTR PTR, BYVAL AS gsize PTR) AS gsize'
~~~
   As expected, the compiler finds the duplicated definition. We've to
   load the file GLib-2.0.bi in to an editor, delete line 2526 and save
   the file. Now the header should be ready to use (test it by
   compiling file test.bas again).
   \note The shipped configuration file GLib-2.0.GirToBac was used in
         the translation process (in first step). That's why just a
         small amount of manual action was necessary. When you start
         from scratch, you'll see much more error messages from the
         compiler and you've to create and fill the configuration file
         line by line to fix them.

-# In order to use this (or further) \Proj created header[s] in your FB
   source, copy (or move) the folder `Gir` and its files in to your
   `freebasic/include` path. Then use
   ~~~
#INCLUDE ONCE "Gir/GLib-2.0.bi"
~~~
   in your code.


# How to get and translate `*.gir` files?  {#SecGirs}

A set of GLib headers was created during the development of \Proj for
testing purposes. First check if set contains your header. You can
download at
http://www.freebasic-portal.de/downloads/bibliotheken/gtk-3-header-dateien-fuer-freebasic-191.html.
If your library isn't included or isn't up-to-date, you need do make
your customized translation.

`*.gir` files get generated when a GI-library gets compiled. So
you can either get them by downloading the source package of a library
and compile it (and all its dependencies, using option
`--enable-introspection=yes`). Thats's the laborious way to go
on non-LINUX systems.

On LINUX systems the `*.gir` files are provided in the package
management system. They're included in the `*-dev` packages (hopefully
this will change in near future). Ie. `Gtk-3.0.gir` is in package
`libgtk-3-dev` on Debian based systems. So you'll get the `*.gir` files
by default, since you have to install this package in order to compile
your source.

Create your header by following these steps:

## Preparation

-# Create your output folder (usually `.../freebasic/include/Gir`)

-# Copy basis header `Gir/_GirToBac-0.0.bi` in your output folder
   (or install the header set mentioned above).

-# Download and extract the `*.gir` file
   (ie. `/usr/lib/girepository-1.0/Xyz-1.0.gir`).

## Translation

-# Jump to your output folder and call \Proj for translation
   ~~~{.txt}
/PATH/TO/girtobac /usr/lib/girepository-1.0/Xyz-1.0
~~~

-# Create a test file (ie. `test.bas` in any folder) containing the line
   ~~~{.txt}
#INCLUDE ONCE "Gir/Xyz-1.0.bi"
~~~
   and compile it
   ~~~{.txt}
fbc -wall test.bas
~~~

-# In case of no compiler errors you're done (the linker may report
   that `-lXyz-1.0` is missing because you didn't install the
   binaries yet).

-# Otherwise see section \ref ErrorFix. Create a control file (ie.
   `Xyz-1.0.GirToBac`) in the output folder and add entries.
   Recompile the `test.bas` code until *fbc* doesn't complain
   any error.


# How to fix Problems?  {#ErrorFix}

Here's a brief summary of the most common problems when compiling the
test code and how to fix them. A control file in your output folder
(`.../freebasic/include/Gir`) is used to add some translation
rules. It's named after the inputs base name with suffix `.GirToBac`
(ie. `Xyz-1.0.GirToBac` for a `Xyz-1.0.gir` file). The rules are
specified in XML syntax (see section \ref SecControlFile). Depending on
the error messages thrown by the fbc when compiling the test file,
different entries should be done:

- <B>Duplicated definition ...</B> There's a naming conflict. This
  happens when
  - a function is declared twice in the `*.gir` file,
  - fbc interpretes a name non-case-sensitve, or
  - a FB keyword is used as a C symbol name.

  Either delete the doubled `DECLARE` line or change
  the FB name to make it unique (usually by adding an underscore
  character, but also consider to use an ALIAS statement). Example:
  ~~~{.xml}
<name search='window'  add='_' />
~~~

- <B>Expected identifier ...</B> There's a conflict between the C
  type name and an FB keyword. Or a C type is used that wasn't
  priviously specified. Rename the type to make it unique or known.
  Example (`Soup-2.4.GirToBac`):
  ~~~{.xml}
<type search='int' replace='gint' />
~~~

- <B>Illegal specification ...</B> or <B>Incomplete type ...</B> The
  symbol named in the error message isn't specified before this
  position (code line). If it's a standard C-type like `float`, the
  type should get replaced by a similar GLib (or FB) type. This needs a
  line like
  ~~~{.xml}
<type search='float' replace="single /'replaced float'/"/>
~~~
  Otherwise, in case of a library specific type, the declaration of
  this UDT needs to get moved forward. Example (`Gio-2.0.GirToBac`):
  ~~~{.xml}
<first search="GInputStream"/>
~~~

- <B>File not found, "Xyz-0.0.bi"</B> The library depends on an other
  library which isn't translated jet. Download the `Xyz-0.0.gir`
  file and translate it first. Then continue with the original
  translation.

You may face further problems or error messages when compiling the
test file. Sorry, it's not possible to cover all of them here. If you
cannot handle an issue don't hasitate to ask for help at
http://www.freebasic.net/forum in the *Libraries* subforum.


# The control file (*.GirToBac)  {#SecControlFile}

Similar to the `*.gir` file, its control file is also XML
formated and is named by the same base-name and with suffix
`*.GirToBac`. Each control file contains adaption rules for one
`*.gir` file, \Proj searches for the control file in the current
folder (where the output gets written). Ie. (see also \ref SecStart)
when the \Proj tool gets called by

~~~{.txt}
girtobac ../example/GLib-2.0
~~~

it loads the file `GLib-2.0.gir` form directory
`../example` and file `GLib-2.0.GirToBac` (if present in
the current folder). If the later isn't present, translation gets done
without any rules. The generated output gets written to the file
`GLib-2.0.bi` in the current folder (where \Proj is called
from).

As mentioned in the previous paragraph, adaption rules may get
specified for

- a mismatch between the library name and its internal namespace
- naming conflicts in variable names (because C naming is
  case-sensitive and FB's isn't)
- typing conflicts in type names (GI is work in process, some C
  types are untranslated in the `*.gir` files, currently)
- reordering of declarations (the `*.gir` files contain the
  symbol declarations in alphabetical order. Since fbc is a single
  pass compiler it needs to re-order some of them in logical order.)
- a symbol check to switch off a dummy header when the complete header
  is in use.
- a missmatch between the library name and the name declared in the
  `*.gir` file.

Therefor the parser for the configuration file understands several
XML-tags and -- depending on the tag type -- the attributes `name`,
`add` and `replace`. Here's a list of the tags:

- <b>binary</b> to override the name of the binary. This is useful if
  the `.gir` file doesn't contain the right library name.
  Example (`cairo-1.0.GirToBac`):
  ~~~{.xml}
<binary name='cairo' />
~~~
- <b>check</b> to enclose the complete header source by
  ~~~{.txt}
#IFNDEF symbol
...
#ENDIF
~~~
  This is useful for a dummy binding that contains just a few type
  declarations. To avoid interferences with the complete binding, the
  dummy source can get switched of.
  Example (`cairo-1.0.GirToBac`):
  ~~~{.xml}
<check name='CAIRO_H' />
~~~

- <b>code</b> to prepend the header by user defined text (source code).
  The code is prepended before including other dependency headers, but
  after the standard header `_GirToBac-0.0.bi`. Ie. this can include
  further headers or provide additional declarations. Example
  (`Soup-2.4.GirToBac`):
  ~~~{.xml}
<code>
#IFNDEF sockaddr
 TYPE sa_family_t AS USHORT ' from bits/sockaddr.bi
 TYPE sockaddr
  sa_family AS sa_family_t
  sa_data(0 TO 14-1) AS UBYTE
 END TYPE
#ENDIF
</code>
~~~

- <b>first</b> to re-order the symbol declaration (`search` contains a
  single word, no attribute). It declares this symbol before the
  others, in the specified order (re-ordering the `first` tags may help
  to solve circular references). It's working on
  * `ENUM`
  * `UNION`
  * `TYPE` (UDT)
  * `TYPE SUB`, `TYPE FUNCTION` (callback prototype, since 0.6)

  Example (`Gtk-3.0.GirToBac`):
  ~~~{.xml}
<first search='GtkWidget' />
<first search='GtkWidgetClass' />
~~~

- <b>name</b> to adapt a symbol name (`search` contains a single word,
  further attributes `add` or `replace`).
  Example (`Gtk-3.0.GirToBac`):
  ~~~{.xml}
<name search='gtk_stock_add' add='_ ALIAS "gtk_stock_add"' />
~~~
  or
  ~~~{.xml}
<name search='GTK_RC_STYLE' replace='_GTK_RC_STYLE' />
~~~
- <b>pack</b> to adapt the library namespace (attribute `name`
  contains the new name in camel case letters).
  Example (`GLib-2.0.GirToBac`):
  ~~~{.xml}
<pack name='G' />
~~~
  or (`GooCanvas-2.0.GirToBac`):
  ~~~{.xml}
<pack name='Goo' />
~~~

- <b>type</b> to adapt types (`search` attribute contains any text,
  further attributes `add` or `replace`). Example
  (`GLib-2.0.GirToBac`):
  ~~~{.xml}
<type search='GIconv' add=' PTR' />
~~~
  or
  ~~~{.xml}
 <type search='void' replace='ANY' />
 <type search='const char* const' replace='CONST gchar PTR CONST' />
~~~

The text in the attribute `search` is applied to the input, it has to
be specified case-sensitive. It contains just one word for `name` and
`first` rules. In case of a `type` rule it may contain more then one
word, but no asterix characters (`*`) at the end. Attributes `replace`
and `add` are used for the output, they get transfered as-is.

\note The folder `Gir` contains the configuration files generated
      during \Proj development and testing. Check them for further
      examples.

\Proj reports duplicated tags while reading the control file. It outputs messages like

~~~{.txt}
loading Gtk-3.0.GirToBac found double <first>: GtkRcPropertyParser
~~~

The first tag gets used in that case and all further tags with the same
fuunction get skipped (no overrides). And, when finished, \Proj reports
about nonexistent symbols (which are defined in a `search` attribute
but not existing in the `*.gir` file). Those messages look like

~~~{.txt}
Symbols in Gdk-3.0.GirToBac, but not in /usr/share/gir-1.0/Gdk-3.0.gir:

	TouchpadGesturePhase <name>
~~~

You can remove the corresponding tags from the control file in order to
spead up the translation process.

The control files should be shipped together with the translated header
files. On the one hand this helps users to learn about the differences
between the C and the FB header. On the other hand the control file
helps anyone who want to generate an up-date for the FB header, further
on.


# Char Types

In C syntax the `char` (= `gchar`) type is either a numerical value (FB
type `BYTE`) or a string text (FB type `ZSTRING`). There's no issue
when a pointer is declared in the C library. \Proj just passes the
declaration to the output (the `char` type is defined in the standard
header `_GirToBac-0.0.bi` as FB type `ZSTRING`). But when the type is
used without a pointer, \Proj can not determine if it should be a
numerical value or a single character. Therefor this type declaration
gets marked, in order to either handle it manually or to auto-replace
it by a type rule, like

~~~{.xml}
<type search="unsigned char /'?'/" replace="guint8" />
~~~


Have fun, share your results.
