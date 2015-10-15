Changelog & Credits {#PagChangelog}
===================
\tableofcontents

Further Development  {#SecToDo}
===================

\Proj is already a powerful tool to auto-generate FB header files. But
there's still some potential optimization, like:

- finish OOP support (... when the fbc got it)
- more automated features like sorting the order or resolving double DECLARES
- ...

Feel free to post your ideas, bug reports, wishes or patches, either
to the project page at

- \Webs

or to the

- [forum page](http://www.freebasic.net/forum/viewtopic.php?p=190158)

or feel free to send your ideas directly to the author (\Email).


Versions  {#SecVersions}
========

Here's a list of the published versions:

girtobac-0.4  {#SubSecV-0-4}
------------

New:

- GIT version control system
- renamed for GIT (lower case)
- hosted on GitHub.com
- CMake build scripts for executable and documentation
- code listings clean of documentational comments (fb-doc-0.4.0)
- options -h and -v

Bugfixes:

- Minor adaptions in the documentation context
- more user information in output
- optimzed caller / callee graphs (fb-doc-0.4.0)

Released on 2015 October, ??  .


GirToBac-0.2  {#SubSecV-0-2}
------------

New:

- GirToBac source 32/64 bit ready.
- Translated header code 32/64 bit ready.
- Self translated GLib header in use.
- Configuraton file attributes \em binary, \em check and \em pack.
- Caller / callee graphs in the documentation.

Bugfixes:

- Gir attribute "throws" now recognized in all functions.
- example file changed to GLib-2.0 (since Gio-2.0.bi needs further headers to compile)
- String literals (instead of `!@"..."` now correct `@!"..."` is used).
- Minor adaptions in the documentation.

Released on 2014 September, 23.


GirToBac-0.0  {#SubSecV-0-0}
------------

Initial release on 2013 August, 11.


Credits  {#SecCredits}
=======

Thanks go to:

- The FreeBASIC developer team for creating a great compiler.

- Colin Walters, Johan Dahlin, Matthias Clasen and Jürg Billeter for
  developing gobject-introspection.

- Dimitri van Heesch for creating the Doxygen tool, which is used to
  generate this documentation.

- All others I forgot to mention.
