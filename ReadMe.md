Welcome to project *girtobac*, a tool to generate FreeBASIC (FB)
header files for libraries based on
[GOject-Introspection](https://wiki.gnome.org/GObjectIntrospection/),
which is a middleware layer between C libraries (using GObject) and
language bindings. The C library can be scanned at compile time and
generate a metadata file, in addition to the actual native C library.
*girtobac* can read this metadata and automatically provide FB
bindings to call into the C library.

*girtobac* is the first approach to connect the FreeBasic
programming language to this tool-chain for easy creating and up-dating
FreeBasic header files of GObject based libraries (ie. like GTK, GDA,
GooCanvas, ...).

The package is [hosted at GitHub](http://github.com/DTJF/girtobac) and contains

- the *girtobac* source code in folder src
- the header GLib-2.0.bi in folder Gir
- an example input file GIO-2.0.gir in folder example
- several configuration files (*.GirToBac) in folder Gir
- the configuration files to build the documentation in folder doc
- the documentation context in .md files in root folder
- CMake scripts to build the targets all, doc, doc_htm, doc_pdf

Find the online documentation at
http://users.freebasic-portal.de/tjf/Projekte/GirToBac/doc/html/.

(This documentation is generated by the [Doxygen
generator](http://www.doxygen.org/) from FB source code by
[fb-doc](http://github.com/DTJF/fb-doc)
filtering.)


Licence
=======

GPLv3: Copyright (C) 2013-2015, Thomas{ At ]Freiherr{ at }gmx[ dOt ]net

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

To get the licence text refer to
http://www.gnu.org/licenses/gpl-3.0.html or write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110- 1301, USA.
