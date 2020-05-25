Preparation  {#PagPreparation}
===========
\tableofcontents

This chapter is about

- how to prepare your system to use \Proj by installing tools,
- how to get the package and
- how to build the executable and the documentation.


# Tools  {#SecTools}

The following table lists all dependencies for \Proj and their types.
At least, you have to install the GLib library (containing GObject) and the FreeBASIC
compiler on your system to build your \Proj executable. (Of course,
later you'll need it for your projects as well.) Beside this mandatory
(M) tool, the others are optional. Some are recommended (R) in order to
make use of all package features. LINUX users find some packages in
their distrubution management system (D).

|                                        Name  | Type |  Function                                                      |
| -------------------------------------------: | :--: | :------------------------------------------------------------- |
| [GLib](https://wiki.gnome.org/Projects/GLib) | M  D | Integrated development environment (ie. to test templates)     |
| [fbc](http://www.freebasic.net)              | M    | FreeBASIC compiler to compile the source code                  |
| [GIT](http://git-scm.com/)                   | R  D | version control system to organize the files                   |
| [CMake](http://www.cmake.org)                | R  D | build management system to build executables and documentation |
| [cmakefbc](http://github.com/DTJF/cmakefbc)  | R    | FreeBASIC extension for CMake                                  |
| [fbdoc](http://github.com/DTJF/fbdoc)        | R    | FreeBASIC extension tool for Doxygen                           |
| [Doxygen](http://www.doxygen.org/)           | R  D | documentation generator (ie. for this text)                    |
| [Graphviz](http://www.graphviz.org/)         | R  D | Graph Visualization Software (caller/callee graphs)            |
| [LaTeX](https://latex-project.org/ftp.html)  | R  D | A document preparation system (for PDF output)                 |

It's beyond the scope of this guide to describe the installation for
those programming tools. Find detailed installation instructions on the
related websides, linked by the name in the first column.

-# First, install the distributed (D) packages of your choise.

-# Make the FB compiler working. If you aren't confident about
   the task you can find a few notes on the [Installing
   FreeBASIC](http://www.freebasic.net/wiki/wikka.php?wakka=CompilerInstalling)
   wiki page.

-# Install cmakefbc, if wanted. That's easy, when you have GIT and CMake.
   Execute the commands
   ~~~{.txt}
   git clone https://github.com/DTJF/cmakefbc
   cd cmakefbc
   mkdir build
   cd build
   cmake .. -DCMAKE_MODULE_PATH=../cmake/Modules
   make
   sudo make install
   ~~~
   \note Omit `sudo` in case of non-LINUX systems.

-# And similar, install fbdoc, if wanted, using GIT and CMake.
   Execute the commands
   ~~~{.txt}
   git clone https://github.com/DTJF/fbdoc
   cd fbdoc
   mkdir build
   cd build
   cmakefbc ..
   make
   sudo make install
   ~~~
   \note Omit `sudo` in case of non-LINUX systems.


# Get Package  {#SecGet}

Depending on whether you installed the optional GIT package, there're
two ways to get the \Proj package.

## GIT  {#SubSecGit}

Using GIT is the prefered way to download the \Proj package (since it
helps users to get involved in to the development process). Get your
copy and change to the source tree by executing

~~~{.txt}
git clone https://github.com/DTJF/girtobac
cd girtobac
~~~

## ZIP  {#SubSecZip}

As an alternative you can download a Zip archive by clicking the
[Download ZIP](https://github.com/DTJF/girtobac/archive/master.zip)
button on the \Proj website, and use your local Zip software to unpack
the archive. Then change to the newly created folder.


# Build the executable  {#SecExecutable}

Before you can use the \Proj tool to generate your header files, you've
to compile the source code first by either

- using the CMake build scripts, or
- direct compiling by calling the FB compiler in the src folder.


## CMake build  {#SubSecCMakeBuild}

The prefered way to build the executable and the documentation files is
to use the scripts for the CMake build system. If you don't want to
install or to use CMake, then skip this section and continue at \ref
SubSecDirectComp.

The CMake scripts check your system and through warnings if anything is
missing. Otherwise you can either perform an in-source or an
out-of-source build. The later should be your prefered choise.


### In-Source-Build

The following command triple will compile the executable in the source
tree and install it on your system:

~~~{.txt}
cmakefbc .
make
sudo make install
~~~

\note Omit `sudo` in case of non-LINUX systems.

\note In-Source-Builds polute the source tree by newly created files.


### Out-Of-Source-Build

The following command quintuple will create a new *build* folder,
change to that folder, compile the executable and install it on your
system:

~~~{.txt}
mkdir build
cd build
cmakefbc ..
make
sudo make install
~~~

\note Omit `sudo` in case of non-LINUX systems.


### Documentation-Build

In order to build the documentation, all recommended packages listed in
section \ref SecTools have to get installed. The following command will
build the documentation in form of an HTML file tree and in form of a
PDF file (either in-source or out-of-source):

~~~{.txt}
make doc
~~~

\note Find the HTML start file at `doxy/html/index.html`.
\note Find the PDF file at `doxy/girtobac.pdf`.

Both targets can get build separately by executing

~~~{.txt}
make doc_htm
make doc_pdf
~~~


## Direct compiling  {#SubSecDirectComp}

### Executable

In order to build the executable change from the package root directory
to the *src* folder and compile by executing

~~~{.txt}
cd src
fbc -e -w all girtobac.bas
~~~

This creates an executable binary named

- *girtobac* (on UNIX-like systems) or
- *girtobac.exe* (on other systems).

that you can install wherever you need it.  

### Documentation

In order to build the documentation, install the tools fbdoc, Doxygen
and Graphviz. Then change from the package root directory to the *doc*
folder, up-date the file *fb-doc.lfn*, execute the Doxygen generator
and adapt correct listings by executing

~~~{.txt}
cd doc
fbdoc -l
doxygen Doxyfile
fbdoc -s
~~~

to build the documentation in subfolders *html* (start file =
index.html) and *latex* (call `make` in that folder to build
refman.pdf).

\note Adapt the configuration file *Doxyfile* (or your customized copy)
      in order to fit the output to your needs.

