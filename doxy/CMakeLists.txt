# Build / install the documentation.
#
# Note: this script needs an fbdoc version (0.4).

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.3)

IF(NOT COMMAND FB_DOCUMENTATION)
  INCLUDE(UseFbDoc)
  IF(NOT COMMAND FB_DOCUMENTATION)
    MSG_LOG("!! no targets <doc...> (UseFbDoc.cmake failed)")
    RETURN()
  ENDIF()
ENDIF()

FB_DOCUMENTATION(
BAS_SRC
  ../src/girtobac.bas
  ../src/girtobac_text.bi
  ../src/girtobac_RepData.bas
DEPENDS
  ../ReadMe.md
  ../_1_Introduction.md
  ../_2_Preparation.md
  ../_3_Tutorial.md
  ../_9_ChangeLog.md
  ../CMakeLists.txt
  Doxyfile
  girtobac.css
  girtobac.xml
  )
