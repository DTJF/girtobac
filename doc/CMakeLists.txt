# Build / install the documentation.
#
# Note: this script needs an fb-doc version (0.4).

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.3)

IF(NOT COMMAND FB_DOCUMENTATION)
  INCLUDE(UseFb-Doc)
ENDIF()

FB_DOCUMENTATION(
BAS_SRC
  ../src/girtobac.bas
  ../src/girtobac_text.bi
  ../src/girtobac_RepData.bas
DEPENDS
  ../ReadMe.md
  ../_Introduction.md
  ../_Preparation.md
  ../_Tutorial.md
  ../_ChangeLog.md
  ../CMakeLists.txt
  girtobac.xml
  )
