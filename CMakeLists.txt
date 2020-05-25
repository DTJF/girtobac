#
# girtobac - Create FB Headers from GObject Introspection *.gir files
#
# Copyright (C) 2013-2020, Thomas{ dOt ]Freiherr[ aT ]gmx[ DoT }net
#
# All rights reserved.
#
# See ReadMe.md for details.
#

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.3)

# add search path if cmakefbc isn't installed
LIST(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# constants
SET(PROJ_NAME "girtobac"           CACHE STRING "project name" FORCE)
SET(PROJ_DESC "Create FB Headers from GObject Introspection *.gir files" CACHE STRING "project description" FORCE)
SET(PROJ_VERS "0.4.5"              CACHE STRING "project version" FORCE)
SET(PROJ_AUTH "DTJF"               CACHE STRING "project authors" FORCE)
SET(PROJ_MAIL "Thomas{ doT ]Freiherr[ At ]gmx[ DoT }net"  CACHE STRING "project mail address" FORCE)
SET(PROJ_WEBS "https://github.com/DTJF/girtobac" CACHE STRING "project website" FORCE)
SET(PROJ_LICE "GPLv3"              CACHE STRING "project licence" FORCE)
SET(PROJ_YEAR "2020"               CACHE STRING "project year" FORCE)

FUNCTION(MSG_LOG Txt)
  FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log "${Txt}\n\n")
  MESSAGE(STATUS ${Txt})
ENDFUNCTION()

# declare the project name and the language (compiler) to use
PROJECT(${PROJ_NAME} Fbc)

ADD_SUBDIRECTORY(src)

ADD_SUBDIRECTORY(doxy)

CONFIGURE_FILE(_ReadMe.md.in ${CMAKE_SOURCE_DIR}/ReadMe.md @ONLY)


# declare installation rules
#INSTALL(DIRECTORY cmake/Modules DESTINATION ${CMAKE_ROOT})

GET_FILENAME_COMPONENT(fbpath ${CMAKE_Fbc_COMPILER} PATH)
IF(UNIX)
  SET(girdest ${fbpath}/../include/freebasic)
ELSE()
  SET(girdest ${fbpath}/../inc/freebasic)
ENDIF()

INSTALL(DIRECTORY Gir DESTINATION ${girdest}
  FILES_MATCHING PATTERN "*.bi"
  )
