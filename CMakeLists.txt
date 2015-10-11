#
# GirToBac - Create FB Headers from GObject Introspection *.gir files
#
# Copyright (C) 2014-2015, Thomas{ dOt ]Freiherr[ aT ]gmx[ DoT }net
#
# All rights reserved.
#
# See ReadMe.md for details.
#

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.3)

# constants
SET(PROJ_NAME "girtobac"           CACHE STRING "project name" FORCE)
SET(PROJ_DESC "Create FB Headers from GObject Introspection *.gir files" CACHE STRING "project description" FORCE)
SET(PROJ_VERS "0.4"                CACHE STRING "project version" FORCE)
SET(PROJ_AUTH "DTJF"               CACHE STRING "project authors" FORCE)
SET(PROJ_MAIL "Thomas{ doT ]Freiherr[ At ]gmx[ DoT }net"  CACHE STRING "project mail address" FORCE)
SET(PROJ_WEBS "https://github.com/DTJF/girtobac" CACHE STRING "project website" FORCE)
SET(PROJ_LICE "GPLv3"              CACHE STRING "project licence" FORCE)
SET(PROJ_YEAR "2015"               CACHE STRING "project year" FORCE)

# Uncomment the next two lines to perform a standard build.
LIST(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")
ADD_SUBDIRECTORY(src)

ADD_SUBDIRECTORY(doc)

PROJECT(${PROJ_NAME} Fbc)

CONFIGURE_FILE(_ReadMe.md.in ${CMAKE_SOURCE_DIR}/ReadMe.md @ONLY)
#INSTALL(DIRECTORY cmake/Modules DESTINATION ${CMAKE_ROOT})
