#
# GirToBac - Create FB Headers from GObject Introspection *.gir files
#
# Copyright (C) 2014-2015, Thomas{ dOt ]Freiherr[ aT ]gmx[ DoT }net
#
# All rights reserved.
#
# Build / install the executable.
#

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.3)

CONFIGURE_FILE(girtobac_text.bi.in ${CMAKE_CURRENT_SOURCE_DIR}/girtobac_text.bi @ONLY)

# declare the target name and the source file[s]
ADD_EXECUTABLE(${PROJ_NAME} ${PROJ_NAME}.bas)
# add #INCLUDE file dependencies
ADD_Fbc_SRC_DEPS(${PROJ_NAME})

# define the compiler options (should have -m for executables)
SET_TARGET_PROPERTIES(${PROJ_NAME} PROPERTIES
  COMPILE_FLAGS "-e -w all -m ${PROJ_NAME}"
  )

# declare installation rule
INSTALL(
  TARGETS ${PROJ_NAME}
  DESTINATION bin
  )
