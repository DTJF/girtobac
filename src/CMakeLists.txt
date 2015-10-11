CMAKE_MINIMUM_REQUIRED(VERSION 2.8.3)

# declare the project name and the language (compiler) to use
PROJECT(${PROJ_NAME} Fbc)

# constants
SET(PROJ_NAME "girtobac"             CACHE STRING "project name" FORCE)
SET(PROJ_DESC "Create FB Headers from GObject Introspection *.gir files" CACHE STRING "project description" FORCE)
SET(PROJ_VERS "0.4.0"               CACHE STRING "project version" FORCE)
SET(PROJ_AUTH "DTJF"               CACHE STRING "project authors" FORCE)
SET(PROJ_MAIL "Thomas{ At ]Freiherr{ at }gmx[ dOt ]net"  CACHE STRING "project mail address" FORCE)
SET(PROJ_WEBS "http://github.com/DTJF/girtobac" CACHE STRING "project website" FORCE)
SET(PROJ_LICE "GPLv3"              CACHE STRING "project licence" FORCE)
STRING(TIMESTAMP tmp "%Y" UTC)
SET(PROJ_YEAR "${tmp}"             CACHE STRING "project year" FORCE)

# declare the target name and the source file[s]
ADD_EXECUTABLE(${PROJ_NAME} ${PROJ_NAME}.bas)

# define the compiler options (should have -m for executables)
SET_TARGET_PROPERTIES(${PROJ_NAME} PROPERTIES
  COMPILE_FLAGS "-e -w all -m ${PROJ_NAME}"
  )


# declare installation rule
INSTALL(
  TARGETS ${PROJ_NAME}
  RUNTIME DESTINATION local/bin
  )
