# Note: this script needs an fb-doc version (0.4), which may not be published yet.

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.3)

# check for fb-doc tool
INCLUDE(FindFb-Doc)
IF(NOT FbDoc_WORKS)
  MESSAGE(STATUS "fb-doc tool not found ==> targets doc, doc_htm and doc_pdf not available!")
  FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
    "Determining if fb-doc tool works failed\n\n")
  RETURN()
ENDIF()

# check for Doxygen
INCLUDE(FindDoxygen)
IF(NOT DOXYGEN_FOUND)
  MESSAGE(STATUS "Doxygen not found ==> target doc not available!")
  FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
    "Determining if doxygen works failed\n\n")
  RETURN()
ENDIF()


CONFIGURE_FILE(DoxyExtension.in DoxyExtension @ONLY)

SET(PDF_FILE ${CMAKE_CURRENT_BINARY_DIR}/latex/refman.pdf)
SET(HTM_FILE ${CMAKE_CURRENT_BINARY_DIR}/html/index.html)

SET(htmconf ${CMAKE_CURRENT_BINARY_DIR}/HtmConf)
SET(pdfconf ${CMAKE_CURRENT_BINARY_DIR}/PdfConf)
SET(lfn ${CMAKE_CURRENT_SOURCE_DIR}/fb-doc.lfn)
SET(MD_SRC
    ../ReadMe.md
    ../_Tutorial.md
    ../_ChangeLog.md
  )
SET(deps
    ../CMakeLists.txt
    ${lfn}
    ${MD_SRC}
    ../src/${PROJ_NAME}.bas
    ../src/${PROJ_NAME}_RepData.bas
    ../_ReadMe.md.in
    ${CMAKE_CURRENT_SOURCE_DIR}/DoxyExtension.in
    ${CMAKE_CURRENT_SOURCE_DIR}/girtobac.xml
  )

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/DoxyExtension.in ${CMAKE_CURRENT_BINARY_DIR}/DoxyExtension @ONLY)
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/HtmConf.in ${htmconf} @ONLY)
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/PdfConf.in ${pdfconf} @ONLY)

ADD_CUSTOM_COMMAND(OUTPUT ${lfn}
  COMMAND ${FbDoc_EXECUTABLE} -l
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )

ADD_CUSTOM_COMMAND(OUTPUT ${HTM_FILE}
  COMMAND ${DOXYGEN_EXECUTABLE} ${htmconf}
  COMMAND ${FbDoc_EXECUTABLE} -s ${htmconf}
  DEPENDS ${deps}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )

ADD_CUSTOM_COMMAND(OUTPUT ${PDF_FILE}
  COMMAND ${DOXYGEN_EXECUTABLE} ${pdfconf}
  COMMAND ${FbDoc_EXECUTABLE} -s ${pdfconf}
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR}/latex make
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/latex/refman.pdf ${CMAKE_CURRENT_BINARY_DIR}/${PROJ_NAME}.pdf
  DEPENDS ${deps}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  VERBATIM
  )

ADD_CUSTOM_TARGET(doc_htm DEPENDS ${HTM_FILE})
ADD_CUSTOM_TARGET(doc_pdf DEPENDS ${PDF_FILE})
ADD_CUSTOM_TARGET(doc)
ADD_DEPENDENCIES(doc DEPENDS doc_htm doc_pdf)
