# Build / install the documentation.
#
# Note: this script needs an fb-doc version (0.4).

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

# set variables
SET(PDF_FILE ${CMAKE_CURRENT_BINARY_DIR}/${PROJ_NAME}.pdf)
SET(HTM_FILE ${CMAKE_CURRENT_BINARY_DIR}/html/index.html)
SET(WWW_FILE ${CMAKE_CURRENT_BINARY_DIR}/DocWWW.time)
SET(REF_FILE ${CMAKE_CURRENT_BINARY_DIR}/latex/refman.pdf)

SET(doxyext ${CMAKE_CURRENT_BINARY_DIR}/DoxyExtension)
SET(htmconf ${CMAKE_CURRENT_BINARY_DIR}/HtmConf)
SET(pdfconf ${CMAKE_CURRENT_BINARY_DIR}/PdfConf)
SET(lfn ${CMAKE_CURRENT_SOURCE_DIR}/fb-doc.lfn)
SET(MD_SRC
    ../ReadMe.md
    ../_Introduction.md
    ../_Preparation.md
    ../_Tutorial.md
    ../_ChangeLog.md
  )
SET(deps
    ../CMakeLists.txt
    ${lfn}
    ../src/${PROJ_NAME}.bas
    ../src/${PROJ_NAME}_RepData.bas
    ../src/${PROJ_NAME}_text.bi.in
    ../_ReadMe.md.in
    ${CMAKE_CURRENT_SOURCE_DIR}/DoxyExtension.in
    ${CMAKE_CURRENT_SOURCE_DIR}/girtobac.xml
  )

#configure the extension files for Doxygen output
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/DoxyExtension.in ${doxyext} @ONLY)
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/HtmConf.in ${htmconf} @ONLY)
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/PdfConf.in ${pdfconf} @ONLY)

# update the list of file names
ADD_CUSTOM_COMMAND(OUTPUT ${lfn}
  COMMAND ${FbDoc_EXECUTABLE} -l ${doxyext}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )

#build the html tree
ADD_CUSTOM_COMMAND(OUTPUT ${HTM_FILE}
  COMMAND ${DOXYGEN_EXECUTABLE} ${htmconf}
  #COMMAND ${FbDoc_EXECUTABLE} -s ${htmconf}
  DEPENDS ${deps} ${MD_SRC}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  VERBATIM
  )

# build the pdf file
ADD_CUSTOM_COMMAND(OUTPUT ${PDF_FILE}
  COMMAND ${DOXYGEN_EXECUTABLE} ${pdfconf}
  COMMAND ${FbDoc_EXECUTABLE} -s ${pdfconf}
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR}/latex make
  COMMAND ${CMAKE_COMMAND} -E rename ${REF_FILE} ${PDF_FILE}
  DEPENDS ${deps} ${MD_SRC}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  VERBATIM
  )

# mirror the local html tree to the www
ADD_CUSTOM_COMMAND(OUTPUT ${WWW_FILE}
  COMMAND MirrorDoc.sh '--reverse --delete --verbose ${CMAKE_CURRENT_BINARY_DIR}/html public_html/Projekte/${PROJ_NAME}/doc/html'
  )

# add targets
ADD_CUSTOM_TARGET(doc_htm DEPENDS ${HTM_FILE})
ADD_CUSTOM_TARGET(doc_pdf DEPENDS ${PDF_FILE})
ADD_CUSTOM_TARGET(doc)
ADD_DEPENDENCIES(doc DEPENDS doc_htm doc_pdf)
ADD_CUSTOM_TARGET(doc_www DEPENDS ${WWW_FILE})
ADD_DEPENDENCIES(doc_www DEPENDS doc_htm)
