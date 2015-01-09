CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(CCWEB)
SET(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR})
#------------------------------------------------------------------------------
#
# Bootstrap: generate ccweave.lua and cctangle.lua from ccweb.ccw
#
#------------------------------------------------------------------------------
ADD_CUSTOM_COMMAND(
  OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/ccweave.lua ${CMAKE_CURRENT_BINARY_DIR}/cctangle.lua
  COMMAND
    lua ${CMAKE_CURRENT_SOURCE_DIR}/bootstrap.lua ${CMAKE_CURRENT_SOURCE_DIR}/ccweb.ccw
  DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/bootstrap.lua ${CMAKE_CURRENT_SOURCE_DIR}/ccweb.ccw)

FIND_PACKAGE(Lua51)
INCLUDE_DIRECTORIES(${LUA_INCLUDE_DIR})
#------------------------------------------------------------------------------
#
# Generate binaries with lua2c
#
# Lua2C requires us to set LUA_PATH, unfortunately. This is easy enough if you're not
# using Windows, which is what most of humanity is using.
#
#------------------------------------------------------------------------------
IF(NOT CMAKE_VERSION VERSION_LESS "3.1")
  SET(ENV_PRE ${CMAKE_COMMAND} -E)
ENDIF()

FOREACH(prog cctangle ccweave)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${prog}.c
    COMMAND
      ${ENV_PRE} env LUA_PATH=${PROJECT_SOURCE_DIR}/submodules/lua2c/lib/?.lua
      lua
      ${PROJECT_SOURCE_DIR}/submodules/lua2c/lua2c.lua
      ${CMAKE_CURRENT_BINARY_DIR}/${prog}.lua
      > ${CMAKE_CURRENT_BINARY_DIR}/${prog}.c
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${prog}.lua)

  ADD_EXECUTABLE(${prog} ${prog}.c)
  TARGET_LINK_LIBRARIES(${prog} ${LUA_LIBRARIES})

  INSTALL(TARGETS ${prog} RUNTIME DESTINATION bin COMPONENT binaries)
  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/${prog}.lua DESTINATION bin COMPONENT scripts)
ENDFOREACH(prog)
#------------------------------------------------------------------------------
#
# Generate documentation with newly generated cctangle binary
#
# CMake does not make it too easy to properly add a target that is also
# a file name, so we make use of spurious files created by the pdftex run
# to make it happen.
#
#------------------------------------------------------------------------------
FIND_PACKAGE(LATEX)
IF(PDFLATEX_COMPILER)
  GET_FILENAME_COMPONENT(PDFLATEX_COMPILER_PATH ${PDFLATEX_COMPILER} ABSOLUTE)
  FIND_PROGRAM(PDFTEX_COMPILER pdftex PATHS ${PDFLATEX_COMPILER_PATH})
ENDIF()
IF(NOT PDFTEX_COMPILER)
  MESSAGE(FATAL_ERROR "Could not find pdftex.")
ENDIF()

ADD_CUSTOM_COMMAND(
  OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/ccweb.tex
    ${CMAKE_CURRENT_BINARY_DIR}/ccweb.idx
    ${CMAKE_CURRENT_BINARY_DIR}/ccweb.scn
  COMMAND
    ${CMAKE_CURRENT_BINARY_DIR}/ccweave ${CMAKE_CURRENT_SOURCE_DIR}/ccweb.ccw
  DEPENDS
    ccweave ${CMAKE_CURRENT_SOURCE_DIR}/ccweb.ccw
  COMMENT "Generating ccweb.tex")

ADD_CUSTOM_COMMAND(
  OUTPUT # Leave out ccweb.pdf so we can make it a target later on.
    ${CMAKE_CURRENT_BINARY_DIR}/ccweb.log
    ${CMAKE_CURRENT_BINARY_DIR}/ccweb.toc
  COMMAND
    ${PDFTEX_COMPILER} ${CMAKE_CURRENT_BINARY_DIR}/ccweb.tex
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ccweb.tex
  COMMENT "Generating ccweb.pdf")

ADD_CUSTOM_TARGET(ccweb.pdf ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ccweb.log)
SET_DIRECTORY_PROPERTIES(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ccweb.pdf)

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/ccweb.pdf DESTINATION doc COMPONENT documentation)
#------------------------------------------------------------------------------
#
# Test
#
#------------------------------------------------------------------------------
ENABLE_TESTING()
FOREACH(test test_test001 test_test002 test_test003 test_test004 test_test005 test_job_name_option)
  ADD_TEST(NAME ${test}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
    COMMAND lua ${CMAKE_CURRENT_SOURCE_DIR}/tests/test.lua ${test})
ENDFOREACH(test)
