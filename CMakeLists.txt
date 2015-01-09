CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(CCWEB)
SET(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR})
#
# Bootstrap: generate ccweave.lua and cctangle.lua from ccweb.ccw
#
ADD_CUSTOM_COMMAND(
  OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/ccweave.lua ${CMAKE_CURRENT_BINARY_DIR}/cctangle.lua
  COMMAND
    lua ${CMAKE_CURRENT_SOURCE_DIR}/bootstrap.lua ${CMAKE_CURRENT_SOURCE_DIR}/ccweb.ccw
  DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/bootstrap.lua ${CMAKE_CURRENT_SOURCE_DIR}/ccweb.ccw)

FIND_PACKAGE(Lua51)
INCLUDE_DIRECTORIES(${LUA_INCLUDE_DIR})
#
# Generate binaries with lua2c
#
# Lua2C requires us to set LUA_PATH, unfortunately. This is easy enough if you're not
# using Windows, which is what most of humanity is using.
#
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

  INSTALL(TARGETS ${prog} RUNTIME DESTINATION bin)
  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/${prog}.lua DESTINATION bin)
ENDFOREACH(prog)

FIND_PACKAGE(LATEX)
IF(PDFLATEX_COMPILER)
  STRING(REPLACE "pdflatex" "pdftex" PDFTEX_COMPILER "${PDFLATEX_COMPILER}")
ENDIF()
#
# Generate documentation with newly generated cctangle binary
#
ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ccweb.tex
  COMMAND
    ccweave ${CMAKE_CURRENT_SOURCE_DIR}/ccweb.ccw
  DEPENDS
    ${CMAKE_CURRENT_BINARY_DIR}/ccweave ${CMAKE_CURRENT_SOURCE_DIR}/ccweb.ccw)

ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ccweb.pdf
  COMMAND
    ${PDFTEX_COMPILER} ${CMAKE_CURRENT_BINARY_DIR}/ccweb.tex
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ccweb.tex)

ADD_CUSTOM_TARGET(ccweb.pdf ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ccweb.pdf)
INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/ccweb.pdf DESTINATION doc)
#
# Test
#
ENABLE_TESTING()
FOREACH(test test_test001 test_test002 test_test003 test_test004 test_test005 test_job_name_option)
  ADD_TEST(NAME ${test}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
    COMMAND lua ${CMAKE_CURRENT_SOURCE_DIR}/tests/test.lua ${test})
ENDFOREACH(test)
