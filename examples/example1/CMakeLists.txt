set (FORTIME_EXAMPLE_NAME1 fortimeexample1)
project (${FORTIME_EXAMPLE_NAME1})
enable_language (Fortran)

set (EXAMPLE_EXE_SOURCE ${FORTIME_EXAMPLE_DIR}/example1/example.F90)
set_source_files_properties(${EXAMPLE_EXE_SOURCE} PROPERTIES COMPILE_FLAGS "${F90_FLAGS}")

add_executable (${FORTIME_EXAMPLE_NAME1} ${EXAMPLE_EXE_SOURCE})

# we can link against it or simply compile the source - here we link
target_link_libraries (${FORTIME_EXAMPLE_NAME1} ${FORTIME_LIB_NAME})
set_target_properties(${FORTIME_EXAMPLE_NAME1}
                      PROPERTIES OUTPUT_NAME ${FORTIME_EXAMPLE_NAME1}
                      RUNTIME_OUTPUT_DIRECTORY ${FORTIME_BUILD_DIR}/bin)
