project (${FORTIME_EXAMPLE_NAME})
enable_language (Fortran)

set (EXAMPLE_EXE_SOURCE ${FORTIME_EXAMPLE_DIR}/example.f90)
set_source_files_properties(${EXAMPLE_EXE_SOURCE} PROPERTIES COMPILE_FLAGS "${F90_FLAGS}")

add_executable (${FORTIME_EXAMPLE_NAME} ${EXAMPLE_EXE_SOURCE})

# we can link against it or simply compile the source - here we link
target_link_libraries (${FORTIME_EXAMPLE_NAME} ${FORTIME_LIB_NAME})
set_target_properties(${FORTIME_EXAMPLE_NAME}
                      PROPERTIES OUTPUT_NAME ${FORTIME_EXAMPLE_NAME}
                      RUNTIME_OUTPUT_DIRECTORY ${FORTIME_BUILD_DIR}/bin)
