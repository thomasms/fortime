project (${FORTIME_LIB_NAME})
enable_language (Fortran)

set (FORTIME_SOURCES
    ${SOURCE_DIR_BASE}/kinds_m.f90
    ${SOURCE_DIR_BASE}/fortime_m.f90)

# set the flags
set_source_files_properties(${FORTIME_SOURCES} PROPERTIES COMPILE_FLAGS "${F90_FLAGS}")

add_library(${FORTIME_LIB_NAME} ${FORTIME_SOURCES})
target_link_libraries (${FORTIME_LIB_NAME})

set_target_properties(${FORTIME_LIB_NAME}
                        PROPERTIES OUTPUT_NAME ${FORTIME_LIB_NAME}
                        LIBRARY_OUTPUT_DIRECTORY ${FORTIME_BUILD_DIR}/lib
                        ARCHIVE_OUTPUT_DIRECTORY ${FORTIME_BUILD_DIR}/lib
                        Fortran_MODULE_DIRECTORY ${FORTIME_MODULE_DIR})
