# CMake project file for forts
cmake_minimum_required (VERSION 2.8)

# project name and details
project (Fortime)
enable_language (Fortran)
set(PROJECT_DESCRIPTION "Fortime")
set(PROJECT_URL "https://git.ccfe.ac.uk/thomasms/fortime")

# set source directories
set (SOURCE_DIR_BASE ${CMAKE_CURRENT_LIST_DIR}/source)
set (FORTIME_EXAMPLE_DIR ${CMAKE_CURRENT_LIST_DIR}/example)
set (FORTIME_BUILD_DIR ${CMAKE_BINARY_DIR})

# library names
set (FORTIME_LIB_NAME fortime)

# example name
set (FORTIME_EXAMPLE_NAME fortimeexample)

# Set release build by default
set (CMAKE_BUILD_TYPE Release CACHE STRING "Select which configuration to build.")
set (FORTIME_BUILD_TYPE "Release")
if(CMAKE_BUILD_TYPE)
    set(FORTIME_BUILD_TYPE ${CMAKE_BUILD_TYPE})
else()
    set(CMAKE_BUILD_TYPE ${FORTIME_BUILD_TYPE})
endif()

# set the general fortran Debug/Release compiler flags
set(FORTIME_RELEASE_FLAGS "-O2")
set(FORTIME_DEBUG_FLAGS "-g -O0")

set(CMAKE_Fortran_FLAGS_RELEASE ${FORTIME_RELEASE_FLAGS})
set(CMAKE_Fortran_FLAGS_DEBUG ${FORTIME_DEBUG_FLAGS})

# Fortran OS specific compiler flags
if (CMAKE_Fortran_COMPILER_ID MATCHES MSVC)
    set ( F90_FLAGS_DEBUG "-fp-model")
    set ( F90_FLAGS_RELEASE  "-fp-model" )
elseif (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set ( F90_FLAGS_DEBUG "-debug  -CB -CA -CU -std08 -fpp -fp-model source")
    set ( F90_FLAGS_RELEASE  "-std08 -unroll -fpp -fp-model source -diag-disable 8291,8577" )
elseif (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set ( F90_FLAGS_DEBUG "-cpp -ffree-form  -ffloat-store -fno-sign-zero -std=f2008 -ffree-line-length-none")
    set ( F90_FLAGS_RELEASE  "-cpp -ffree-form -ffloat-store -fno-sign-zero -std=f2008 -ffree-line-length-none" )
endif ()

# Release or Debug build
if(FORTIME_BUILD_TYPE MATCHES Release)
    message(STATUS "Release build")
    set(F90_FLAGS ${F90_FLAGS_RELEASE})
elseif(FORTIME_BUILD_TYPE MATCHES Debug)
    message(STATUS "Debug build")
    set(F90_FLAGS ${F90_FLAGS_DEBUG})
else()
    message(STATUS "Default build (Release)")
    set(F90_FLAGS ${F90_FLAGS_RELEASE})
endif()

# set the output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY bin)

# Collect all the mod files into their own include directory
set(FORTIME_MODULE_DIR ${FORTIME_BUILD_DIR}/include)

# set include and link directories
include_directories (${FORTIME_BUILD_DIR}/include)
link_directories (${FORTIME_BUILD_DIR}/lib)

# libs and examples
add_subdirectory(${SOURCE_DIR_BASE})
add_subdirectory(${FORTIME_EXAMPLE_DIR})
