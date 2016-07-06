#################################################################
# HEADER
#################################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.7)
SET(CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS TRUE)

PROJECT(XH5For Fortran C)

SET(${PROJECT_NAME}_VERSION 0.0.1)
SET(${PROJECT_NAME}_SOVERSION 1)
SET(LIB ${PROJECT_NAME})

SET(CMAKE_VERBOSE_MAKEFILE TRUE)

#################################################################
# DEFINE PATHS
#################################################################

SET(SRC_PATH ${CMAKE_SOURCE_DIR}/src)
SET(LIB_PATH ${SRC_PATH}/lib)
SET(EXAMPLES_PATH ${SRC_PATH}/examples)
SET(TESTS_PATH ${SRC_PATH}/tests)
SET(INCLUDE_PATH ${SRC_PATH}/include)

#SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake/Modules/")


#################################################################
# BUILD PATHS
#################################################################

SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
SET(THIRDPARTY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/thirdparty)
INCLUDE_DIRECTORIES(${CMAKE_Fortran_MODULE_DIRECTORY})

#################################################################
# ADD INCLUDE DIRS
#################################################################

include_directories(${INCLUDE_PATH})

#################################################################
# CONFIGURATION TYPES & BUILD MODE
#################################################################

SET(CMAKE_CONFIGURATION_TYPES DEBUG RELEASE)
IF(NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE DEBUG CACHE STRING
      "Choose the type of build, options are: NONE DEBUG RELEASE"
      FORCE)

  SET_PROPERTY(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS NONE DEBUG RELEASE)
ENDIF(NOT CMAKE_BUILD_TYPE)

#################################################################
# STATIC LIBRARIES
#################################################################
# Try to search first static libraries
IF(NOT ${BUILD_SHARED_LIBS})
    SET(CMAKE_FIND_LIBRARY_SUFFIXES ".a ${CMAKE_FIND_LIBRARY_SUFFIXES}")
    SET( HDF5_USE_STATIC_LIBRARIES ON)
ENDIF()

#################################################################
# List of external libraries
#################################################################

SET(EXT_LIBS  MPI HDF5)

# HDF5 components to search
SET(HDF5_FIND_COMPONENTS C Fortran HL Fortran_HL)

#################################################################
# Find packages
#################################################################

FOREACH(EXT_LIB ${EXT_LIBS})
    IF(DEFINED ${PROJECT_NAME}_ENABLE_${EXT_LIB} AND ${PROJECT_NAME}_ENABLE_${EXT_LIB} AND NOT ${EXT_LIB}_FOUND)
        IF(${EXT_LIB} STREQUAL "HDF5")
            FIND_PACKAGE( ${EXT_LIB} COMPONENTS C Fortran HL Fortran_HL)
        ELSE()
            FIND_PACKAGE( ${EXT_LIB} )
        ENDIF()
    
        IF (${EXT_LIB}_FOUND)
            IF(IS_DIRECTORY ${${EXT_LIB}_INCLUDE_DIR})
               INCLUDE_DIRECTORIES(${${EXT_LIB}_INCLUDE_DIR})
            ENDIF()
        ENDIF()

    ELSEIF(DEFINED ${PROJECT_NAME}_ENABLE_${EXT_LIB} AND NOT ${PROJECT_NAME}_ENABLE_${EXT_LIB})
        UNSET(${EXT_LIB}_INCLUDES CACHE)
        UNSET(${EXT_LIB}_LIBS CACHE)
        UNSET(${EXT_LIB}_INCLUDE_DIR CACHE)
        UNSET(${EXT_LIB}_LIBRARIES CACHE)

    ELSE(NOT DEFINED ${PROJECT_NAME}_ENABLE_${EXT_LIB})
        IF(${EXT_LIB} STREQUAL "HDF5")
            FIND_PACKAGE( ${EXT_LIB} COMPONENTS C Fortran HL Fortran_HL)
        ELSE()
            FIND_PACKAGE( ${EXT_LIB} )
        ENDIF()

        IF (${${EXT_LIB}_FOUND})
           SET(NO_LIBS FALSE)
           OPTION(${PROJECT_NAME}_ENABLE_${EXT_LIB} "Enable/disable ${EXT_LIB} library" ON)
           IF(IS_DIRECTORY ${${EXT_LIB}_INCLUDE_DIR})
               INCLUDE_DIRECTORIES(${${EXT_LIB}_INCLUDE_DIR})
           ENDIF()
        ELSE()
           OPTION(${PROJECT_NAME}_ENABLE_${EXT_LIB} "Enable/disable ${EXT_LIB} library" OFF)
           UNSET(${EXT_LIB}_INCLUDES CACHE)
           UNSET(${EXT_LIB}_LIBS CACHE)
           UNSET(${EXT_LIB}_INCLUDE_DIR CACHE)
           UNSET(${EXT_LIB}_LIBRARIES CACHE)
        ENDIF()
    ENDIF()
ENDFOREACH ()

#################################################################
# MPI Fortran DEFINITIONS AND PATHS
#################################################################

IF(MPI_Fortran_COMPILER AND ${PROJECT_NAME}_ENABLE_MPI)
    # Add MPI directories
    INCLUDE_DIRECTORIES(${MPI_Fortran_INCLUDE_PATH})

    # Find mpi.mod
    FIND_PATH(MPI_MOD_FOUND mpi.mod  PATHS ${MPI_Fortran_INCLUDE_PATH})
    IF(MPI_MOD_FOUND)
        TRY_COMPILE(FORTRAN_COMPILER_MATCH_MPI_MOD 
                    ${PROJECT_BINARY_DIR} 
                    ${TESTS_PATH}/try_compile_mpi_mod.f90	
                    CMAKE_FLAGS
                        "-DINCLUDE_DIRECTORIES:STRING=${MPI_Fortran_INCLUDE_PATH}"
                        "-DLINK_LIBRARIES:STRING=${MPI_Fortran_LIBRARIES}"
                    OUTPUT_VARIABLE OUTPUTVARIABLE)

        IF(FORTRAN_COMPILER_MATCH_MPI_MOD)
            ADD_DEFINITIONS(-DMPI_MOD)
        ELSE()
            MESSAGE(WARNING "MPI: mpi.mod file is NOT COMPATIBLE with this compiler! mpif.h is going be included instead")
            ADD_DEFINITIONS(-DMPI_H)            
        ENDIF()
    ELSE()
        MESSAGE(WARNING "MPI: mpi.mod file NOT FOUND! mpif.h is going to be included instead")
        ADD_DEFINITIONS(-DMPI_H)
    ENDIF()
    ADD_DEFINITIONS(-DENABLE_MPI)
ENDIF()


#################################################################
# HDF5 Fortran DEFINITIONS AND PATHS
#################################################################

IF(HDF5_Fortran_COMPILER_EXECUTABLE AND ${PROJECT_NAME}_ENABLE_HDF5)
    # Add HDF5 directories
    INCLUDE_DIRECTORIES(${HDF5_Fortran_INCLUDE_DIR})

    # Find hdf5.mod
    FIND_PATH(HDF5_MOD_FOUND hdf5.mod  PATHS ${HDF5_Fortran_INCLUDE_DIR})
    IF(HDF5_MOD_FOUND)
        TRY_COMPILE(FORTRAN_COMPILER_MATCH_HDF5_MOD 
                    ${PROJECT_BINARY_DIR} 
                    ${TESTS_PATH}/try_compile_hdf5_mod.f90	
                    CMAKE_FLAGS
                        "-DINCLUDE_DIRECTORIES:STRING=${HDF5_Fortran_INCLUDE_DIR}"
                    LINK_LIBRARIES ${HDF5_Fortran_LIBRARIES})

        IF(FORTRAN_COMPILER_MATCH_HDF5_MOD)
            ADD_DEFINITIONS(-DENABLE_HDF5)
            IF (HDF5_IS_PARALLEL)
                ADD_DEFINITIONS(-DENABLE_PARALLEL_HDF5)
            ELSEIF(${PROJECT_NAME}_ENABLE_MPI)
                MESSAGE(FATAL_ERROR "ERROR: INDEPENDENT IO in PARALLEL environments not supported!")
            ENDIF()
        ELSE()
            MESSAGE(WARNING "HDF5: hdf5.mod file is NOT COMPATIBLE! Disabling HDF5 ... ")
            SET(${PROJECT_NAME}_ENABLE_HDF5 OFF)
        ENDIF()
    ELSE()
        MESSAGE(WARNING "HDF5: hdf5.mod file NOT FOUND! Disabling HDF5 ...")
        SET(${PROJECT_NAME}_ENABLE_HDF5 OFF)
    ENDIF()
ENDIF()

#################################################################
# FFLAGS depend on the compiler and the build type
#################################################################

GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF(CMAKE_BUILD_TYPE STREQUAL "DEBUG")
#   SET(MACROS "${MACROS} -DDEBUG -Dmemcheck")
    ADD_DEFINITIONS(-DDEBUG)
    ADD_DEFINITIONS(-Dmemcheck)
ENDIF()

ADD_DEFINITIONS(-D${CMAKE_Fortran_COMPILER_ID})

message(STATUS "COMPILER INFO: ${CMAKE_Fortran_COMPILER_ID} - ${Fortran_COMPILER_NAME}")

IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
  # gfortran 
  set(FORTRAN_FLAGS "-fdefault-real-8 -ffree-line-length-0 -cpp -Wimplicit-interface ${EXTRA_FLAGS} ")
  set (CMAKE_Fortran_FLAGS "${FORTRAN_FLAGS} ${MACROS} ${INCLUDES} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 ${EXTRA_FLAGS} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -fbacktrace -fbounds-check -fprofile-arcs -ftest-coverage -Wimplicit-interface ${EXTRA_FLAGS} " CACHE STRING "" FORCE)
ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
  # ifort (untested)
  set(FORTRAN_FLAGS "-r8 -fpp -W1 ${EXTRA_FLAGS} ")
  set (CMAKE_Fortran_FLAGS "${FORTRAN_FLAGS} ${MACROS} ${INCLUDES}" CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 ${EXTRA_FLAGS} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -traceback -g -debug all -check all -ftrapuv -warn nointerfaces ${EXTRA_FLAGS} " CACHE STRING "" FORCE)
# A partir de CMake 3.1
# -prof-gen:srcpos -prof-dir${PROJECT_BINARY_DIR}

ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "XL" OR Fortran_COMPILER_NAME MATCHES "xlf*")
  # xlf (untested)
  set(FORTRAN_FLAGS "-q64 -qrealsize=8 -qsuffix=f=f90:cpp=f90 -qxlf2003=polymorphic -Wl,-dy ${EXTRA_FLAGS} ")
  set (CMAKE_Fortran_FLAGS "${FORTRAN_FLAGS} ${MACROS} ${INCLUDES}" CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -qstrict ${EXTRA_FLAGS} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -qfullpath -qkeepparm ${EXTRA_FLAGS} " CACHE STRING "" FORCE)
ELSE ()
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
ENDIF ()

SET(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -Wl,--allow-multiple-definition")
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--allow-multiple-definition")

message (STATUS "CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
message (STATUS "CMAKE_Fortran_FLAGS: " ${CMAKE_Fortran_FLAGS})
message (STATUS "CMAKE_Fortran_FLAGS_RELEASE: " ${CMAKE_Fortran_FLAGS_RELEASE})
message (STATUS "CMAKE_Fortran_FLAGS_DEBUG: " ${CMAKE_Fortran_FLAGS_DEBUG})

#################################################################
# ENABLE TESTING
#################################################################

SET(BUILDNAME ${CMAKE_Fortran_COMPILER_ID}_${CMAKE_BUILD_TYPE}_MKL=${${PROJECT_NAME}_ENABLE_MKL} CACHE STRING "" )
ENABLE_TESTING()
INCLUDE(CTest)

#################################################################
# Add external projects
#################################################################

include(ExternalProject)
SET(FOX_LIB FoX)

SET(${FOX_LIB}_SRC_PATH ${CMAKE_SOURCE_DIR}/thirdparty/fox)
SET(${FOX_LIB}_BINARY_PATH ${THIRDPARTY_OUTPUT_PATH}/${FOX_LIB})
EXTERNALPROJECT_Add(${FOX_LIB}
    DOWNLOAD_COMMAND ""
    SOURCE_DIR ${${FOX_LIB}_SRC_PATH}
    BINARY_DIR ${${FOX_LIB}_BINARY_PATH}
    INSTALL_DIR ${INSTALL_PREFIX}
    # Fortran compiler must be forced in order to link external projects from the main project
    CONFIGURE_COMMAND cmake -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS} -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -D${FOX_LIB}_ENABLE_WXML=ON -D${FOX_LIB}_ENABLE_DOM=ON -D${FOX_LIB}_ENABLE_WCML=OFF -D${FOX_LIB}_ENABLE_WKML=OFF -D${FOX_LIB}_ENABLE_EXAMPLES=OFF ${${FOX_LIB}_SRC_PATH}
    BUILD_COMMAND cmake --build .
    TEST_COMMAND ""
    INSTALL_COMMAND ""
)

# The order of the libraries reveals the right linking order
SET(FOX_LIBS ${${FOX_LIB}_BINARY_PATH}/lib/lib${FOX_LIB}_dom.a ${${FOX_LIB}_BINARY_PATH}/lib/lib${FOX_LIB}_sax.a ${${FOX_LIB}_BINARY_PATH}/lib/lib${FOX_LIB}_wxml.a ${${FOX_LIB}_BINARY_PATH}/lib/lib${FOX_LIB}_utils.a ${${FOX_LIB}_BINARY_PATH}/lib/lib${FOX_LIB}_common.a ${${FOX_LIB}_BINARY_PATH}/lib/lib${FOX_LIB}_fsys.a)
# Without the full path doesn't work properlly
#SET(FOX_LIBS ${FOX_LIB}_dom ${FOX_LIB}_sax ${FOX_LIB}_wxml ${FOX_LIB}_utils ${FOX_LIB}_common ${FOX_LIB}_fsys)

LINK_DIRECTORIES(${THIRDPARTY_OUTPUT_PATH}/${FOX_LIB}/lib)
INCLUDE_DIRECTORIES(${THIRDPARTY_OUTPUT_PATH}/${FOX_LIB}/modules)

#################################################################
# DEBUG MODE RELATED STUFF
#################################################################

IF(CMAKE_BUILD_TYPE STREQUAL "DEBUG")
    #############################################################
    # CODE INSTRUMENTATION WITH SCALASCA OR SCOREP 
    #############################################################

    IF(ENABLE_SCALASCA)

      FILE(WRITE scalasca_compiler
        "#!/bin/bash
        scalasca -instrument ${CMAKE_Fortran_COMPILER} -opt-report \"$@\"
        ")
      FILE(INSTALL scalasca_compiler
        DESTINATION ${PROJECT_BINARY_DIR}
        PERMISSIONS OWNER_READ OWNER_EXECUTE )
      FILE(REMOVE scalasca_compiler)
      SET(CMAKE_Fortran_COMPILER ${PROJECT_BINARY_DIR}/scalasca_compiler)

    ELSEIF(ENABLE_SCOREP)

      FILE(WRITE scorep_compiler
        "#!/bin/bash
         scorep ${CMAKE_Fortran_COMPILER} -opt-report \"$@\"
        ")
      FILE(INSTALL scorep_compiler
        DESTINATION ${PROJECT_BINARY_DIR}
        PERMISSIONS OWNER_READ OWNER_EXECUTE )
      FILE(REMOVE scorep_compiler)
      SET(CMAKE_Fortran_COMPILER ${PROJECT_BINARY_DIR}/scorep_compiler)

    ENDIF()
ENDIF()


#############################################################
# FLUSH DATA AFTER EvERY WRITING
#############################################################

IF(${PROJECT_NAME}_FORCE_FLUSH)
    ADD_DEFINITIONS(-DFORCE_FLUSH)
ENDIF()

#############################################################
# PRINT THE TIME EXPENDED IN EVERY IO OPERATION
#############################################################

IF(${PROJECT_NAME}_PRINT_IO_TIMES)
    ADD_DEFINITIONS(-DPRINT_IO_TIMES)
ENDIF()

#################################################################
# ADD SOURCE SUBDIRS
#################################################################

ADD_SUBDIRECTORY(${LIB_PATH})
ADD_SUBDIRECTORY(${TESTS_PATH})
ADD_SUBDIRECTORY(${EXAMPLES_PATH})

