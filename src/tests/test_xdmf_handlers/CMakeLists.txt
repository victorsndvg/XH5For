#################################################################
# TESTS
#################################################################

FILE(GLOB_RECURSE TESTS_SRC *.f90 *.F90)
SET(TESTS_SRC ${TESTS_SRC} PARENT_SCOPE)

#################################################################
# FoX LIBRARIES
#################################################################

LINK_DIRECTORIES(${THIRDPARTY_OUTPUT_PATH}/${FOX_LIB}/lib)
INCLUDE_DIRECTORIES(${THIRDPARTY_OUTPUT_PATH}/${FOX_LIB}/modules)

#################################################################
# FORCE MPI FORTRAN COMPILER IF EXISTS
#################################################################

IF(MPI_Fortran_COMPILER AND ${PROJECT_NAME}_ENABLE_MPI)
    # Force MPI Fortran compiler
    INCLUDE(CMakeForceCompiler)
    CMAKE_FORCE_Fortran_COMPILER(${MPI_Fortran_COMPILER} ${CMAKE_Fortran_COMPILER_ID})
ENDIF()

#################################################################
# EXTERNAL LIBRARIES
#################################################################

FOREACH(TEST_SRC ${TESTS_SRC})
    GET_FILENAME_COMPONENT(EXE_NAME ${TEST_SRC} NAME_WE)
    ADD_EXECUTABLE(${EXE_NAME} ${TEST_SRC})
    TARGET_LINK_LIBRARIES(${EXE_NAME} ${LIB})
    FOREACH (EXT_LIB ${EXT_LIBS})
        IF(DEFINED ${PROJECT_NAME}_ENABLE_${EXT_LIB} AND ${PROJECT_NAME}_ENABLE_${EXT_LIB} AND ${EXT_LIB}_FOUND)
            TARGET_LINK_LIBRARIES(${EXE_NAME} ${${EXT_LIB}_LIBRARIES})
        ENDIF()
    ENDFOREACH()
    FOREACH(FOX_LIB ${FOX_LIBS})
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${FOX_LIB})
    ENDFOREACH()

    IF(MPI_Fortran_COMPILER AND ${PROJECT_NAME}_ENABLE_MPI)
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${MPI_Fortran_LIBRARIES})
    ENDIF()

    ADD_TEST(${EXE_NAME}_TEST ${EXECUTABLE_OUTPUT_PATH}/${EXE_NAME})
    IF(MPI_FOUND AND MPIEXEC AND MPIEXEC_NUMPROC_FLAG AND MPIEXEC_MAX_NUMPROCS)
        ADD_TEST(${EXE_NAME}_PARALLEL_TEST ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${EXECUTABLE_OUTPUT_PATH}/${EXE_NAME})
    ENDIF()
ENDFOREACH()



