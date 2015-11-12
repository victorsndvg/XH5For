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
# FORCE HDF5 FORTRAN COMPILER IF EXISTS
#################################################################

IF(HDF5_Fortran_COMPILER_EXECUTABLE AND ${PROJECT_NAME}_ENABLE_HDF5)
    # Force HDF5 Fortran compiler
    INCLUDE(CMakeForceCompiler)
    CMAKE_FORCE_Fortran_COMPILER(${HDF5_Fortran_COMPILER_EXECUTABLE} ${CMAKE_Fortran_COMPILER_ID})
ENDIF()

#################################################################
# EXTERNAL LIBRARIES
#################################################################

FOREACH(TEST_SRC ${TESTS_SRC})
    GET_FILENAME_COMPONENT(EXE_NAME ${TEST_SRC} NAME_WE)
    ADD_EXECUTABLE(${EXE_NAME} ${TEST_SRC})
    TARGET_LINK_LIBRARIES(${EXE_NAME} ${LIB})

    FOREACH(FOX_LIB ${FOX_LIBS})
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${FOX_LIB})
    ENDFOREACH()

    IF(MPI_Fortran_COMPILER AND ${PROJECT_NAME}_ENABLE_MPI)
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${MPI_Fortran_LIBRARIES})
    ENDIF()

    IF(NOT DEFINED HDF5_Fortran_COMPILER_EXECUTABLE AND ${PROJECT_NAME}_ENABLE_HDF5)
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${HDF5_Fortran_HL_LIBRARIES})
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${HDF5_HL_LIBRARIES})
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${HDF5_Fortran_LIBRARIES})
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${HDF5_LIBRARIES})
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${HDF5_dl_LIBRARY_RELEASE})
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${HDF5_m_LIBRARY_RELEASE})
    ENDIF()

    ADD_TEST(${EXE_NAME}_TEST ${EXECUTABLE_OUTPUT_PATH}/${EXE_NAME})
ENDFOREACH()



