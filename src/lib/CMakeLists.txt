#################################################################
# Search F90 files recursively in all subdirs
#################################################################

FILE(GLOB_RECURSE LIB_SRC *.f90 *.F90 *.c)
SET(LIB_SRC ${LIB_SRC} PARENT_SCOPE)

#################################################################
# Library target
#################################################################
ADD_LIBRARY(${LIB} ${LIB_SRC})

#################################################################
# External libraries
#################################################################
FOREACH(EXT_LIB ${EXT_LIBS})
    IF (${EXT_LIB}_FOUND)
        TARGET_LINK_LIBRARIES(${LIB} ${${EXT_LIB}_LIBRARIES})
    ENDIF()
ENDFOREACH()

#################################################################
# MPI library
#################################################################
IF(MPI_Fortran_COMPILER AND ${PROJECT_NAME}_ENABLE_MPI)
    TARGET_LINK_LIBRARIES(${LIB} ${MPI_Fortran_LIBRARIES})
ENDIF()

#################################################################
# External projects
#################################################################
FOREACH(FOX_LIB ${FOX_LIBS})
        TARGET_LINK_LIBRARIES(${LIB} ${FOX_LIB})
ENDFOREACH()

SET_TARGET_PROPERTIES(${LIB} PROPERTIES VERSION ${${LIB}_VERSION} SOVERSION ${${LIB}_SOVERSION})
EXPORT(TARGETS ${LIB} APPEND FILE ${PROJECT_BINARY_DIR}/${LIB}Targets.cmake)

