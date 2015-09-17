#################################################################
# ADD TEST SOURCE SUBDIRS
#################################################################

SET(TESTS_FOX_XDMF_PATH ${TESTS_PATH}/test_FoX_xdmf)
SET(TESTS_MPI_ENV_PATH ${TESTS_PATH}/test_mpi_env)
SET(TESTS_XDMF_HANDLERS_PATH ${TESTS_PATH}/test_xdmf_handlers)
SET(TESTS_HDF5_HANDLERS_PATH ${TESTS_PATH}/test_hdf5_handlers)

ADD_SUBDIRECTORY(${TESTS_FOX_XDMF_PATH})
ADD_SUBDIRECTORY(${TESTS_MPI_ENV_PATH})
ADD_SUBDIRECTORY(${TESTS_XDMF_HANDLERS_PATH})
ADD_SUBDIRECTORY(${TESTS_HDF5_HANDLERS_PATH})
