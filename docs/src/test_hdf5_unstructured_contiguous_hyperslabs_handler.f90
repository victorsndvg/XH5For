program test_hdf5_uns_hyperslabs_handler

use PENF, only : I4P, I8P, R4P, R8P, str
use xh5for_parameters
use hdf5_unstructured_contiguous_hyperslab_handler
use mpi_environment
use steps_handler
use unstructured_spatial_grid_descriptor
use unstructured_uniform_grid_descriptor

#if defined(ENABLE_MPI) && defined(MPI_MOD)
  use mpi
#endif
  implicit none
#if defined(ENABLE_MPI) && defined(MPI_H)
  include 'mpif.h'
#endif

    type(mpi_env_t)                                                     :: mpienv
    type(steps_handler_t)                                               :: stepshandler
    type(unstructured_spatial_grid_descriptor_t)                        :: spatialgrid
    type(unstructured_uniform_grid_descriptor_t)                        :: uniformgrid
    type(hdf5_unstructured_contiguous_hyperslab_handler_t)              :: heavydata
    real(R8P), dimension(8)                                             :: trianglegeometry = (/0.0,0.0, 1.0,0.0, 0.0,1.0, 1.1,1.1/)
    integer(I4P), dimension(6)                                          :: triangletopology = (/0,1,2,1,2,3/)
    integer                                                             :: mpierr, i

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_INIT(mpierr)
#endif

    call mpienv%initialize()
    call stepshandler%initialize(mpienv)
    call spatialgrid%initialize(MPIEnvironment=mpienv, NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY, GridType=XDMF_GRID_TYPE_UNSTRUCTURED)
    call spatialgrid%SetTopologySizePerGridID(TopologySize=int(size(triangletopology,dim=1),I8P),ID=mpienv%get_rank())
    call uniformgrid%initialize(NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY, GridType=XDMF_GRID_TYPE_UNSTRUCTURED)
    call heavydata%initialize(MPIEnvironment=mpienv, StepsHandler=stepshandler, SpatialGridDescriptor=spatialgrid, UniformGridDescriptor=uniformgrid)
    call heavydata%OpenFile(action=XDMF_ACTION_WRITE, fileprefix='hdf5_uns_hyperslab')
    call heavydata%WriteTopology(triangletopology+mpienv%get_rank(), Name='Connectivities')
    call heavydata%WriteGeometry(trianglegeometry, Name='Coordinates')
    call heavydata%CloseFile()

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif


end program test_hdf5_uns_hyperslabs_handler
