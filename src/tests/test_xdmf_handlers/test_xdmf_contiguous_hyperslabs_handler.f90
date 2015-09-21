program test_xdmf_hyperslabs_handler

use IR_Precision, only : I4P, I8P, R4P, R8P, str
use xh5for_parameters
use Fox_xdmf
use xdmf_contiguous_hyperslab_handler
use mpi_environment
use spatial_grid_descriptor
use uniform_grid_descriptor
#ifdef ENABLE_MPI
#ifdef MPI_MOD
  use mpi
#else
  include 'mpif.h'
#endif
#endif

implicit none

    type(mpi_env_t) :: mpienv
    type(spatial_grid_descriptor_t) :: spatialgrid
    type(uniform_grid_descriptor_t) :: uniformgrid
    type(xdmf_contiguous_hyperslab_handler_t) :: lightdata
    integer         :: mpierr
    integer(I4P)    :: i

#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_INIT(mpierr)
#endif
    call mpienv%initialize()
    call spatialgrid%initialize(MPIEnvironment=mpienv, NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY)
    call uniformgrid%initialize(NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY)
    call lightdata%initialize(MPIEnvironment=mpienv, SpatialGridDescriptor=spatialgrid, UniformGridDescriptor=uniformgrid)
    call lightdata%OpenFile('hyperslab.xmf')
    do i=0, mpienv%get_comm_size()-1
        call lightdata%OpenGrid(GridID=i)
        call lightdata%WriteTopology(GridID=i)
        call lightdata%WriteGeometry(GridID=i)
        call lightdata%CloseGrid()
!        call lightdata%WriteAttribute(Name='solution', Center=XDMF_ATTRIBUTE_CENTER_NODE, Type=XDMF_ATTRIBUTE_TYPE_SCALAR, GridID=i)
    enddo
    call lightdata%CloseFile()
#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_FINALIZE(mpierr)
#endif


end program test_xdmf_hyperslabs_handler
