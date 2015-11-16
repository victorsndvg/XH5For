program test_xdmf_hyperslabs_handler

use IR_Precision, only : I4P, I8P, R4P, R8P, str
use xh5for_parameters
use Fox_xdmf
use xdmf_unstructured_contiguous_hyperslab_handler
use mpi_environment
use unstructured_spatial_grid_descriptor
use unstructured_uniform_grid_descriptor
#ifdef ENABLE_MPI
#ifdef MPI_MOD
  use mpi
#else
  include 'mpif.h'
#endif
#endif

implicit none

    type(mpi_env_t) :: mpienv
    type(unstructured_spatial_grid_descriptor_t)                        :: spatialgrid
    type(unstructured_uniform_grid_descriptor_t)                        :: uniformgrid
    type(xdmf_unstructured_contiguous_hyperslab_handler_t)              :: lightdata
    real(R4P),    dimension(8)                                          :: geometry  = (/0,0,0,1,1,1,1,0/)
    integer(I4P), dimension(4)                                          :: topology  = (/0,1,2,3/)
    real(R4P),    dimension(4)                                          :: values    = (/9,8,7,6/)
    integer         :: mpierr
    integer(I4P)    :: i

#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_INIT(mpierr)
#endif
    call mpienv%initialize()
    call spatialgrid%initialize(MPIEnvironment=mpienv, NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY)
    call spatialgrid%AllgatherConnectivitySize(ConnectivitySize=int(size(topology,dim=1),I8P))
    call uniformgrid%initialize(NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY)
    call lightdata%initialize(MPIEnvironment=mpienv, SpatialGridDescriptor=spatialgrid, UniformGridDescriptor=uniformgrid)
    call lightdata%OpenFile(action=XDMF_ACTION_WRITE, fileprefix='xdmf_hyperslab')
    call lightdata%SetTopology(Connectivities=topology, Name='Connectivities')
    call lightdata%SetGeometry(Coordinates=geometry, Name='Coordinates')
    call lightdata%AppendAttribute(Name='solution', Center=XDMF_ATTRIBUTE_CENTER_NODE, Type=XDMF_ATTRIBUTE_TYPE_SCALAR, Attribute=values)
    call lightdata%Serialize()
    call lightdata%CloseFile()
#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_FINALIZE(mpierr)
#endif


end program test_xdmf_hyperslabs_handler
