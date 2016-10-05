program test_xdmf_uns_hyperslabs_handler

use PENF, only : I4P, I8P, R4P, R8P, str
use xh5for_parameters
use Fox_xdmf
use xdmf_unstructured_contiguous_hyperslab_handler
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

    type(mpi_env_t) :: mpienv
    type(steps_handler_t)                                               :: stepshandler
    type(unstructured_spatial_grid_descriptor_t)                        :: spatialgrid
    type(unstructured_uniform_grid_descriptor_t)                        :: uniformgrid
    type(xdmf_unstructured_contiguous_hyperslab_handler_t)              :: lightdata
    real(R4P),    dimension(8)                                          :: geometry  = (/0,0,0,1,1,1,1,0/)
    integer(I4P), dimension(4)                                          :: topology  = (/0,1,2,3/)
    real(R4P),    dimension(4)                                          :: values    = (/9,8,7,6/)
    integer         :: mpierr
    integer(I4P)    :: i

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_INIT(mpierr)
#endif

    call mpienv%initialize()
    call stepshandler%initialize(mpienv)
    call spatialgrid%initialize(MPIEnvironment=mpienv, NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY, GridType=XDMF_GRID_TYPE_UNSTRUCTURED)
    call spatialgrid%SetTopologySizePerGridID(TopologySize=int(size(topology,dim=1),I8P),ID=mpienv%get_rank())
    call uniformgrid%initialize(NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY, GridType=XDMF_GRID_TYPE_UNSTRUCTURED)
    call lightdata%initialize(MPIEnvironment=mpienv, StepsHandler=stepshandler, SpatialGridDescriptor=spatialgrid, UniformGridDescriptor=uniformgrid)
    call lightdata%Open(action=XDMF_ACTION_WRITE, fileprefix='xdmf_uns_hyperslab')
    call lightdata%SetTopology(Connectivities=topology, Name='Connectivities')
    call lightdata%SetGeometry(XYZ=geometry, Name='Coordinates')
    call lightdata%AppendAttribute(Name='solution', Center=XDMF_ATTRIBUTE_CENTER_NODE, Type=XDMF_ATTRIBUTE_TYPE_SCALAR, Attribute=values)
    call lightdata%SerializeSpatialFile()
    call lightdata%SerializeTemporalFile()

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif


end program test_xdmf_uns_hyperslabs_handler
