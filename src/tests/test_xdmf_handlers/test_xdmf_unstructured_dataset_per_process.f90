program test_xdmf_uns_ddp_handler

use IR_Precision, only : I4P, I8P, R4P, R8P, str
use xh5for_parameters
use Fox_xdmf
use xdmf_unstructured_dataset_per_process_handler
use mpi_environment
use steps_handler
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

    type(mpi_env_t)                                                     :: mpienv
    type(steps_handler_t)                                               :: stepshandler
    type(unstructured_spatial_grid_descriptor_t)                        :: spatialgrid
    type(unstructured_uniform_grid_descriptor_t)                        :: uniformgrid
    type(xdmf_unstructured_dataset_per_process_handler_t)               :: lightdata
    real(R8P),    dimension(8)                                          :: geometry  = (/0,0,0,1,1,1,1,0/)
    integer(I8P), dimension(4)                                          :: topology  = (/0,1,2,3/)
    real(R8P),    dimension(4)                                          :: values    = (/9,8,7,6/)
    integer         :: mpierr
    integer(I4P)    :: i

#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_INIT(mpierr)
#endif
    call mpienv%initialize()
    call stepshandler%initialize()
    call spatialgrid%initialize(MPIEnvironment=mpienv, NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY, GridType=XDMF_GRID_TYPE_UNSTRUCTURED)
    call spatialgrid%SetTopologySizePerGridID(TopologySize=int(size(topology,dim=1),I8P),ID=mpienv%get_rank())
    call uniformgrid%initialize(NumberOfNodes=4_I8P, NumberOfElements=2_I8P, TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY, GridType=XDMF_GRID_TYPE_UNSTRUCTURED)
    call lightdata%initialize(MPIEnvironment=mpienv, StepsHandler=stepshandler, SpatialGridDescriptor=spatialgrid, UniformGridDescriptor=uniformgrid)
    call lightdata%OpenTemporalFile(action=XDMF_ACTION_WRITE, fileprefix='xdmf_unstructured_dpp')
    call lightdata%SetTopology(Connectivities=topology, Name='Connectivities')
    call lightdata%SetGeometry(XYZ=geometry, Name='Coordinates')
    call lightdata%AppendAttribute(Name='solution', Center=XDMF_ATTRIBUTE_CENTER_NODE, Type=XDMF_ATTRIBUTE_TYPE_SCALAR, Attribute=values)
    call lightdata%Serialize()
    call lightdata%CloseTemporalFile()
#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_FINALIZE(mpierr)
#endif


end program test_xdmf_uns_ddp_handler
