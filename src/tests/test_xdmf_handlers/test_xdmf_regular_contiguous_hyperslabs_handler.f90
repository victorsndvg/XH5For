program test_xdmf_reg_hyperslabs_handler

use IR_Precision, only : I4P, I8P, R4P, R8P, str
use xh5for_parameters
use Fox_xdmf
use xdmf_structured_contiguous_hyperslab_handler
use mpi_environment
use steps_handler
use structured_spatial_grid_descriptor
use structured_uniform_grid_descriptor

#if defined(ENABLE_MPI) && defined(MPI_MOD)
  use mpi
#endif
  implicit none
#if defined(ENABLE_MPI) && defined(MPI_H)
  include 'mpif.h'
#endif

    type(mpi_env_t)                                                   :: mpienv
    type(steps_handler_t)                                             :: stepshandler
    type(structured_spatial_grid_descriptor_t)                        :: spatialgrid
    type(structured_uniform_grid_descriptor_t)                        :: uniformgrid
    type(xdmf_structured_contiguous_hyperslab_handler_t)              :: lightdata
    real(R4P),    dimension(3)                                        :: Origin  = (/0,0,0/)
    real(R4P),    dimension(3)                                        :: DxDyDz  = (/1,1,1/)
    real(R4P),    dimension(:), allocatable                           :: values  
    integer                                                           :: mpierr
    integer                                                           :: i


#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_INIT(mpierr)
#endif

    values = (/(i,i=0,10*20*30)/)

    call mpienv%initialize()
    call stepshandler%initialize(mpienv)
    call spatialgrid%initialize(MPIEnvironment=mpienv, XDim=10_I8P, YDim=20_I8P, ZDim=30_I8P, GridType=XDMF_GRID_TYPE_REGULAR)
    call uniformgrid%initialize(XDim=10_I8P, YDim=20_I8P, ZDim=30_I8P, GridType=XDMF_GRID_TYPE_REGULAR)
    call lightdata%initialize(MPIEnvironment=mpienv, StepsHandler=stepshandler, SpatialGridDescriptor=spatialgrid, UniformGridDescriptor=uniformgrid)
    call lightdata%Open(action=XDMF_ACTION_WRITE, fileprefix='xdmf_regular_hyperslab')
    call lightdata%SetGeometry(XYZ=Origin, Name='Coordinates')
    call lightdata%AppendAttribute(Name='solution', Center=XDMF_ATTRIBUTE_CENTER_NODE, Type=XDMF_ATTRIBUTE_TYPE_SCALAR, Attribute=values)
    call lightdata%SerializeSpatialFile()
    call lightdata%SerializeTemporalFile()

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif


end program test_xdmf_reg_hyperslabs_handler
