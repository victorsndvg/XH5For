program test_hdf5_reg_dpp_handler

use PENF, only : I4P, I8P, R4P, R8P, str
use xh5for_parameters
use hdf5_structured_dataset_per_process_handler
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
    type(hdf5_structured_dataset_per_process_handler_t)               :: heavydata
    real(R8P),    dimension(3)                                        :: Origin  = (/0,0,0/)
    real(R8P),    dimension(3)                                        :: DxDyDz  = (/1,1,1/)
    integer(i4P), dimension(:), allocatable                           :: values  
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
    call heavydata%initialize(MPIEnvironment=mpienv, StepsHandler=stepshandler, SpatialGridDescriptor=spatialgrid, UniformGridDescriptor=uniformgrid)
    call heavydata%OpenFile(action=XDMF_ACTION_WRITE, fileprefix='hdf5_regular_dpp')
    call heavydata%WriteGeometry(Origin=Origin,DxDyDz=DxDyDz, Name='Coordinates')
    call heavydata%WriteAttribute(Name='solution', Center=XDMF_ATTRIBUTE_CENTER_NODE, Type=XDMF_ATTRIBUTE_TYPE_SCALAR, Values=values)
    call heavydata%CloseFile()

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif


end program test_hdf5_reg_dpp_handler
