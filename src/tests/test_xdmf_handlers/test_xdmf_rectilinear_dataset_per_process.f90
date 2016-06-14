program test_xdmf_rect_dpp_handler

use IR_Precision, only : I4P, I8P, R4P, R8P, str
use xh5for_parameters
use Fox_xdmf
use xdmf_structured_dataset_per_process_handler
use mpi_environment
use steps_handler
use structured_spatial_grid_descriptor
use structured_uniform_grid_descriptor
#ifdef ENABLE_MPI
#ifdef MPI_MOD
  use mpi
#else
  include 'mpif.h'
#endif
#endif

implicit none

    type(mpi_env_t)                                                   :: mpienv
    type(steps_handler_t)                                             :: stepshandler
    type(structured_spatial_grid_descriptor_t)                        :: spatialgrid
    type(structured_uniform_grid_descriptor_t)                        :: uniformgrid
    type(xdmf_structured_dataset_per_process_handler_t)               :: lightdata
    real(R8P),    dimension(3)                                        :: Xpoints  = (/1,2,3/)
    real(R8P),    dimension(4)                                        :: Ypoints  = (/2,3,4,5/)
    real(R8P),    dimension(5)                                        :: Zpoints  = (/3,4,5,6,7/)
    real(R8P),    dimension(:), allocatable                           :: values  
    integer                                                           :: mpierr
    integer                                                           :: i


#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_INIT(mpierr)
#endif

    values = (/(i,i=0,size(Xpoints)*size(Ypoints)*size(Zpoints))/)

    call mpienv%initialize()
    call stepshandler%initialize(mpienv)
    call spatialgrid%initialize(MPIEnvironment=mpienv, XDim=int(size(Xpoints),I8P), YDim=int(size(Ypoints),I8P), ZDim=int(size(Zpoints),I8P), GridType=XDMF_GRID_TYPE_RECTILINEAR)
    call uniformgrid%initialize(XDim=int(size(Xpoints),I8P), YDim=int(size(Ypoints),I8P), ZDim=int(size(Zpoints),I8P), GridType=XDMF_GRID_TYPE_RECTILINEAR)
    call lightdata%initialize(MPIEnvironment=mpienv, StepsHandler=stepshandler, SpatialGridDescriptor=spatialgrid, UniformGridDescriptor=uniformgrid)
    call lightdata%OpenTemporalFile(action=XDMF_ACTION_WRITE, fileprefix='xdmf_rectilinear_ddp')
    call lightdata%SetGeometry(XYZ=Xpoints, Name='Coordinates')
    call lightdata%AppendAttribute(Name='solution', Center=XDMF_ATTRIBUTE_CENTER_NODE, Type=XDMF_ATTRIBUTE_TYPE_SCALAR, Attribute=values)
    call lightdata%Serialize()
    call lightdata%CloseTemporalFile()
#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_FINALIZE(mpierr)
#endif


end program test_xdmf_rect_dpp_handler
