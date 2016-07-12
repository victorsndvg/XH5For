program xh5for_dpp_regular_grid

use xh5for
use IR_Precision, only: I4P, I8P, R4P, R8P, str

#if defined(ENABLE_MPI) && defined(MPI_MOD)
  use mpi
#endif
  implicit none
#if defined(ENABLE_MPI) && defined(MPI_H)
  include 'mpif.h'
#endif

    !-----------------------------------------------------------------
    !< Variable definition
    !----------------------------------------------------------------- 
    type(xh5for_t)             :: xh5
    real(R4P),    dimension(3) :: Origin = (/0,0,0/)
    real(R4P),    dimension(3) :: DxDyDz = (/0.1,0.2,0.5/)
    integer(I4P), dimension(3) :: GridShape = (/11, 6, 3/)
    integer(I4P), allocatable  :: scalartempI4P(:)
    real(R8P),    allocatable  :: scalartempR8P(:)
    real(R4P),    allocatable  :: out_Origin(:)
    real(R4P),    allocatable  :: out_DxDyDz(:)
    integer(I4P), allocatable  :: out_scalartempI4P(:)
    real(R8P),    allocatable  :: out_scalartempR8P(:)

    integer                    :: i
    integer                    :: rank = 0
    integer                    :: mpierr
    integer                    :: exitcode = 0


    !-----------------------------------------------------------------
    !< Main program
    !----------------------------------------------------------------- 

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_INIT(mpierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr);
#endif
    !< Initialize some values depending on the mpi rank
    allocate(scalartempI4P(GridShape(1)*GridShape(2)*GridShape(3)))
    scalartempI4P(:) = (/(i,i=1,size(scalartempI4P))/)
    allocate(scalartempR8P((GridShape(1)-1)*(GridShape(2)-1)*(GridShape(3)-1)))
    scalartempR8P(:) = (/(real(i+rank),i=size(scalartempR8P),1,-1)/)
    Origin = Origin + rank

    !< Write XDMF/HDF5 file
    call xh5%Open(FilePrefix='xh5for_dpp_regular_grid', GridType=XDMF_GRID_TYPE_REGULAR, Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS, Action=XDMF_ACTION_WRITE)
    call xh5%SetGrid(GridShape = GridShape)
    call xh5%WriteGeometry(Origin=Origin, DxDyDz=DxDyDz)
    call xh5%WriteAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=scalartempI4P)
    call xh5%WriteAttribute(Name='Temperature_R8P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=scalartempR8P)
    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%Open(FilePrefix='xh5for_dpp_regular_grid', GridType=XDMF_GRID_TYPE_REGULAR, Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS, Action=XDMF_ACTION_READ)
    call xh5%ParseGrid()
    call xh5%ReadGeometry(Origin=out_Origin, DxDyDz=out_DxDyDz)
    call xh5%ReadAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=out_scalartempI4P)
    call xh5%ReadAttribute(Name='Temperature_R8P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=out_scalartempR8P)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_Origin - Origin)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_DxDyDz - DxDyDz)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_scalarTempI4P - scalarTempI4P)==0)) exitcode = -1 !I8P not supported in HDF5 layer
    if(.not. (sum(out_scalartempR8P - scalartempR8P)<=epsilon(0._R4P))) exitcode = -1
#else
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif


#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif

    call exit( status=exitcode)
end program xh5for_dpp_regular_grid
