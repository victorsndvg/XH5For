program xh5for_dpp_rectilinear_grid

use xh5for
#ifdef ENABLE_MPI
#ifdef MPI_MOD
  use mpi
#else
  include 'mpif.h'
#endif
#endif

implicit none
    !-----------------------------------------------------------------
    !< Variable definition
    !----------------------------------------------------------------- 
    type(xh5for_t)             :: xh5
    real(R4P),    dimension(6) :: X = (/0.0, 0.2, 0.4, 0.6, 0.8, 1.0/)
    real(R4P),    dimension(4) :: Y = (/0.0, 0.33, 0.66, 1.0/)
    real(R4P),    dimension(3) :: Z = (/0.0, 0.5, 1.0/)
    integer(I4P), allocatable  :: scalartempI4P(:)
    real(R4P),    allocatable  :: out_X(:)
    real(R4P),    allocatable  :: out_Y(:)
    real(R4P),    allocatable  :: out_Z(:)
    integer(I4P), allocatable  :: out_scalartempI4P(:)
!    integer(I8P), allocatable  :: out_scalartempI8P(:)

    integer                    :: i
    integer                    :: rank = 0
    integer                    :: mpierr
    integer                    :: exitcode = 0


    !-----------------------------------------------------------------
    !< Main program
    !----------------------------------------------------------------- 

#ifdef ENABLE_MPI
    call MPI_INIT(mpierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr);
#endif
    !< Initialize some values depending on the mpi rank
    allocate(scalartempI4P((size(X)-1)*(size(Y)-1)*(size(Z)-1)))
    scalartempI4P(:) = (/(i,i=1,size(scalartempI4P))/)
    X = X + rank
    Y = Y + rank
    Z = Z + rank

    !< Write XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS)
    call xh5%SetGridType(GridType=XDMF_GRID_TYPE_RECTILINEAR)
    call xh5%Initialize(GridShape=(/size(X), size(Y), size(Z)/))
    call xh5%Open(action=XDMF_ACTION_WRITE , fileprefix='xh5for_dpp_rectilinear_grid')
    call xh5%WriteGeometry(X=X, Y=Y, Z=Z)
    call xh5%WriteAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=scalartempI4P)  
    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS)
    call xh5%Initialize()
    call xh5%Open(action=XDMF_ACTION_READ, fileprefix='xh5for_dpp_rectilinear_grid')
    call xh5%Parse()
    call xh5%ReadGeometry(X=out_X, Y=out_Y, Z=out_Z)
    call xh5%ReadAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=out_scalartempI4P)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_X - X)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_Y - Y)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_Z - Z)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_scalartempI4P - scalartempI4P)==0._I4P)) exitcode = -1
#else
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif

#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif

    call exit( status=exitcode)
end program xh5for_dpp_rectilinear_grid
