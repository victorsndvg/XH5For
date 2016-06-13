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

    integer                    :: rank = 0
    integer                    :: mpierr
    integer                    :: exitcode = 0

    real(R8P)                  :: time = 0.0
    integer                    :: num_steps = 5
    integer                    :: i


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

    do i=1, num_steps
        time = time + 1
        call xh5%AppendStep(Value=time)
        call xh5%WriteGeometry(X=X, Y=Y, Z=Z)
        call xh5%WriteAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=scalartempI4P+i)  
        call xh5%Serialize()
    enddo

    call xh5%Close()
    call xh5%Free()


#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif

    call exit( status=exitcode)
end program xh5for_dpp_rectilinear_grid
