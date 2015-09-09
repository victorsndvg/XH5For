module mpi_environment
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< MPI interface module 
!--------------------------------------------------------------------- -----------------------------------------------------------

use IR_Precision, only : I4P, I8P, R4P, R8P

#ifdef MPI_MOD
    use mpi
#endif
#ifdef MPI_H
    include 'mpif.h'
#endif

    type mpi_env_t
    !-----------------------------------------------------------------
    !< MPI environment type
    !----------------------------------------------------------------- 
        integer(I4P) :: comm = 0                                      !< MPI communicator
        integer(I4P) :: root = 0                                      !< MPI root processor
        integer(I4P) :: rank = 0                                      !< MPI rank
        integer(I4P) :: size = 1                                      !< MPI communicator size
    contains
    private
        procedure         :: mpi_allgather_single_int_value_I4P
        procedure         :: mpi_allgather_single_int_value_I8P
        procedure, public :: initialize => mpi_initialize
        procedure, public :: get_comm                     => mpi_get_comm
        procedure, public :: get_root                     => mpi_get_root
        procedure, public :: get_rank                     => mpi_get_rank
        procedure, public :: get_comm_size                => mpi_get_comm_size
        procedure, public :: is_root                      => mpi_is_root
        generic, public :: mpi_allgather_single_int_value => mpi_allgather_single_int_value_I4P, &
                                                        mpi_allgather_single_int_value_I8P
    end type mpi_env_t

public :: mpi_env_t

contains

    subroutine mpi_initialize(this, comm, root, mpierror)
    !-----------------------------------------------------------------
    !< Default MPI values initialization
    !----------------------------------------------------------------- 
        class(mpi_env_t), intent(INOUT) :: this                       !< MPI environment
        integer, optional, intent(IN)   :: comm                       !< MPI communicator
        integer, optional, intent(IN)   :: root                       !< MPI root processor
        integer, optional, intent(OUT)  :: mpierror                   !< MPI error
        logical                         :: mpi_was_initialized        !< Flag to check if MPI was initialized
        integer                         :: mpierr                     !< Aux variable for MPI error checking
    !----------------------------------------------------------------- 
        if(present(mpierror)) mpierror = 0
        this%comm = 0
        this%root = 0
        this%rank = 0
        this%size = 1

#if defined(MPI_MOD) || defined(MPI_H)
        call MPI_Initialized(mpi_was_initialized, mpierr)

        if(mpi_was_initialized) then
            if(present(comm)) then
                this%comm = comm
            else
                this%comm = MPI_COMM_WORLD
            endif
    
            call MPI_COMM_RANK (this%comm, this%rank, mpierr)
            call MPI_COMM_SIZE (this%comm, this%size, mpierr)
    
            if(present(root)) then
                this%root = root
            else
                this%root = 0
            endif
        endif
        if(present(mpierror)) mpierror = mpierr
#endif
    end subroutine mpi_initialize

    function mpi_get_comm(this)
    !-----------------------------------------------------------------
    !< Return MPI communicator
    !----------------------------------------------------------------- 
        class(mpi_env_t), intent(IN) :: this                          !< MPI environment
        integer                      :: mpi_get_comm                  !< MPI communicator
    !----------------------------------------------------------------- 
        mpi_get_comm = this%comm
    end function mpi_get_comm

    function mpi_get_root(this)
    !-----------------------------------------------------------------
    !< Return MPI root processor
    !----------------------------------------------------------------- 
        class(mpi_env_t), intent(IN) :: this                          !< MPI environment
        integer                      :: mpi_get_root                  !< MPI root processor
    !----------------------------------------------------------------- 
        mpi_get_root = this%root
    end function mpi_get_root

    function mpi_get_rank(this)
    !-----------------------------------------------------------------
    !< Return MPI rank
    !----------------------------------------------------------------- 
        class(mpi_env_t), intent(IN) :: this                          !< MPI environment
        integer                      :: mpi_get_rank                  !< MPI rank
    !----------------------------------------------------------------- 
        mpi_get_rank = this%rank
    end function mpi_get_rank

    function mpi_get_comm_size(this)
    !----------------------------------------------------------------- 
    !< Return MPI communicator size
    !----------------------------------------------------------------- 
        class(mpi_env_t), intent(IN) :: this                          !< MPI environment
        integer                      :: mpi_get_comm_size             !< MPI communicator size
    !----------------------------------------------------------------- 
        mpi_get_comm_size = this%size
    end function mpi_get_comm_size

    subroutine mpi_allgather_single_int_value_I4P(this, send_data, recv_data, mpierror)
    !-----------------------------------------------------------------
    !< MPI_allgather interface for a single I4P value per task
    !----------------------------------------------------------------- 
        class(mpi_env_t),          intent(IN)  :: this                !< MPI environment
        integer(I4P),              intent(IN)  :: send_data           !< MPI_allgather send data
        integer(I4P), allocatable, intent(OUT) :: recv_data(:)        !< MPI_allgather receive data
        integer(I4P), optional,    intent(OUT) :: mpierror            !< MPI error
        integer(I4P)                           :: mpierr              !< Aux variable for MPI error
        integer(I4P)                           :: comm_size           !< MPI communicator size aux variable
    !----------------------------------------------------------------- 
        if(present(mpierror)) mpierror = 0
        comm_size = this%get_comm_size()
        if(allocated(recv_data)) deallocate(recv_data); allocate(recv_data(comm_size))
#if defined(MPI_MOD) || defined(MPI_H)
        call MPI_ALLGATHER(send_data, 1, MPI_INTEGER, recv_data, 1, MPI_INTEGER, this%comm, mpierr) 
#else
        recv_data(:comm_size) = send_data
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_allgather_single_int_value_I4P

    subroutine mpi_allgather_single_int_value_I8P(this, send_data, recv_data, mpierror)
    !-----------------------------------------------------------------
    !< MPI_allgather interface for a single I8P value per task
    !----------------------------------------------------------------- 
        class(mpi_env_t),          intent(IN)  :: this                !< MPI environment
        integer(I8P),              intent(IN)  :: send_data           !< MPI_allgather send data
        integer(I8P), allocatable, intent(OUT) :: recv_data(:)        !< MPI_allgather receive data
        integer(I4P), optional,    intent(OUT) :: mpierror            !< MPI error
        integer(I4P)                           :: mpierr              !< Aux variable for MPI error
        integer(I4P)                           :: comm_size           !< MPI communicator size aux variable
    !----------------------------------------------------------------- 
        if(present(mpierror)) mpierror = 0
        comm_size = this%get_comm_size()
        if(allocated(recv_data)) deallocate(recv_data); allocate(recv_data(comm_size))
#if defined(MPI_MOD) || defined(MPI_H)
        call MPI_ALLGATHER(send_data, 1, MPI_LONG, recv_data, 1, MPI_LONG, this%comm, mpierr) 
#else
        recv_data(:comm_size) = send_data
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_allgather_single_int_value_I8P


    function mpi_is_root(this)
    !-----------------------------------------------------------------
    !< Is the current task the root processor?
    !----------------------------------------------------------------- 
        class(mpi_env_t), intent(IN)  :: this                         !< MPI environment
        logical                       :: mpi_is_root                  !< Boolean variable, True if is root task   
    !----------------------------------------------------------------- 
        mpi_is_root = this%get_rank() == this%get_root()
    end function mpi_is_root



end module mpi_environment
