program test_mpi_environment

use PENF, only : I4P, I8P, R4P, R8P, str

use mpi_environment
#ifdef MPI_MOD
  use mpi
#endif
implicit none
#ifdef MPI_H
  include 'mpif.h'
#endif


    type(mpi_env_t)               :: env
    integer                       :: mpierr
    integer(I4P),     allocatable :: recv_int(:)
    integer(I8P),     allocatable :: recv_double_int(:)
    integer(I8P)                  :: recv_single_double_int
    character(len=:), allocatable :: recv_string

#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_INIT(mpierr)
#endif

    call env%initialize()

    if(env%get_rank() == env%get_root()) then
        recv_single_double_int = 123_I8P
        recv_string = 'string from root Root!'
    endif
    

    call env%mpi_allgather(env%get_rank(), recv_int)
    call env%mpi_allgather(int(env%get_rank(),I8P), recv_double_int)
    call env%mpi_broadcast(recv_single_double_int)
    call env%mpi_broadcast(recv_string)

    if(env%get_rank() == env%get_root()) then
        print*, 'The MPI communicator has '//trim(str(no_sign=.true.,n=env%get_comm_size()))//&
                ' tasks and I am task '//trim(str(no_sign=.true., n=env%get_rank())), ' (root)'
        print*, 'Allgather task IDs in single precision integers are: ', str(no_sign=.true., n=recv_int)
        print*, 'Allgather task IDs in double precision integers are: ', str(no_sign=.true., n=recv_double_int)
    else
        print*, 'The MPI communicator has '//trim(str(no_sign=.true., n=env%get_comm_size()))//&
                ' tasks and I am task '//trim(str(no_sign=.true., n=env%get_rank()))//&
                ', received double int: '//trim(str(no_sign=.true., n=recv_single_double_int))//&
                ', received string: '//trim(recv_string)
    endif

#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_FINALIZE(mpierr)
#endif

end program test_mpi_environment

