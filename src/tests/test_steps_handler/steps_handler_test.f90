!-----------------------------------------------------------------
! XH5For (XDMF parallel partitioned mesh I/O on top of HDF5)
! Copyright (c) 2015 Santiago Badia, Alberto F. Martín, 
! Javier Principe and Víctor Sande.
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library.
!-----------------------------------------------------------------
program steps_handler_test

use mpi_environment
use steps_handler
use PENF, only: I4P, R4P, R8P, str

#ifdef MPI_MOD
  use mpi
#endif
implicit none
#ifdef MPI_H
  include 'mpif.h'
#endif

    type(mpi_env_t)       :: mpienv
    type(steps_handler_t) :: TimeSteps
    integer(I4P)          :: i, mpierr, NumberOfSteps

#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_INIT(mpierr)
#endif

    call mpienv%initialize()
    call TimeSteps%initialize(mpienv)

    NumberOfSteps = 12_I4P

    print*, 'Initial number of steps:', TimeSteps%GetNumberOfSteps()
    print*, 'Adding steps ... '
    do i=1, NumberOfSteps
        if(mod(i,2)==0) then
            call TimeSteps%Append(Value=real(i, R4P))
            print*, 'Number of step:', TimeSteps%GetCurrentStep(), 'with value:', TimeSteps%GetCurrentValue()
        else
            call TimeSteps%Append(Value=real(i, R8P))
            print*, 'Number of step:', TimeSteps%GetCurrentStep(), 'with value:', TimeSteps%GetStepValue(StepNumber=TimeSteps%GetCurrentStep())
        endif
    enddo

    print*, 'Total number of steps:', TimeSteps%GetNumberOfSteps()
    print*, ''
    print*, 'Iterate over steps:', TimeSteps%GetNumberOfSteps()
    call TimeSteps%Begin()
    do while (.not. TimeSteps%Hasfinished())
        print*, 'Number of step:', TimeSteps%GetCurrentStep(), 'with value:', TimeSteps%GetCurrentValue()
        call TimeSteps%Next()
    enddo
    print*, 'Freeing ... '
    call TimeSteps%Free()

    call TimeSteps%initialize(mpienv)
    print*, ''
    print*, 'Initial number of steps:', TimeSteps%GetNumberOfSteps()
    print*, 'Adding steps ... '
    do i=NumberOfSteps, 1, -1
        if(mod(i,2)==0) then
            call TimeSteps%Append(Filename='Filename_'//trim(adjustl(str(no_sign=.true., n=i))))
            call TimeSteps%SetCurrentValue(Value=real(i, R8P))
            print*, 'Number of step:', TimeSteps%GetCurrentStep(), 'with value:', TimeSteps%GetCurrentValue(), 'in file: ', TimeSteps%GetStepFilename(StepNumber=TimeSteps%GetCurrentStep())

        else
            call TimeSteps%Append(Value=real(i, R8P))
            print*, 'Number of step:', TimeSteps%GetCurrentStep(), 'with value:', TimeSteps%GetStepValue(StepNumber=TimeSteps%GetCurrentStep())
        endif
    enddo

    print*, 'Total number of steps:', TimeSteps%GetNumberOfSteps()
    print*, 'Freeing ... '
    call TimeSteps%Free()
    call mpienv%Free()

#if defined(MPI_MOD) || defined(MPI_H)
    call MPI_FINALIZE(mpierr)
#endif


end program steps_handler_test
