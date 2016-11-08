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
program xh5for_ch_rectilinear_grid

use xh5for
use PENF, only: I4P, I8P, R4P, R8P, str

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
    real(R8P),    dimension(6) :: X = (/0.0, 0.2, 0.4, 0.6, 0.8, 1.0/)
    real(R8P),    dimension(4) :: Y = (/0.0, 0.33, 0.66, 1.0/)
    real(R8P),    dimension(3) :: Z = (/0.0, 0.5, 1.0/)
    integer(I4P), allocatable  :: scalartempI4P(:)
    real(R8P),    allocatable  :: out_X(:)
    real(R8P),    allocatable  :: out_Y(:)
    real(R8P),    allocatable  :: out_Z(:)
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

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_INIT(mpierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr);
#endif
    !< Initialize some values depending on the mpi rank
    allocate(scalartempI4P((size(X)-1)*(size(Y)-1)*(size(Z)-1)))
    scalartempI4P(:) = (/(i,i=1,size(scalartempI4P))/)
    X = X + rank
    Y = Y + rank
    Z = Z + rank

    !< Write XDMF/HDF5 file.    
    call xh5%Open(FilePrefix='xh5for_ch_rectilinear_non_static_grid_series', Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB, GridType=XDMF_GRID_TYPE_RECTILINEAR, Action=XDMF_ACTION_WRITE)

    do i=1, num_steps
        call xh5%AppendStep(Value=time+i)
        call xh5%SetGrid(GridShape=(/size(X), size(Y), size(Z)/))
        call xh5%WriteGeometry(X=X, Y=Y, Z=Z)
        call xh5%WriteAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=scalartempI4P+i)  
    enddo

    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%Open(FilePrefix='xh5for_ch_rectilinear_non_static_grid_series', Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB, GridType=XDMF_GRID_TYPE_RECTILINEAR, Action=XDMF_ACTION_READ)

    do i=1, xh5%GetNumberOfSteps()
        call xh5%ParseGrid()
        call xh5%ReadGeometry(X=out_X, Y=out_Y, Z=out_Z)
        call xh5%ReadAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=out_scalartempI4P)
        call xh5%NextStep()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_X - X)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_Y - Y)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_Z - Z)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_scalartempI4P - (scalartempI4P+i))==0._I4P)) exitcode = -1
#endif
    enddo

    call xh5%Close()
    call xh5%Free()


#ifndef ENABLE_HDF5
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif


#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif

    call exit( status=exitcode)
end program xh5for_ch_rectilinear_grid
