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
program test_xdmf_rect_dpp_handler

use PENF, only : I4P, I8P, R4P, R8P, str
use xh5for_parameters
use Fox_xdmf
use xdmf_structured_dataset_per_process_handler
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
    type(xdmf_structured_dataset_per_process_handler_t)               :: lightdata
    real(R8P),    dimension(3)                                        :: Xpoints  = (/1,2,3/)
    real(R8P),    dimension(4)                                        :: Ypoints  = (/2,3,4,5/)
    real(R8P),    dimension(5)                                        :: Zpoints  = (/3,4,5,6,7/)
    real(R8P),    dimension(:), allocatable                           :: values  
    integer                                                           :: mpierr
    integer                                                           :: i


#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_INIT(mpierr)
#endif

    values = (/(i,i=0,size(Xpoints)*size(Ypoints)*size(Zpoints))/)

    call mpienv%initialize()
    call stepshandler%initialize(mpienv)
    call spatialgrid%initialize(MPIEnvironment=mpienv, XDim=int(size(Xpoints),I8P), YDim=int(size(Ypoints),I8P), ZDim=int(size(Zpoints),I8P), GridType=XDMF_GRID_TYPE_RECTILINEAR)
    call uniformgrid%initialize(XDim=int(size(Xpoints),I8P), YDim=int(size(Ypoints),I8P), ZDim=int(size(Zpoints),I8P), GridType=XDMF_GRID_TYPE_RECTILINEAR)
    call lightdata%initialize(MPIEnvironment=mpienv, StepsHandler=stepshandler, SpatialGridDescriptor=spatialgrid, UniformGridDescriptor=uniformgrid)
    call lightdata%Open(action=XDMF_ACTION_WRITE, fileprefix='xdmf_rectilinear_ddp')
    call lightdata%SetGeometry(XYZ=Xpoints, Name='Coordinates')
    call lightdata%AppendAttribute(Name='solution', Center=XDMF_ATTRIBUTE_CENTER_NODE, Type=XDMF_ATTRIBUTE_TYPE_SCALAR, Attribute=values)
    call lightdata%SerializeSpatialFile()
    call lightdata%SerializeTemporalFile()

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif


end program test_xdmf_rect_dpp_handler
