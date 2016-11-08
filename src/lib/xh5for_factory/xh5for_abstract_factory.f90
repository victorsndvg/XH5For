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
module xh5for_abstract_factory

use xdmf_handler
use hdf5_handler
use uniform_grid_descriptor
use spatial_grid_descriptor

implicit none
private

    type, abstract :: xh5for_abstract_factory_t
    contains
        procedure(xh5for_factory_CreateUniformGridDescriptor), deferred :: CreateUniformGridDescriptor 
        procedure(xh5for_factory_CreateSpatialGridDescriptor), deferred :: CreateSpatialGridDescriptor 
        procedure(xh5for_factory_CreateXDMFHandler), deferred :: CreateXDMFHandler
        procedure(xh5for_factory_CreateHDF5Handler), deferred :: CreateHDF5Handler
    end type xh5for_abstract_factory_t

    abstract interface
        subroutine xh5for_factory_CreateUniformGridDescriptor(this, UniformGridDescriptor)
            import xh5for_abstract_factory_t
            import uniform_grid_descriptor_t
            class(xh5for_abstract_factory_t),                       intent(IN)  :: this
            class(uniform_grid_descriptor_t), allocatable, intent(OUT) :: UniformGridDescriptor
        end subroutine

        subroutine xh5for_factory_CreateSpatialGridDescriptor(this, SpatialGridDescriptor)
            import xh5for_abstract_factory_t
            import spatial_grid_descriptor_t
            class(xh5for_abstract_factory_t),                       intent(IN)  :: this
            class(spatial_grid_descriptor_t), allocatable, intent(OUT) :: SpatialGridDescriptor
        end subroutine

        subroutine xh5for_factory_CreateXDMFHandler(this, XDMFHandler)
            import xh5for_abstract_factory_t
            import xdmf_handler_t
            class(xh5for_abstract_factory_t),              intent(IN)  :: this
            class(xdmf_handler_t), allocatable, intent(OUT) :: XDMFHandler
        end subroutine

        subroutine xh5for_factory_CreateHDF5Handler(this, HDF5Handler)
            import xh5for_abstract_factory_t
            import hdf5_handler_t
            class(xh5for_abstract_factory_t),            intent(IN)  :: this
            class(hdf5_handler_t), allocatable, intent(OUT) :: HDF5Handler
        end subroutine
    end interface

public :: xh5for_abstract_factory_t

end module xh5for_abstract_factory
