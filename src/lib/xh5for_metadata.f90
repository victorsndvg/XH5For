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
module xh5for_metadata

use PENF, only: I4P, I8P
use xh5for_parameters

implicit none
private

    type :: xh5for_metadata_t
    private
        character(len=:), allocatable :: Name
        integer(I4P)                  :: Type             = XDMF_NO_VALUE
        integer(I4P)                  :: Center           = XDMF_NO_VALUE
        character(len=:), allocatable :: DataType
        integer(I4P)                  :: Precision        = XDMF_NO_VALUE
        integer(I8P),     allocatable :: ArrayDimensions(:)
    contains
        procedure, public :: SetName            => xh5for_metadata_SetName
        procedure, public :: SetType            => xh5for_metadata_SetType
        procedure, public :: SetCenter          => xh5for_metadata_SetCenter
        procedure, public :: SetDataType        => xh5for_metadata_SetDataType
        procedure, public :: SetPrecision       => xh5for_metadata_SetPrecision
        procedure, public :: SetArrayDimensions => xh5for_metadata_SetArrayDimensions
        procedure, public :: GetName            => xh5for_metadata_GetName
        procedure, public :: GetType            => xh5for_metadata_GetType
        procedure, public :: GetCenter          => xh5for_metadata_GetCenter
        procedure, public :: GetDataType        => xh5for_metadata_GetDataType
        procedure, public :: GetPrecision       => xh5for_metadata_GetPrecision
        procedure, public :: GetArrayDimensions => xh5for_metadata_GetArrayDimensions
        procedure, public :: Free               => xh5for_metadata_Free
    end type xh5for_metadata_t

public :: xh5for_metadata_t

contains

    subroutine xh5for_metadata_SetName(this, Name)
    !-----------------------------------------------------------------
    !< Set the Name
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        character(len=*),         intent(IN)    :: Name               !< DataSet Name
    !----------------------------------------------------------------- 
        this%Name = Name
    end subroutine xh5for_metadata_setName


    subroutine xh5for_metadata_SetType(this, Type)
    !-----------------------------------------------------------------
    !< Set the Type
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        integer(I4P),             intent(IN)    :: Type               !< Type
    !----------------------------------------------------------------- 
        this%Type = Type
    end subroutine xh5for_metadata_setType


    subroutine xh5for_metadata_SetCenter(this, Center)
    !-----------------------------------------------------------------
    !< Set the Center
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        integer(I4P),             intent(IN)    :: Center             !< Center
    !----------------------------------------------------------------- 
        this%Center = Center
    end subroutine xh5for_metadata_setCenter


    subroutine xh5for_metadata_SetDataType(this, DataType)
    !-----------------------------------------------------------------
    !< Set the DataType
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        character(len=*),         intent(IN)    :: DataType           !< DataType
    !----------------------------------------------------------------- 
        this%DataType = DataType
    end subroutine xh5for_metadata_setDataType


    subroutine xh5for_metadata_SetPrecision(this, Precision)
    !-----------------------------------------------------------------
    !< Set the Precision
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        integer(I4P),             intent(IN)    :: Precision          !< Precision
    !----------------------------------------------------------------- 
        this%Precision = Precision
    end subroutine xh5for_metadata_setPrecision


    subroutine xh5for_metadata_SetArrayDimensions(this, ArrayDimensions)
    !-----------------------------------------------------------------
    !< Set the Dimension
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        integer(I8P),             intent(IN)    :: ArrayDimensions(:) !< Array Dimensions
    !----------------------------------------------------------------- 
        if(allocated(this%ArrayDimensions)) deallocate(this%ArrayDimensions)
        allocate(this%ArrayDimensions(size(ArrayDimensions, dim=1, kind=I4P)))
        this%ArrayDimensions(:) = ArrayDimensions(:)
    end subroutine xh5for_metadata_SetArrayDimensions


    function xh5for_metadata_GetName(this)
    !-----------------------------------------------------------------
    !< Return the Name
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                   !< XH5For metadata type
        character(len=:), allocatable :: xh5for_metadata_GetName       !< Returned Name
    !----------------------------------------------------------------- 
        xh5for_metadata_GetName = this%Name
    end function xh5for_metadata_GetName


    function xh5for_metadata_GetType(this)
    !-----------------------------------------------------------------
    !< Return the Type
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                  !< XH5For metadata type
        integer(I4P) :: xh5for_metadata_GetType                       !< Returned Type
    !----------------------------------------------------------------- 
        xh5for_metadata_GetType = this%Type
    end function xh5for_metadata_GetType


    function xh5for_metadata_GetCenter(this)
    !-----------------------------------------------------------------
    !< Return the Center
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                  !< XH5For metadata type
        integer(I4P) :: xh5for_metadata_GetCenter                     !< Returned Center
    !----------------------------------------------------------------- 
        xh5for_metadata_GetCenter = this%Center
    end function xh5for_metadata_GetCenter


    function xh5for_metadata_GetDataType(this)
    !-----------------------------------------------------------------
    !< Return the DataType
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                   !< XH5For metadata type
        character(len=:), allocatable :: xh5for_metadata_GetDataType   !< Returned DataType
    !----------------------------------------------------------------- 
        xh5for_metadata_GetDataType = this%DataType
    end function xh5for_metadata_GetDataType


    function xh5for_metadata_GetPrecision(this)
    !-----------------------------------------------------------------
    !< Return the Precision
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                  !< XH5For metadata type
        integer(I4P) :: xh5for_metadata_GetPrecision                  !< Returned Precision
    !----------------------------------------------------------------- 
        xh5for_metadata_GetPrecision = this%Precision
    end function xh5for_metadata_GetPrecision


    subroutine xh5for_metadata_GetArrayDimensions(this, ArrayDimensions)
    !-----------------------------------------------------------------
    !< Return the Dimension
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t),  intent(IN)    :: this              !< XH5For metadata type
        integer(I4P), allocatable, intent(INOUT) :: ArrayDimensions(:)!< Returned Dimension
    !----------------------------------------------------------------- 
        if(allocated(this%arrayDimensions)) then
            if(allocated(ArrayDimensions)) deallocate(ArrayDimensions)
            allocate(ArrayDimensions(size(this%ArrayDimensions, dim=1)))
            ArrayDimensions(:) = this%ArrayDimensions(:)
        endif
    end subroutine xh5for_metadata_GetArrayDimensions


    subroutine xh5for_metadata_Free(this)
    !-----------------------------------------------------------------
    !< Free XH5For Metadata
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
    !----------------------------------------------------------------- 
        if(allocated(this%Name)) deallocate(this%Name)
        if(allocated(this%DataType)) deallocate(this%DataType)
        if(allocated(this%ArrayDimensions)) deallocate(this%ArrayDimensions)
        this%Type       = XDMF_NO_VALUE
        this%Center     = XDMF_NO_VALUE
        this%Precision  = XDMF_NO_VALUE
    end subroutine



end module xh5for_metadata
