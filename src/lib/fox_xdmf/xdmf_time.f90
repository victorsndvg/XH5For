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
module xdmf_time
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use PENF,         only: I4P, R4P, R8P, str, cton
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_element, only: xdmf_element_t
use xdmf_utils

implicit none
private
!---------------------------------------------------------------------
! XDMFTime properties (* Default):
! TimeType            Single | HyperSlab | List | Range
! Value               (no default - Only valid for TimeType="Single")
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_time_t
    !-----------------------------------------------------------------
    !< XDMF Time type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: TimeType
        real(R8P)                     :: Value
    contains
    private
        procedure         :: default_initialization => xdmf_time_default_initialization
        procedure, public :: free                   => xdmf_time_free
        procedure         :: xdmf_time_open_timetype
        procedure         :: xdmf_time_open_R4P_value
        procedure         :: xdmf_time_open_R8P_value
        generic,   public :: open                   => xdmf_time_open_R4P_value, &
                                                       xdmf_time_open_R8P_value, &
                                                       xdmf_time_open_timetype
        procedure, public :: parse                  => xdmf_time_parse
        procedure, public :: close                  => xdmf_time_close
        procedure, public :: print                  => xdmf_time_print
    end type xdmf_time_t

public :: xdmf_time_t

contains

    function xdmf_time_get_TimeType(this)
    !-----------------------------------------------------------------
    !< Return the Time TimeType
    !----------------------------------------------------------------- 
        class(xdmf_time_t), intent(IN) :: this                        !< XDMF Time type
        character(len=:), allocatable :: xdmf_time_get_TimeType       !< Time TimeType
    !----------------------------------------------------------------- 
        xdmf_time_get_TimeType = this%TimeType
    end function xdmf_time_get_TimeType


    function xdmf_time_get_Value(this)
    !-----------------------------------------------------------------
    !< Return the Time Value
    !----------------------------------------------------------------- 
        class(xdmf_time_t), intent(IN) :: this                        !< XDMF Time type
        integer(R8P) :: xdmf_time_get_Value                           !< Time Value
    !----------------------------------------------------------------- 
        xdmf_time_get_Value = this%Value
    end function xdmf_time_get_Value


    subroutine xdmf_time_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Time type
    !----------------------------------------------------------------- 
        class(xdmf_time_t), intent(INOUT) :: this                 !< XDMF Time type
    !----------------------------------------------------------------- 
        if(allocated(this%TimeType))   deallocate(this%TimeType)
    end subroutine xdmf_time_free


    subroutine xdmf_time_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF time with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_time_t), intent(INOUT) :: this                 !< XDMF Time type
    !----------------------------------------------------------------- 
        call this%free()
        this%TimeType = ''
        this%Value = 0._R8P
    end subroutine xdmf_time_default_initialization

    subroutine xdmf_time_open_timetype(this, xml_handler, TimeType)
    !-----------------------------------------------------------------
    !< Open a new time XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_time_t),         intent(INOUT) :: this             !< XDMF Time type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: TimeType         !< XDMF Time TimeType attribute
    !----------------------------------------------------------------- 
        call this%set_tag('Time')

        call xml_NewElement(xml_handler, 'Time')
        if(PRESENT(TimeType)) then; if(isSupportedTimeTypeName(TimeType)) &
            call xml_AddAttribute(xml_handler, name="TimeType", value=TimeType)
        endif
    end subroutine xdmf_time_open_timetype

    subroutine xdmf_time_open_R4P_value(this, xml_handler, TimeType, Value)
    !-----------------------------------------------------------------
    !< Open a new time XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_time_t),         intent(INOUT) :: this                 !< XDMF Time type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: TimeType         !< XDMF Time TimeType attribute
        real(R4P),                  intent(IN)    :: Value            !< XDMF Time Value attribute
    !----------------------------------------------------------------- 
        call this%set_tag('Time')

        call xml_NewElement(xml_handler, 'Time')
        if(PRESENT(TimeType)) then; if(isSupportedTimeTypeName(TimeType)) &
            call xml_AddAttribute(xml_handler, name="TimeType", value=TimeType)
        endif

        call xml_AddAttribute(xml_handler, name="Value", value=Value)
    end subroutine xdmf_time_open_R4P_value

    subroutine xdmf_time_open_R8P_value(this, xml_handler, TimeType, Value)
    !-----------------------------------------------------------------
    !< Open a new time XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_time_t),         intent(INOUT) :: this                 !< XDMF Time type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: TimeType         !< XDMF Time TimeType attribute
        real(R8P),                  intent(IN)    :: Value            !< XDMF Time Value attribute
    !----------------------------------------------------------------- 
        call this%set_tag('Time')

        call xml_NewElement(xml_handler, 'Time')
        if(PRESENT(TimeType)) then; if(isSupportedTimeTypeName(TimeType)) &
            call xml_AddAttribute(xml_handler, name="TimeType", value=TimeType)
        endif

        call xml_AddAttribute(xml_handler, name="Value", value=Value)
    end subroutine xdmf_time_open_R8P_value


    subroutine xdmf_time_parse(this, DOMNode)
    !-----------------------------------------------------------------
    !< Parse a DOM time into a XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_time_t),         intent(INOUT) :: this             !< XDMF Time type
        type(Node),       pointer,  intent(IN)    :: DOMNode          !< FoX DOM Node containig a Time element
        character(len=:), allocatable             :: TimeType         !< XDMF Time TimeType attribute
        real(R8P)                                 :: Value            !< XDMF Time Value attribute
    !----------------------------------------------------------------- 
        call this%default_initialization()

        if(this%node_is_time(DOMNode)) then

            if(hasAttribute(DOMNode, 'TimeType')) then
                TimeType = getAttribute(DOMNode, 'TimeType')
                if(isSupportedTimeTypeName(TimeType)) this%TimeType = TimeType
            endif

            if(hasAttribute(DOMNode, 'Value')) then
                this%Value = cton(str=getAttribute(DOMNode, 'Value'),knd=0._R8P)
            endif
        endif
    end subroutine xdmf_time_parse


    subroutine xdmf_time_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new Time XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_time_t), intent(IN)    :: this                 !< XDMF Time type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Time')
    end subroutine xdmf_time_close

    subroutine xdmf_time_print(this, IndentationLevel)
    !-----------------------------------------------------------------
    !< Print on screen the Time XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_time_t),      intent(IN)    :: this                !< XDMF Time type
        integer(I4P), optional,  intent(IN)    :: IndentationLevel    !< Indentation level
        integer(I4P)                           :: indlev = 0          !< Aux Indentation level
    !-----------------------------------------------------------------
        if(present(IndentationLevel)) indlev = IndentationLevel
        print*, repeat('  ',indlev)//'-------------------------------------------'
        print*, repeat('  ',indlev)//'TIME:'
        print*, repeat('  ',indlev)//'-------------------------------------------'
        if(allocated(this%TimeType)) print*, repeat('  ',indlev)//'TimeType: '//this%TimeType
        print*, repeat('  ',indlev)//'Value: '//str(no_sign=.true.,n=this%Value)
    end subroutine xdmf_time_print


end module xdmf_time
