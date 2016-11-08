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
module xdmf_domain
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Domain handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use PENF,         only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_element, only: xdmf_element_t

implicit none
private
!---------------------------------------------------------------------
! XDMFDomain properties (* Default):
! Name            (no default)
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_domain_t
    !-----------------------------------------------------------------
    !< XDMF Domain type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: Name
    contains
    private
        procedure         :: xdmf_domain_open
        procedure         :: default_initialization => xdmf_domain_default_initialization
        procedure, public :: free                   => xdmf_domain_free
        generic,   public :: open                   => xdmf_domain_open
        procedure, public :: parse                  => xdmf_domain_parse
        procedure, public :: close                  => xdmf_domain_close
        procedure, public :: print                  => xdmf_domain_print
        procedure, public :: get_Name               => xdmf_domain_get_Name
    end type xdmf_domain_t

public :: xdmf_domain_t

contains

    function xdmf_domain_get_Name(this)
    !-----------------------------------------------------------------
    !< Return the Domain Name
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(IN) :: this                   !< XDMF Domain type
        character(len=:), allocatable :: xdmf_domain_get_name      !< Domain Name
    !----------------------------------------------------------------- 
        xdmf_domain_get_name = this%Name
    end function xdmf_domain_get_Name


    subroutine xdmf_domain_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Domain type
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(INOUT) :: this                   !< XDMF Domain type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))        deallocate(this%Name)
    end subroutine xdmf_domain_free


    subroutine xdmf_domain_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF domain with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(INOUT) :: this                   !< XDMF Domain type
    !----------------------------------------------------------------- 
        call this%free()
        call this%set_tag('Domain')
    end subroutine xdmf_domain_default_initialization


    subroutine xdmf_domain_open(this, xml_handler, Name)
    !-----------------------------------------------------------------
    !< Open a new domain XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_domain_t),     intent(INOUT) :: this               !< XDMF Domain type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Domain Name attribute
    !----------------------------------------------------------------- 
        call this%set_tag('Domain')

        call xml_NewElement(xml_handler, 'Domain')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)
    end subroutine xdmf_domain_open


    subroutine xdmf_domain_parse(this, DOMNode)
    !-----------------------------------------------------------------
    !< Parse a DOM Domain into a XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_domain_t),       intent(INOUT) :: this             !< XDMF Domain type
        type(Node),       pointer,  intent(IN)    :: DOMNode          !< FoX DOM Node containig a Domain element
        character(len=:), allocatable             :: Name             !< XDMF Domain Name attribute
    !----------------------------------------------------------------- 
        call this%default_initialization()

        if(this%node_is_domain(DOMNode)) then
            if(hasAttribute(DOMNode, 'Name')) then
                this%Name = getAttribute(DOMNode, 'Name')
            endif
        endif
    end subroutine xdmf_domain_parse


    subroutine xdmf_domain_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new Domain XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(IN)    :: this                   !< XDMF Domain type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Domain')
    end subroutine xdmf_domain_close


    subroutine xdmf_domain_print(this, IndentationLevel)
    !-----------------------------------------------------------------
    !< Print on screen the Domain XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(IN)    :: this                   !< XDMF domain type
        integer(I4P), optional,  intent(IN) :: IndentationLevel       !< Indentation level
        integer(I4P)                        :: indlev = 0             !< Aux Indentation level
    !-----------------------------------------------------------------
        if(present(IndentationLevel)) indlev = IndentationLevel
        print*, repeat('  ',indlev)//'-------------------------------------------'
        print*, repeat('  ',indlev)//'DOMAIN:'
        print*, repeat('  ',indlev)//'-------------------------------------------'
        if(allocated(this%Name)) print*, repeat('  ',indlev)//'Name: '//this%Name
    end subroutine xdmf_domain_print



end module xdmf_domain
