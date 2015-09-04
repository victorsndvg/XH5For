module xdmf_domain
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Domain handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use xdmf_element, only: xdmf_element_t

implicit none
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
        procedure         :: default_initialization => domain_default_initialization
        procedure         :: free                   => domain_free
        procedure, public :: open                   => domain_open
        procedure, public :: close                  => domain_close
    end type xdmf_domain_t

contains

    subroutine domain_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Domain type
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(INOUT) :: this                   !< XDMF Domain type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))        deallocate(this%Name)
    end subroutine domain_free


    subroutine domain_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF domain with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(INOUT) :: this                   !< XDMF Domain type
    !----------------------------------------------------------------- 
        call this%free()
    end subroutine domain_default_initialization


    subroutine domain_open(this, xml_handler, Name)
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
    end subroutine domain_open


    subroutine domain_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new Domain XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(IN)    :: this                   !< XDMF Domain type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Domain')
    end subroutine domain_close


end module xdmf_domain
