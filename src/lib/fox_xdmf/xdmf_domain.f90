module xdmf_domain
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Domain handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
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
        procedure         :: domain_open
        procedure         :: default_initialization => domain_default_initialization
        procedure, public :: free                   => domain_free
        generic,   public :: open                   => domain_open
        procedure, public :: parse                  => domain_parse
        procedure, public :: close                  => domain_close
        procedure, public :: print                  => domain_print
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
        call this%set_tag('Domain')
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


    subroutine domain_parse(this, DOMNode)
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
    end subroutine Domain_parse


    subroutine domain_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new Domain XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(IN)    :: this                   !< XDMF Domain type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Domain')
    end subroutine domain_close


    subroutine domain_print(this)
    !-----------------------------------------------------------------
    !< Print on screen the Domain XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_domain_t), intent(IN)    :: this                   !< XDMF domain type
    !-----------------------------------------------------------------
        print*, '-------------------------------------------'
        print*, 'Domain:'
        print*, '-------------------------------------------'
        if(allocated(this%Name)) print*, 'Name: '//this%Name
    end subroutine domain_print



end module xdmf_domain
