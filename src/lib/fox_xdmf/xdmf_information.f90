module xdmf_information
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Information handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_element, only: xdmf_element_t

implicit none
!---------------------------------------------------------------------
! XDMFInformation properties (* Default):
! Name            (no default)
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_information_t
    !-----------------------------------------------------------------
    !< XDMF Information type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: Name
        character(len=:), allocatable :: Value
    contains
    private
        procedure         :: information_open
        procedure         :: default_initialization => information_default_initialization
        procedure, public :: free                   => information_free
        generic,   public :: open                   => information_open
        procedure, public :: parse                  => information_parse
        procedure, public :: close                  => information_close
        procedure, public :: print                  => information_print
    end type xdmf_information_t

contains

    subroutine information_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Information type
    !----------------------------------------------------------------- 
        class(xdmf_information_t), intent(INOUT) :: this              !< XDMF Information type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))        deallocate(this%Name)
        if(allocated(this%Value))       deallocate(this%Value)
    end subroutine information_free


    subroutine information_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF information with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_information_t), intent(INOUT) :: this              !< XDMF Information type
    !----------------------------------------------------------------- 
        call this%free()
    end subroutine information_default_initialization


    subroutine information_open(this, xml_handler, Name, Value)
    !-----------------------------------------------------------------
    !< Open a new information XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_information_t),  intent(INOUT) :: this             !< XDMF Information type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Information Name attribute
        character(len=*), optional, intent(IN)    :: Value            !< XDMF Information Value attribute
    !----------------------------------------------------------------- 
        call this%set_tag('Information')

        call xml_NewElement(xml_handler, 'Information')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(Value))                                                      &
            call xml_AddAttribute(xml_handler, name="Value", value=Value)
    end subroutine information_open


    subroutine information_parse(this, DOMNode)
    !-----------------------------------------------------------------
    !< Parse a DOM information into a XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_information_t),  intent(INOUT) :: this             !< XDMF Information type
        type(Node),       pointer,  intent(IN)    :: DOMNode          !< FoX DOM Node containig a Information element
        character(len=:), allocatable             :: Name             !< XDMF Information Name attribute
        character(len=:), allocatable             :: Value            !< XDMF Information Value attribute
    !----------------------------------------------------------------- 
        call this%default_initialization()

        if(this%node_is_information(DOMNode)) then
            if(hasAttribute(DOMNode, 'Name')) then
                this%Name = getAttribute(DOMNode, 'Name')
            endif

            if(hasAttribute(DOMNode, 'Value')) then
                this%Value = getAttribute(DOMNode, 'Value')
            endif
        endif
    end subroutine information_parse


    subroutine information_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new Information XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_information_t), intent(IN)    :: this              !< XDMF Information type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Information')
    end subroutine information_close


    subroutine information_print(this, IndentationLevel)
    !-----------------------------------------------------------------
    !< Print on screen the Information XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_information_t), intent(IN)  :: this                !< XDMF Information type
        integer(I4P), optional,    intent(IN)  :: IndentationLevel    !< Indentation level
        integer(I4P)                           :: indlev = 0          !< Aux Indentation level
    !-----------------------------------------------------------------
        if(present(IndentationLevel)) indlev = IndentationLevel
        print*, repeat('  ',indlev)//'-------------------------------------------'
        print*, repeat('  ',indlev)//'INFORMATION:'
        print*, repeat('  ',indlev)//'-------------------------------------------'
        if(allocated(this%Name)) print*, repeat('  ',indlev)//'Name: '//this%Name
        if(allocated(this%Value)) print*, repeat('  ',indlev)//'Value: '//this%Value
    end subroutine information_print


end module xdmf_information
