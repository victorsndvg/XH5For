module xdmf_information
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Information handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
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
        procedure         :: free                   => information_free
        generic,   public :: open                   => information_open
        procedure, public :: close                  => information_close
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


    subroutine information_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new Information XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_information_t), intent(IN)    :: this              !< XDMF Information type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Information')
    end subroutine information_close


end module xdmf_information
