module xdmf_time
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, R4P, R8P, str, cton
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_utils,   only: is_in_option_list, warning_message
use xdmf_element, only: xdmf_element_t

implicit none
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
        procedure         :: default_initialization => time_default_initialization
        procedure         :: is_valid_TimeType      => time_is_valid_TimeType
        procedure, public :: free                   => time_free
        procedure         :: time_open_timetype
        procedure         :: time_open_R4P_value
        procedure         :: time_open_R8P_value
        generic,   public :: open                   => time_open_R4P_value, &
                                                       time_open_R8P_value, &
                                                       time_open_timetype
        procedure, public :: parse                  => time_parse
        procedure, public :: close                  => time_close
        procedure, public :: print                  => time_print
    end type xdmf_time_t

contains

    function time_is_valid_TimeType(this, TimeType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid Grid GridType
    !----------------------------------------------------------------- 
        class(xdmf_time_t), intent(IN) :: this                        !< XDMF Grid type
        character(len=*),   intent(IN) :: TimeType                    !< XDMF Grid TimeType attribute
        logical                        :: is_valid                    !< Valid TimeType confirmation flag
        character(len=:), allocatable  :: allowed_TimeTypes           !< Allowed TimeType array
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_TimeTypes = 'Single&HyperSlab&List&Range'
        is_valid = is_in_option_list(option_list=allowed_TimeTypes, option=TimeType, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message('Wrong TimeType: "'//TimeType//'" (Note: Case sensitive)')
    end function time_is_valid_TimeType


    subroutine time_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Time type
    !----------------------------------------------------------------- 
        class(xdmf_time_t), intent(INOUT) :: this                 !< XDMF Time type
    !----------------------------------------------------------------- 
        if(allocated(this%TimeType))   deallocate(this%TimeType)
    end subroutine time_free


    subroutine time_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF time with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_time_t), intent(INOUT) :: this                 !< XDMF Time type
    !----------------------------------------------------------------- 
        call this%free()
        this%TimeType = ''
        this%Value = 0._R8P
    end subroutine time_default_initialization

    subroutine time_open_timetype(this, xml_handler, TimeType)
    !-----------------------------------------------------------------
    !< Open a new time XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_time_t),         intent(INOUT) :: this             !< XDMF Time type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: TimeType         !< XDMF Time TimeType attribute
    !----------------------------------------------------------------- 
        call this%set_tag('Time')

        call xml_NewElement(xml_handler, 'Time')
        if(PRESENT(TimeType)) then; if(this%is_valid_TimeType(TimeType)) &
            call xml_AddAttribute(xml_handler, name="TimeType", value=TimeType)
        endif
    end subroutine time_open_timetype

    subroutine time_open_R4P_value(this, xml_handler, TimeType, Value)
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
        if(PRESENT(TimeType)) then; if(this%is_valid_TimeType(TimeType)) &
            call xml_AddAttribute(xml_handler, name="TimeType", value=TimeType)
        endif

        call xml_AddAttribute(xml_handler, name="Value", value=Value)
    end subroutine time_open_R4P_value

    subroutine time_open_R8P_value(this, xml_handler, TimeType, Value)
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
        if(PRESENT(TimeType)) then; if(this%is_valid_TimeType(TimeType)) &
            call xml_AddAttribute(xml_handler, name="TimeType", value=TimeType)
        endif

        call xml_AddAttribute(xml_handler, name="Value", value=Value)
    end subroutine time_open_R8P_value


    subroutine time_parse(this, DOMNode)
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
                if(this%is_valid_TimeType(TimeType=TimeType)) this%TimeType = TimeType
            endif

            if(hasAttribute(DOMNode, 'Value')) then
                this%Value = cton(str=getAttribute(DOMNode, 'Value'),knd=0._R8P)
            endif
        endif
    end subroutine time_parse


    subroutine time_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new Time XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_time_t), intent(IN)    :: this                 !< XDMF Time type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Time')
    end subroutine time_close

    subroutine time_print(this, IndentationLevel)
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
    end subroutine time_print


end module xdmf_time
