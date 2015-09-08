module xdmf_attribute
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF attribute handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use xdmf_utils,   only: is_in_option_list, warning_message
use xdmf_element, only: xdmf_element_t

implicit none
!---------------------------------------------------------------------
! XDMFAttribute properties (* Default):
! Name            (no default)
! AttributeType   *Scalar | Vector | Tensor | Tensor6 | Matrix | GlobalID
! Center          *Node | Cell | Grid | Face | Edge
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_attribute_t
    !-----------------------------------------------------------------
    !< XDMF Attribute type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: Name
        character(len=:), allocatable :: AttributeType
        character(len=:), allocatable :: Center
    contains
    private
        procedure         :: attribute_open
        procedure         :: default_initialization => attribute_default_initialization
        procedure         :: is_valid_AttributeType => attribute_is_valid_AttributeType
        procedure         :: is_valid_Center        => attribute_is_valid_Center
        procedure         :: free                   => attribute_free
        generic,   public :: open                   => attribute_open
        procedure, public :: close                  => attribute_close
    end type xdmf_attribute_t

contains

    function attribute_is_valid_AttributeType(this, AttributeType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid Attribute AttributeType
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(IN) :: this                   !< XDMF Attribute type
        character(len=*),        intent(IN) :: AttributeType          !< XDMF Attribute AttributeType attribute
        logical                             :: is_valid               !< Valid AttributeType confirmation flag
        character(len=:), allocatable       :: allowed_AttributeTypes !< Allowed AttributeTypes array
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_AttributeTypes = 'Scalar&Vector&Tensor&Tensor6&Matrix&GlobalID'
        is_valid = is_in_option_list(option_list=allowed_AttributeTypes, option=AttributeType, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message('Wrong AttributeType: "'//AttributeType//'" (Note: Case sensitive)')
    end function attribute_is_valid_AttributeType


    function attribute_is_valid_Center(this, Center) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid attribute Section
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(IN) :: this                   !< XDMF Attribute type
        character(len=*),        intent(IN) :: Center                 !< XDMF Attribute Center attribute
        logical                             :: is_valid               !< Valid Center confirmation flag
        character(len=:), allocatable       :: allowed_Centers        !< Allowed Sections array
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_Centers = 'Node&Cell&Grid&Face&Edge'
        is_valid = is_in_option_list(option_list=allowed_Centers, option=Center, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message('Wrong Center: "'//Center//'" (Note: Case sensitive)')
    end function attribute_is_valid_Center


    subroutine attribute_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Attribute type
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(INOUT) :: this                !< XDMF attribute type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))           deallocate(this%Name)
        if(allocated(this%AttributeType))  deallocate(this%AttributeType)
        if(allocated(this%Center))        deallocate(this%Center)
    end subroutine attribute_free


    subroutine attribute_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF Attribute with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(INOUT) :: this                !< XDMF attribute type
    !----------------------------------------------------------------- 
        call this%free()
        this%AttributeType  = 'Scalar'
        this%Center         = 'Node'
    end subroutine attribute_default_initialization


    subroutine attribute_open(this, xml_handler, Name, AttributeType, Center)
    !-----------------------------------------------------------------
    !< Open a new attribute XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t),    intent(INOUT) :: this             !< XDMF Attribute type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Attribute Name attribute
        character(len=*), optional, intent(IN)    :: AttributeType    !< XDMF Attribute AttributeType attribute
        character(len=*), optional, intent(IN)    :: Center           !< XDMF Attribute Center attribute
    !----------------------------------------------------------------- 
        call this%set_tag('Attribute')

        call xml_NewElement(xml_handler, 'Attribute')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(AttributeType)) then; if(this%is_valid_AttributeType(AttributeType)) &
            call xml_AddAttribute(xml_handler, name="AttributeType", value=AttributeType)
        endif

        if(PRESENT(Center)) then; if(this%is_valid_Center(Center)) &
            call xml_AddAttribute(xml_handler, name="Center", value=Center)
        endif
    end subroutine attribute_open


    subroutine attribute_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new attribute XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(IN)    :: this                !< XDMF attribute type
        type(xmlf_t),            intent(INOUT) :: xml_handler         !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Attribute')
    end subroutine attribute_close


end module xdmf_attribute
