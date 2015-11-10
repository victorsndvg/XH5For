module xdmf_attribute
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF attribute handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_element, only: xdmf_element_t
use xdmf_utils

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
        procedure         :: xdmf_attribute_open
        procedure         :: default_initialization => xdmf_attribute_default_initialization
        procedure, public :: free                   => xdmf_attribute_free
        generic,   public :: open                   => xdmf_attribute_open
        procedure, public :: parse                  => xdmf_attribute_parse
        procedure, public :: close                  => xdmf_attribute_close
        procedure, public :: print                  => xdmf_attribute_print
        procedure, public :: get_Name               => xdmf_attribute_get_Name
        procedure, public :: get_AttributeType      => xdmf_attribute_get_AttributeType
        procedure, public :: get_Center             => xdmf_attribute_get_Center
    end type xdmf_attribute_t

contains

    function xdmf_attribute_get_Name(this)
    !-----------------------------------------------------------------
    !< Return the Attribute Name
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(IN) :: this                   !< XDMF Attribute type
        character(len=:), allocatable :: xdmf_attribute_get_name       !< Attribute Name
    !----------------------------------------------------------------- 
        xdmf_attribute_get_name = this%Name
    end function xdmf_attribute_get_Name


    function xdmf_attribute_get_AttributeType(this)
    !-----------------------------------------------------------------
    !< Return the Attribute Name
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(IN) :: this                       !< XDMF Attribute type
        character(len=:), allocatable :: xdmf_attribute_get_AttributeType !< Attribute Attribute
    !----------------------------------------------------------------- 
        xdmf_attribute_get_AttributeType = this%AttributeType
    end function xdmf_attribute_get_AttributeType


    function xdmf_attribute_get_Center(this)
    !-----------------------------------------------------------------
    !< Return the Attribute Name
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(IN) :: this                    !< XDMF Attribute type
        character(len=:), allocatable :: xdmf_attribute_get_Center     !< Attribute Center
    !----------------------------------------------------------------- 
        xdmf_attribute_get_Center = this%Center
    end function xdmf_attribute_get_Center


    subroutine xdmf_attribute_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Attribute type
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(INOUT) :: this                !< XDMF attribute type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))           deallocate(this%Name)
        if(allocated(this%AttributeType))  deallocate(this%AttributeType)
        if(allocated(this%Center))        deallocate(this%Center)
    end subroutine xdmf_attribute_free


    subroutine xdmf_attribute_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF Attribute with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(INOUT) :: this                !< XDMF attribute type
    !----------------------------------------------------------------- 
        call this%free()
        this%AttributeType  = 'Scalar'
        this%Center         = 'Node'
    end subroutine xdmf_attribute_default_initialization


    subroutine xdmf_attribute_open(this, xml_handler, Name, AttributeType, Center)
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

        if(PRESENT(AttributeType)) then; if(isSupportedAttributeTypeName(AttributeType)) &
            call xml_AddAttribute(xml_handler, name="AttributeType", value=AttributeType)
        endif

        if(PRESENT(Center)) then; if(isSupportedAttributeCenterName(Center)) &
            call xml_AddAttribute(xml_handler, name="Center", value=Center)
        endif
    end subroutine xdmf_attribute_open


    subroutine xdmf_attribute_parse(this, DOMNode)
    !-----------------------------------------------------------------
    !< Parse a DOM attribute into a XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t),    intent(INOUT) :: this             !< XDMF Attribute type
        type(Node),       pointer,  intent(IN)    :: DOMNode          !< FoX DOM Node containig a Attribute element
        character(len=:), allocatable             :: Name             !< XDMF Attribute Name attribute
        character(len=:), allocatable             :: AttributeType    !< XDMF Attribute AttributeType attribute
        character(len=:), allocatable             :: Center           !< XDMF Attribute Center attribute
    !----------------------------------------------------------------- 
        call this%default_initialization()

        if(this%node_is_attribute(DOMNode)) then
            if(hasAttribute(DOMNode, 'Name')) then
                this%Name = getAttribute(DOMNode, 'Name')
            endif

            if(hasAttribute(DOMNode, 'AttributeType')) then
                AttributeType = getAttribute(DOMNode, 'AttributeType')
                if(isSupportedAttributeTypeName(AttributeType)) this%AttributeType = AttributeType
            endif

            if(hasAttribute(DOMNode, 'Center')) then
                center = getAttribute(DOMNode, 'Center')
                if(isSupportedAttributeCenterName(Center)) this%Center = Center
            endif
        endif
    end subroutine xdmf_attribute_parse


    subroutine xdmf_attribute_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new attribute XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(IN)    :: this                !< XDMF attribute type
        type(xmlf_t),            intent(INOUT) :: xml_handler         !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Attribute')
    end subroutine xdmf_attribute_close


    subroutine xdmf_attribute_print(this, IndentationLevel)
    !-----------------------------------------------------------------
    !< Print on screen the Attribute XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_attribute_t), intent(IN)    :: this                !< XDMF grid type
        integer(I4P), optional,  intent(IN)    :: IndentationLevel    !< Indentation level
        integer(I4P)                           :: indlev = 0          !< Aux Indentation level
    !-----------------------------------------------------------------
        if(present(IndentationLevel)) indlev = IndentationLevel
        print*, repeat('  ',indlev)//'-------------------------------------------'
        print*, repeat('  ',indlev)//'ATTRIBUTE:'
        print*, repeat('  ',indlev)//'-------------------------------------------'
        if(allocated(this%Name)) print*, repeat('  ',indlev)//'Name: '//this%Name
        if(allocated(this%AttributeType)) print*, repeat('  ',indlev)//'AttributeType: '//this%AttributeType
        if(allocated(this%Center)) print*, repeat('  ',indlev)//'Center: '//this%Center
    end subroutine xdmf_attribute_print



end module xdmf_attribute
