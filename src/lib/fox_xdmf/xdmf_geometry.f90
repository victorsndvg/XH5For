module xdmf_geometry
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Geometry handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_utils,   only: is_in_option_list, warning_message
use xdmf_element, only: xdmf_element_t

implicit none
!---------------------------------------------------------------------
! XDMFGeometry properties (* Default):
! GeometryType     XYZ | XY | X_Y_Z | VxVyVz | Origin_DxDyDz | Origin_DxDy
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_geometry_t
    !-----------------------------------------------------------------
    !< XDMF Geometry type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: GeometryType
    contains
    private
        procedure         :: geometry_open 
        procedure         :: default_initialization => geometry_default_initialization
        procedure         :: is_valid_GeometryType  => geometry_is_valid_GeometryType
        procedure, public :: free                   => geometry_free
        generic,   public :: open                   => geometry_open
        procedure, public :: parse                  => geometry_parse
        procedure, public :: close                  => geometry_close
        procedure, public :: print                  => geometry_print
    end type xdmf_geometry_t

contains

    function geometry_is_valid_GeometryType(this, GeometryType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid Grid GridType
    !----------------------------------------------------------------- 
        class(xdmf_geometry_t), intent(IN) :: this                    !< XDMF Grid type
        character(len=*),       intent(IN) :: GeometryType            !< XDMF Grid GeometryType attribute
        logical                            :: is_valid                !< Valid GeometryType confirmation flag
        character(len=:), allocatable      :: allowed_GeometryTypes   !< Allowed GeometryType array
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_GeometryTypes = 'XYZ&XY&X_Y_Z&VxVyVz&Origin_DxDyDz%Origin_DxDy'
        is_valid = is_in_option_list(option_list=allowed_GeometryTypes, option=GeometryType, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message('Wrong GeometryType: "'//GeometryType//'" (Note: Case sensitive)')
    end function geometry_is_valid_GeometryType


    subroutine geometry_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Geometry type
    !----------------------------------------------------------------- 
        class(xdmf_geometry_t), intent(INOUT) :: this                 !< XDMF Geometry type
    !----------------------------------------------------------------- 
        if(allocated(this%GeometryType))   deallocate(this%GeometryType)
    end subroutine geometry_free


    subroutine geometry_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF geometry with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_geometry_t), intent(INOUT) :: this                 !< XDMF Geometry type
    !----------------------------------------------------------------- 
        call this%free()
        this%GeometryType = 'XYZ'
    end subroutine geometry_default_initialization


    subroutine geometry_open(this, xml_handler, GeometryType)
    !-----------------------------------------------------------------
    !< Open a new geometry XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_geometry_t),     intent(INOUT) :: this             !< XDMF Geometry type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: GeometryType     !< XDMF Geometry Name attribute
    !----------------------------------------------------------------- 
        call this%set_tag('Geometry')

        call xml_NewElement(xml_handler, 'Geometry')
        if(PRESENT(GeometryType)) then; if(this%is_valid_GeometryType(GeometryType)) &
            call xml_AddAttribute(xml_handler, name="GeometryType", value=GeometryType)
        endif
    end subroutine geometry_open


    subroutine geometry_parse(this, DOMNode)
    !-----------------------------------------------------------------
    !< Parse a DOM geometry into a XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_geometry_t),     intent(INOUT) :: this             !< XDMF Geometry type
        type(Node),       pointer,  intent(IN)    :: DOMNode          !< FoX DOM Node containig a Geometry element
        character(len=:), allocatable             :: GeometryType     !< XDMF Geometry Geometry attribute
    !----------------------------------------------------------------- 
        call this%default_initialization()

        if(this%node_is_geometry(DOMNode)) then
            if(hasAttribute(DOMNode, 'GeometryType')) then
                GeometryType = getAttribute(DOMNode, 'GeometryType')
                if(this%is_valid_GeometryType(GeometryType=GeometryType)) this%GeometryType = GeometryType
            endif
        endif
    end subroutine geometry_parse


    subroutine geometry_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new Geometry XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_geometry_t), intent(IN)    :: this                 !< XDMF Geometry type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Geometry')
    end subroutine geometry_close


    subroutine geometry_print(this)
    !-----------------------------------------------------------------
    !< Print on screen the Geometry XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_geometry_t), intent(IN)    :: this                     !< XDMF Geometry type
    !-----------------------------------------------------------------
        print*, '-------------------------------------------'
        print*, 'GEOMETRY:'
        print*, '-------------------------------------------'
        if(allocated(this%GeometryType)) print*, 'GeometryType: '//this%GeometryType
    end subroutine geometry_print


end module xdmf_geometry
