module xdmf_xinclude
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF XI:include handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use PENF,         only: I4P
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_element, only: xdmf_element_t
use xdmf_utils

implicit none
private
!---------------------------------------------------------------------
! Xinclude properties (* Default): https://www.w3.org/TR/xinclude/
! HRef                  URI
! Parse                 *xml, text (Not allowed)
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_xinclude_t
    !-----------------------------------------------------------------
    !< XDMF Xinclude type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: HRef
    contains
    private
        procedure         :: default_initialization => xdmf_xinclude_default_initialization
        procedure         :: xdmf_xinclude_open
        procedure, public :: free                   => xdmf_xinclude_free
        generic,   public :: open                   => xdmf_xinclude_open
        procedure, public :: parse                  => xdmf_xinclude_parse
        procedure, public :: GetHRef                => xdmf_xinclude_GetHRef
        procedure, public :: close                  => xdmf_xinclude_close
        procedure, public :: print                  => xdmf_xinclude_print
    end type xdmf_xinclude_t

public :: xdmf_xinclude_t

contains

    function xdmf_xinclude_get_HRef(this)
    !-----------------------------------------------------------------
    !< Return the Xinclude Value
    !----------------------------------------------------------------- 
        class(xdmf_xinclude_t), intent(IN) :: this                    !< XDMF Xinclude type
        character(len=:), allocatable :: xdmf_xinclude_get_HRef       !< HRef string
    !----------------------------------------------------------------- 
        xdmf_xinclude_get_HRef = this%HRef
    end function xdmf_xinclude_get_HRef


    subroutine xdmf_xinclude_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Xinclude type
    !----------------------------------------------------------------- 
        class(xdmf_xinclude_t), intent(INOUT) :: this                 !< XDMF Xinclude type
    !----------------------------------------------------------------- 
        if(allocated(this%HRef))    deallocate(this%HRef)
    end subroutine xdmf_xinclude_free


    subroutine xdmf_xinclude_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF Xinclude with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_xinclude_t), intent(INOUT) :: this                 !< XDMF Xinclude type
    !----------------------------------------------------------------- 
        call this%free()
        this%HRef = ''
    end subroutine xdmf_xinclude_default_initialization


    subroutine xdmf_xinclude_open(this, xml_handler, HRef)
    !-----------------------------------------------------------------
    !< Open a new Xinclude XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_xinclude_t),     intent(INOUT) :: this             !< XDMF Xinclude type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: HRef             !< XDMF Xinclude XI:includeType attribute
    !----------------------------------------------------------------- 
        call this%set_tag('xi:include')

        call xml_NewElement(xml_handler, 'xi:include')
        if(PRESENT(HRef)) then
            call xml_AddAttribute(xml_handler, name="href", value=HRef)
        endif
        call xml_AddAttribute(xml_handler, name="parse", value='xml')
    end subroutine xdmf_xinclude_open


    subroutine xdmf_xinclude_parse(this, DOMNode)
    !-----------------------------------------------------------------
    !< Parse a DOM XI:include into a XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_xinclude_t),     intent(INOUT) :: this             !< XDMF XI:include type
        type(Node),       pointer,  intent(IN)    :: DOMNode          !< FoX DOM Node containig a XI:include element
    !----------------------------------------------------------------- 
        call this%default_initialization()

        if(this%node_is_xinclude(DOMNode)) then
            if(hasAttribute(DOMNode, 'href')) then
                this%HRef = getAttribute(DOMNode, 'href')
            endif

        endif
    end subroutine xdmf_xinclude_parse


    function xdmf_xinclude_GetHRef(this) result(HRef)
    !-----------------------------------------------------------------
    !< Return HRef
    !----------------------------------------------------------------- 
        class(xdmf_xinclude_t),     intent(INOUT) :: this             !< XDMF XI:include type
        character(len=:), allocatable             :: HRef             !< Return XInclude HRef
    !----------------------------------------------------------------- 
        if(allocated(this%HRef)) HRef = this%HRef
    end function xdmf_xinclude_GetHRef


    subroutine xdmf_xinclude_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new XI:include XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_xinclude_t), intent(IN)    :: this                 !< XDMF XI:include type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'xi:include')
    end subroutine xdmf_xinclude_close


    subroutine xdmf_xinclude_print(this, IndentationLevel)
    !-----------------------------------------------------------------
    !< Print on screen the XI:include XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_xinclude_t),  intent(IN)    :: this                !< XDMF XI:include type
        integer(I4P), optional,  intent(IN)    :: IndentationLevel    !< Indentation level
        integer(I4P)                           :: indlev = 0          !< Aux Indentation level
    !-----------------------------------------------------------------
        if(present(IndentationLevel)) indlev = IndentationLevel
        print*, repeat('  ',indlev)//'-------------------------------------------'
        print*, repeat('  ',indlev)//'xi:include:'
        print*, repeat('  ',indlev)//'-------------------------------------------'
        if(allocated(this%HRef))  print*, repeat('  ',indlev)//'HRef: '//this%HRef
    end subroutine xdmf_xinclude_print


end module xdmf_xinclude
