module xdmf_grid
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF grid handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use PENF,         only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_element, only: xdmf_element_t
use xdmf_utils

implicit none
private
!---------------------------------------------------------------------
! XDMFGrid properties (* Default):
! Name             (no default)
! GridType         *Uniform | Collection | Tree | Subset
! CollectionType   *Spatial | Temporal (Only Meaningful if GridType="Collection")
! Section          *DataItem | All  (Only Meaningful if GridType="Subset")
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_grid_t
    !-----------------------------------------------------------------
    !< XDMF Grid type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: Name
        character(len=:), allocatable :: GridType
        character(len=:), allocatable :: CollectionType
        character(len=:), allocatable :: Section
    contains
    private
        procedure         :: xdmf_grid_open
        procedure         :: default_initialization => xdmf_grid_default_initialization
        procedure         :: collectiontype_is_spatial => xdmf_grid_collectiontype_is_spatial
        procedure         :: gridtype_is_collection => xdmf_grid_gridtype_is_collection
        procedure, public :: is_spatial_collection  => xdmf_grid_is_spatial_collection
        procedure, public :: free                   => xdmf_grid_free
        generic,   public :: open                   => xdmf_grid_open
        procedure, public :: parse                  => xdmf_grid_parse
        procedure, public :: close                  => xdmf_grid_close
        procedure, public :: print                  => xdmf_grid_print
        procedure, public :: get_Name               => xdmf_grid_get_Name
        procedure, public :: get_GridType           => xdmf_grid_get_GridType
        procedure, public :: get_CollectionType     => xdmf_grid_get_CollectionType
        procedure, public :: get_Section            => xdmf_grid_get_Section
    end type xdmf_grid_t

public :: xdmf_grid_t

contains


    function xdmf_grid_get_Name(this)
    !-----------------------------------------------------------------
    !< Return the Grid Name
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(IN) :: this                        !< XDMF Grid type
        character(len=:), allocatable :: xdmf_grid_get_Name           !< Grid Name
    !----------------------------------------------------------------- 
        xdmf_grid_get_Name = this%Name
    end function xdmf_grid_get_Name


    function xdmf_grid_get_GridType(this)
    !-----------------------------------------------------------------
    !< Return the Grid GridTpe
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(IN) :: this                        !< XDMF Grid type
        character(len=:), allocatable :: xdmf_grid_get_GridType       !< Grid GridType
    !----------------------------------------------------------------- 
        xdmf_grid_get_GridType = this%GridType
    end function xdmf_grid_get_GridType


    function xdmf_grid_get_CollectionType(this)
    !-----------------------------------------------------------------
    !< Return the Grid CollectionType
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(IN) :: this                        !< XDMF Grid type
        character(len=:), allocatable :: xdmf_grid_get_CollectionType !< Grid CollectionType
    !----------------------------------------------------------------- 
        xdmf_grid_get_CollectionType = this%CollectionType
    end function xdmf_grid_get_CollectionType


    function xdmf_grid_get_Section(this)
    !-----------------------------------------------------------------
    !< Return the Grid Section
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(IN) :: this                        !< XDMF Grid type
        character(len=:), allocatable :: xdmf_grid_get_Section        !< Grid Section
    !----------------------------------------------------------------- 
        xdmf_grid_get_Section = this%Section
    end function xdmf_grid_get_Section


    function xdmf_grid_gridtype_is_collection(this)
    !-----------------------------------------------------------------
    !< Check if GridType is collection
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),         intent(INOUT) :: this             !< XDMF Grid type
        logical :: xdmf_grid_gridtype_is_collection
    !-----------------------------------------------------------------         
        if(allocated(this%GridType)) xdmf_grid_gridtype_is_collection = this%GridType == 'Collection'
    end function xdmf_grid_gridtype_is_collection


    function xdmf_grid_collectiontype_is_spatial(this)
    !-----------------------------------------------------------------
    !< Check if CollectionType is spatial
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),         intent(INOUT) :: this             !< XDMF Grid type
        logical :: xdmf_grid_collectiontype_is_spatial
    !-----------------------------------------------------------------
        if(allocated(This%CollectionType)) xdmf_grid_collectiontype_is_spatial = this%CollectionType=='Spatial'
    end function xdmf_grid_collectiontype_is_spatial


    function xdmf_grid_is_spatial_collection(this)
    !-----------------------------------------------------------------
    !< Check if is a spatial collection grid
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),         intent(INOUT) :: this             !< XDMF Grid type
        logical :: xdmf_grid_is_spatial_collection
    !-----------------------------------------------------------------
        xdmf_grid_is_spatial_collection = this%gridtype_is_collection() .and. this%collectiontype_is_spatial()
    end function xdmf_grid_is_spatial_collection


    subroutine xdmf_grid_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Grid type
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(INOUT) :: this                     !< XDMF grid type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))           deallocate(this%Name)
        if(allocated(this%GridType))       deallocate(this%GridType)
        if(allocated(this%CollectionType)) deallocate(this%CollectionType)
        if(allocated(this%Section))        deallocate(this%Section)
    end subroutine xdmf_grid_free


    subroutine xdmf_grid_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF Grid with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(INOUT) :: this                     !< XDMF grid type
    !----------------------------------------------------------------- 
        call this%free()
        call this%set_tag('Grid')
        this%GridType       = 'Uniform'
        this%CollectionType = 'Spatial'
        this%Section        = 'DataItem'
    end subroutine xdmf_grid_default_initialization


    subroutine xdmf_grid_open(this, xml_handler, Name, GridType, CollectionType, Section)
    !-----------------------------------------------------------------
    !< Open a new grid XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),         intent(INOUT) :: this             !< XDMF Grid type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Grid Name attribute
        character(len=*), optional, intent(IN)    :: GridType         !< XDMF Grid GridType attribute
        character(len=*), optional, intent(IN)    :: CollectionType   !< XDMF Grid CollectionType attribute
        character(len=*), optional, intent(IN)    :: Section          !< XDMF Grid Section attribute
    !----------------------------------------------------------------- 
        call this%set_tag('Grid')

        call xml_NewElement(xml_handler, 'Grid')
        if(PRESENT(Name))  &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(GridType)) then; if(isSupportedGridTypeName(GridType)) &
            call xml_AddAttribute(xml_handler, name="GridType", value=GridType)
        endif

        if(PRESENT(CollectionType)) then; if(isSupportedGridCollectionTypeName(CollectionType)) &
            call xml_AddAttribute(xml_handler, name="CollectionType", value=CollectionType)
        endif

        if(PRESENT(Section)) then; if(isSupportedGridSectionName(Section)) &
            call xml_AddAttribute(xml_handler, name="Section", value=Section)
        endif
    end subroutine xdmf_grid_open


    subroutine xdmf_grid_parse(this, DOMNode)
    !-----------------------------------------------------------------
    !< Parse a DOM grid into a XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),         intent(INOUT) :: this             !< XDMF Grid type
        type(Node),       pointer,  intent(IN)    :: DOMNode          !< FoX DOM Node containig a Grid element
        character(len=:), allocatable             :: Name             !< XDMF Grid Name attribute
        character(len=:), allocatable             :: GridType         !< XDMF Grid GridType attribute
        character(len=:), allocatable             :: CollectionType   !< XDMF Grid CollectionType attribute
        character(len=:), allocatable             :: Section          !< XDMF Grid Section attribute
    !----------------------------------------------------------------- 
        call this%default_initialization()

        if(this%node_is_grid(DOMNode)) then
            if(hasAttribute(DOMNode, 'Name')) then
                this%Name = getAttribute(DOMNode, 'Name')
            endif

            if(hasAttribute(DOMNode, 'GridType')) then
                GridType = getAttribute(DOMNode, 'GridType')
                if(isSupportedGridTypeName(GridType)) this%GridType = GridType
            endif

            if(hasAttribute(DOMNode, 'CollectionType')) then
                CollectionType = getAttribute(DOMNode, 'CollectionType')
                if(isSupportedGridCollectionTypeName(CollectionType)) this%CollectionType = CollectionType
            endif

            if(hasAttribute(DOMNode, 'Section')) then
                Section = getAttribute(DOMNode, 'Section')
                if(isSupportedGridSectionName(Section)) this%Section = Section
            endif
        endif
    end subroutine xdmf_grid_parse

    subroutine xdmf_grid_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new grid XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(IN)    :: this                     !< XDMF grid type
        type(xmlf_t),       intent(INOUT) :: xml_handler              !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Grid')
    end subroutine xdmf_grid_close

    subroutine xdmf_grid_print(this, IndentationLevel)
    !-----------------------------------------------------------------
    !< Print on screen the Grid XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(IN)      :: this                   !< XDMF grid type
        integer(I4P), optional,  intent(IN) :: IndentationLevel       !< Indentation level
        integer(I4P)                        :: indlev = 0             !< Aux Indentation level
    !-----------------------------------------------------------------
        if(present(IndentationLevel)) indlev = IndentationLevel
        print*, repeat('  ',indlev)//'-------------------------------------------'
        print*, repeat('  ',indlev)//'GRID:'
        print*, repeat('  ',indlev)//'-------------------------------------------'
        if(allocated(this%Name)) print*, repeat('  ',indlev)//'Name: '//this%Name
        if(allocated(this%GridType)) print*, repeat('  ',indlev)//'GridType: '//this%GridType
        if(allocated(this%CollectionType)) print*, repeat('  ',indlev)//'CollectionType: '//this%CollectionType
        if(allocated(this%Section)) print*, repeat('  ',indlev)//'Section: '//this%Section
    end subroutine xdmf_grid_print

end module xdmf_grid
