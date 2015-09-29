module xdmf_grid
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF grid handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_utils,   only: is_in_option_list, warning_message
use xdmf_element, only: xdmf_element_t

implicit none
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
        procedure         :: grid_open
        procedure         :: default_initialization => grid_default_initialization
        procedure         :: is_valid_GridType      => grid_is_valid_GridType
        procedure         :: is_valid_CollectionType=> grid_is_valid_CollectionType
        procedure         :: is_valid_Section       => grid_is_valid_Section
        procedure         :: collectiontype_is_spatial => grid_collectiontype_is_spatial
        procedure         :: gridtype_is_collection => grid_gridtype_is_collection
        procedure, public :: is_spatial_collection  => grid_is_spatial_collection
        procedure, public :: free                   => grid_free
        generic,   public :: open                   => grid_open
        procedure, public :: parse                  => grid_parse
        procedure, public :: close                  => grid_close
        procedure, public :: print                  => grid_print
    end type xdmf_grid_t

contains

    function grid_is_valid_GridType(this, GridType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid Grid GridType
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),     intent(IN) :: this                    !< XDMF Grid type
        character(len=*),       intent(IN) :: GridType                !< XDMF Grid GridType attribute
        logical                            :: is_valid                !< Valid GridType confirmation flag
        character(len=:), allocatable      :: allowed_GridTypes       !< Allowed GridTypes array
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_GridTypes = 'Uniform&Collection&Tree&Subset'
        is_valid = is_in_option_list(option_list=allowed_GridTypes, option=GridType, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message('Wrong GridType: "'//GridType//'" (Note: Case sensitive)')
    end function grid_is_valid_GridType


    function grid_is_valid_CollectionType(this, CollectionType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid grid CollectionType
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),     intent(IN) :: this                    !< XDMF Grid type
        character(len=*),       intent(IN) :: CollectionType          !< XDMF Grid GridType attribute
        logical                            :: is_valid                !< Valid GridType confirmation flag
        character(len=:), allocatable      :: allowed_CollectionTypes !< Allowed GridTypes array
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_CollectionTypes = 'Spatial&Temporal'
        is_valid = is_in_option_list(option_list=allowed_CollectionTypes, option=CollectionType, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message('Wrong CollectionType: "'//CollectionType//'" (Note: Case sensitive)')
    end function grid_is_valid_CollectionType

    function grid_is_valid_Section(this, Section) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid grid Section
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),     intent(IN) :: this                    !< XDMF Grid type
        character(len=*),       intent(IN) :: Section                 !< XDMF Grid Section attribute
        logical                            :: is_valid                !< Valid Section confirmation flag
        character(len=:), allocatable      :: allowed_Sections        !< Allowed Sections array
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_Sections = 'DataItem&All'
        is_valid = is_in_option_list(option_list=allowed_Sections, option=Section, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message('Wrong Section: "'//Section//'" (Note: Case sensitive)')
    end function grid_is_valid_Section


    function grid_gridtype_is_collection(this)
    !-----------------------------------------------------------------
    !< Check if GridType is collection
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),         intent(INOUT) :: this             !< XDMF Grid type
        logical :: grid_gridtype_is_collection
    !-----------------------------------------------------------------         
        if(allocated(this%GridType)) grid_gridtype_is_collection = this%GridType == 'Collection'
    end function grid_gridtype_is_collection


    function grid_collectiontype_is_spatial(this)
    !-----------------------------------------------------------------
    !< Check if CollectionType is spatial
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),         intent(INOUT) :: this             !< XDMF Grid type
        logical :: grid_collectiontype_is_spatial
    !-----------------------------------------------------------------
        if(allocated(This%CollectionType)) grid_collectiontype_is_spatial = this%CollectionType=='Spatial'
    end function grid_collectiontype_is_spatial


    function grid_is_spatial_collection(this)
    !-----------------------------------------------------------------
    !< Check if is a spatial collection grid
    !----------------------------------------------------------------- 
        class(xdmf_grid_t),         intent(INOUT) :: this             !< XDMF Grid type
        logical :: grid_is_spatial_collection
    !-----------------------------------------------------------------
        grid_is_spatial_collection = this%gridtype_is_collection() .and. this%collectiontype_is_spatial()
    end function grid_is_spatial_collection


    subroutine grid_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Grid type
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(INOUT) :: this                     !< XDMF grid type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))           deallocate(this%Name)
        if(allocated(this%GridType))       deallocate(this%GridType)
        if(allocated(this%CollectionType)) deallocate(this%CollectionType)
        if(allocated(this%Section))        deallocate(this%Section)
    end subroutine grid_free


    subroutine grid_default_initialization(this)
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
    end subroutine grid_default_initialization


    subroutine grid_open(this, xml_handler, Name, GridType, CollectionType, Section)
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
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(GridType)) then; if(this%is_valid_GridType(GridType)) &
            call xml_AddAttribute(xml_handler, name="GridType", value=GridType)
        endif

        if(PRESENT(CollectionType)) then; if(this%is_valid_CollectionType(CollectionType)) &
            call xml_AddAttribute(xml_handler, name="CollectionType", value=CollectionType)
        endif

        if(PRESENT(Section)) then; if(this%is_valid_Section(Section)) &
            call xml_AddAttribute(xml_handler, name="Section", value=Section)
        endif
    end subroutine grid_open


    subroutine grid_parse(this, DOMNode)
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
                if(this%is_valid_GridType(GridType=GridType)) this%GridType = GridType
            endif

            if(hasAttribute(DOMNode, 'CollectionType')) then
                CollectionType = getAttribute(DOMNode, 'CollectionType')
                if(this%is_valid_CollectionType(CollectionType=CollectionType)) this%CollectionType = CollectionType
            endif

            if(hasAttribute(DOMNode, 'Section')) then
                Section = getAttribute(DOMNode, 'Section')
                if(this%is_valid_Section(Section=Section)) this%Section = Section
            endif
        endif
    end subroutine grid_parse

    subroutine grid_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new grid XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(IN)    :: this                     !< XDMF grid type
        type(xmlf_t),       intent(INOUT) :: xml_handler              !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Grid')
    end subroutine grid_close

    subroutine grid_print(this)
    !-----------------------------------------------------------------
    !< Print on screen the Grid XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_grid_t), intent(IN)    :: this                     !< XDMF grid type
    !-----------------------------------------------------------------
        print*, '-------------------------------------------'
        print*, 'GRID:'
        print*, '-------------------------------------------'
        if(allocated(this%Name)) print*, 'Name: '//this%Name
        if(allocated(this%GridType)) print*, 'GridType: '//this%GridType
        if(allocated(this%CollectionType)) print*, 'CollectionType: '//this%CollectionType
        if(allocated(this%Section)) print*, 'Section: '//this%Section
    end subroutine grid_print

end module xdmf_grid
