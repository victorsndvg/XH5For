module xdmf_topology
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF topology handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use xdmf_utils,   only: is_in_option_list, warning_message
use xdmf_element, only: xdmf_element_t

implicit none
!---------------------------------------------------------------------
! XDMFTopology properties (* Default):
! Name               (no default)
! TopologyType       Polyvertex | Polyline | Polygon |
!                    Triangle | Quadrilateral | Tetrahedron | Pyramid| Wedge | Hexahedron |
!                    Edge_3 | Triagle_6 | Quadrilateral_8 | Tetrahedron_10 | Pyramid_13 |
!                    Wedge_15 | Hexahedron_20 |
!                    Mixed |
!                    2DSMesh | 2DRectMesh | 2DCoRectMesh |
!                    3DSMesh | 3DRectMesh | 3DCoRectMesh
! NodesPerElement    (no default) Only Important for Polyvertex, Polygon and Polyline
! NumberOfElement    (no default)
!     OR
! Dimensions         (no default)
! Order              each cell type has its own default
! BaseOffset         *0 | #
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_topology_t
    !-----------------------------------------------------------------
    !< XDMF Topology type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: Name
        character(len=:), allocatable :: TopologyType
        integer(I4P)                  :: NodesPerElement
        integer(I8P), allocatable     :: Dimensions(:)
        integer(I4P)                  :: Order
        integer(I4P)                  :: BaseOffset
    contains
    private
        procedure         :: default_initialization => topology_default_initialization
        procedure         :: is_valid_TopologyType  => topology_is_valid_TopologyType
        procedure         :: free                   => topology_free
        procedure, public :: open                   => topology_open
        procedure, public :: close                  => topology_close
    end type xdmf_topology_t

contains

    function topology_is_valid_TopologyType(this, TopologyType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid Topology TopologyType
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(IN) :: this                    !< XDMF Topology type
        character(len=*),       intent(IN) :: TopologyType            !< XDMF Topology TopologyType attribute
        logical                            :: is_valid                !< Valid TopologyType confirmation flag
        character(len=:), allocatable      :: allowed_TopologyTypes   !< Allowed TopologyTypes array
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_topologyTypes = 'Polyvertex&Polyline&Polygon&Triangle&Quadrilateral' // &
                            '&Tetrahedron&Pyramid&Wedge&Hexahedron&Edge_3&Triangle_6'// &
                            '&Quadrilateral_8&Tetrahedron_10&Pyramid_13&Wedge_15'    // &
                            '&Hexahedron_20&Mixed&2DSMesh&2DRectMesh&2DCoRectMesh'   // &
                            '&3DSMesh&3DRectMesh&3DCoRectMesh'

        is_valid = is_in_option_list(option_list=allowed_TopologyTypes, option=TopologyType, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message('Wrong TopologyType: "'//TopologyType//'" (Note: Case sensitive)')
    end function topology_is_valid_TopologyType


    subroutine topology_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Topology type
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(INOUT) :: this                 !< XDMF topology type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))         deallocate(this%Name)
        if(allocated(this%TopologyType)) deallocate(this%TopologyType)
        if(allocated(this%Dimensions))   deallocate(This%Dimensions)
        this%NodesPerElement = 0
        this%Order = 0
        this%BaseOffset = 0
    end subroutine topology_free


    subroutine topology_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF Topology with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(INOUT) :: this                 !< XDMF topology type
    !----------------------------------------------------------------- 
        call this%free()
    end subroutine topology_default_initialization


    subroutine topology_open(this, xml_handler, Name, TopologyType, NodesPerElement, Dimensions, Order, BaseOffset)
    !-----------------------------------------------------------------
    !< Open a new topology XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t),     intent(INOUT) :: this             !< XDMF Topology type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Topology Name attribute
        character(len=*), optional, intent(IN)    :: TopologyType     !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: NodesPerElement  !< XDMF Topology NodesPerElement attribute
        integer(i8P)    , optional, intent(IN)    :: Dimensions(:)    !< XDMF Topology Dimensions attribute
        integer(i4P)    , optional, intent(IN)    :: Order            !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: BaseOffset       !< XDMF Topology TopologyType attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
        integer(I4P)                              :: i                !< Aux index variable
    !----------------------------------------------------------------- 
        call this%set_tag('Topology')

        call xml_NewElement(xml_handler, 'Topology')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(TopologyType)) then; if(this%is_valid_TopologyType(TopologyType)) &
            call xml_AddAttribute(xml_handler, name="TopologyType", value=TopologyType)
        endif

        if(PRESENT(NodesPerElement)) &
            call xml_AddAttribute(xml_handler, name="NodesPerElement", value=NodesPerElement)

        if(PRESENT(Dimensions)) then
            if(allocated(char_dims)) deallocate(char_dims)
            i = size(Dimensions,dim=1)
            allocate(character(len=64*i) :: char_dims)
            write(char_dims, fmt=*)   (trim(adjustl(str(no_sign=.true., n=Dimensions(i))))//' ',i=1, size(Dimensions,dim=1) )
            call xml_AddAttribute(xml_handler, name="Dimensions", value=trim(char_dims) )
            deallocate(char_dims)
        endif

        if(PRESENT(Order)) &
            call xml_AddAttribute(xml_handler, name="Order", value=Order)

        if(PRESENT(BaseOffset)) &
            call xml_AddAttribute(xml_handler, name="BaseOffset", value=BaseOffset)
    end subroutine topology_open


    subroutine topology_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new topology XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(IN)    :: this                     !< XDMF topology type
        type(xmlf_t),       intent(INOUT) :: xml_handler              !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Topology')
    end subroutine topology_close


end module xdmf_topology
