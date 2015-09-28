module uniform_grid_descriptor

use IR_Precision, only: I4P, I8P, str
use xdmf_utils,   only : warning_message
use XH5For_parameters

implicit none

private

    type :: uniform_grid_descriptor_t
    !-----------------------------------------------------------------
    !< Save local grid information
    !----------------------------------------------------------------- 
        integer(I4P) :: GridType           = XDMF_GRID_TYPE_UNSTRUCTURED
        integer(I4P) :: GeometryType       = XDMF_GEOMETRY_TYPE_XYZ
        integer(I4P) :: TopologyType       = XDMF_NO_VALUE
        integer(I8P) :: NumberOfNodes      = XDMF_NO_VALUE
        integer(I8P) :: NumberOfElements   = XDMF_NO_VALUE
        logical      :: warn = .true.  !< Flag to show warnings on screen
    contains
        procedure         :: is_valid_TopologyType => uniform_grid_descriptor_is_valid_TopologyType
        procedure         :: is_valid_GeometryType => uniform_grid_descriptor_is_valid_GeometryType
        procedure, public :: Initialize            => uniform_grid_descriptor_Initialize
        procedure, public :: Free                  => uniform_grid_descriptor_Free
        procedure, public :: SetNumberOfNodes      => uniform_grid_descriptor_SetNumberOfNodes
        procedure, public :: SetNumberOfElements   => uniform_grid_descriptor_SetNumberOfElements
        procedure, public :: SetTopologyType       => uniform_grid_descriptor_SetTopologyType
        procedure, public :: SetGeometryType       => uniform_grid_descriptor_SetGeometryType
        procedure, public :: GetNumberOfNodes      => uniform_grid_descriptor_GetNumberOfNodes
        procedure, public :: GetNumberOfElements   => uniform_grid_descriptor_GetNumberOfElements
        procedure, public :: GetTopologyType       => uniform_grid_descriptor_GetTopologyType
        procedure, public :: GetGeometryType       => uniform_grid_descriptor_GetGeometryType
    end type uniform_grid_descriptor_t

public:: uniform_grid_descriptor_t

contains

    subroutine uniform_grid_descriptor_SetNumberOfNodes(this, NumberOfNodes)
    !-----------------------------------------------------------------
    !< Set the number of nodes of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this          !< Local grid descriptor
        integer(I8P),                     intent(IN)    :: NumberOfNodes !< Number of nodes of the local grid
    !-----------------------------------------------------------------
        this%NumberOfNodes = NumberOfNodes
    end subroutine uniform_grid_descriptor_SetNumberOfNodes


    subroutine uniform_grid_descriptor_SetNumberOfElements(this, NumberOfElements)
    !-----------------------------------------------------------------
    !< Set the number of elements of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
        integer(I8P),                     intent(IN)    :: NumberOfElements !< Number of nodes of the local grid
    !-----------------------------------------------------------------
        this%NumberOfElements = NumberOfElements
    end subroutine uniform_grid_descriptor_SetNumberOfElements


    function uniform_grid_descriptor_GetNumberOfNodes(this)
    !-----------------------------------------------------------------
    !< Return the number of nodes of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I8P) :: uniform_grid_descriptor_getNumberOfNodes      !< Number of nodes of the local grid
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetNumberOfNodes = this%NumberOfNodes
    end function uniform_grid_descriptor_GetNumberOfNodes


    function uniform_grid_descriptor_GetNumberOfElements(this)
    !-----------------------------------------------------------------
    !< Return the number of elements of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Localk grid descriptor
        integer(I8P) :: uniform_grid_descriptor_GetNumberOfElements   !< Number of elements of the local grid
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetNumberOfElements = this%NumberOfElements
    end function uniform_grid_descriptor_GetNumberOfElements


    function uniform_grid_descriptor_is_valid_TopologyType(this, TopologyType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem NumberType
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t),  intent(IN) :: this                    !< Local Data Handler
        integer(I4P),                      intent(IN) :: TopologyType            !< XDMF Topology Type
        logical                                       :: is_valid                !< Valid Topology Type confirmation flag
        integer(I4P), allocatable                     :: allowed_TopologyTypes(:)!< Dataitem Topology Type list 
    !----------------------------------------------------------------- 
        !< @Note: Mixed topologies or variable number of node elements not allowed
        !< @Note: mixed topologies can be implemented with and array of celltypes (offset?)
        !< @Note: variable number of nodes can be implemented with and array of number of nodes (offset?)
        allowed_TopologyTypes = (/ &
!                                XDMF_TOPOLOGY_TYPE_POLYVERTEX,      &
!                                XDMF_TOPOLOGY_TYPE_POLYLINE,        &
!                                XDMF_TOPOLOGY_TYPE_POLYGON,         &
                                XDMF_TOPOLOGY_TYPE_TRIANGLE,        &
                                XDMF_TOPOLOGY_TYPE_QUADRILATERAL,   &
                                XDMF_TOPOLOGY_TYPE_TETRAHEDRON,     &
                                XDMF_TOPOLOGY_TYPE_PYRAMID,         &
                                XDMF_TOPOLOGY_TYPE_WEDGE,           &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON,      &
                                XDMF_TOPOLOGY_TYPE_EDGE_3,          &
                                XDMF_TOPOLOGY_TYPE_TRIANGLE_6,      &
                                XDMF_TOPOLOGY_TYPE_QUADRILATERAL_8, &
                                XDMF_TOPOLOGY_TYPE_QUADRILATERAL_9, &
                                XDMF_TOPOLOGY_TYPE_TETRAHEDRON_10,  &
                                XDMF_TOPOLOGY_TYPE_PYRAMID_13,      &
                                XDMF_TOPOLOGY_TYPE_WEDGE_15,        &
                                XDMF_TOPOLOGY_TYPE_WEDGE_18,        &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_20,   &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_24,   &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_27,   &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_64,   &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_125,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_216,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_343,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_512,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_729,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1000, &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1331 &
!                                XDMF_TOPOLOGY_TYPE_MIXED            &
                                /)
        is_valid = MINVAL(ABS(allowed_TopologyTypes - TopologyType)) == 0_I4P
        if(.not. is_valid .and. this%warn) call warning_message('Wrong Topology Type: "'//trim(str(no_sign=.true., n=TopologyType))//'"')
    end function uniform_grid_descriptor_is_valid_TopologyType


    subroutine uniform_grid_descriptor_SetTopologyType(this, TopologyType)
    !-----------------------------------------------------------------  
    !< Set XDMF topology type
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this         !< Local grid derived type
        integer(I4P),                     intent(IN)    :: TopologyType !< XDMF topology type
    !-----------------------------------------------------------------
        if(this%is_valid_TopologyType(TopologyType)) then
            this%TopologyType = TopologyType
        else
            this%TopologyType = XDMF_NO_VALUE
        endif
    end subroutine uniform_grid_descriptor_SetTopologyType


    function uniform_grid_descriptor_GetTopologyType(this)
    !-----------------------------------------------------------------
    !< Return the XDMF topology type of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetTopologyType       !< XDMF Topology type
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetTopologyType = this%TopologyType
    end function uniform_grid_descriptor_GetTopologyType


    function uniform_grid_descriptor_is_valid_GeometryType(this, GeometryType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem NumberType
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t),  intent(IN) :: this                    !< Local Data Handler
        integer(I4P),                      intent(IN) :: GeometryType            !< XDMF Geometry Type
        logical                                       :: is_valid                !< Valid Geometry Type confirmation flag
        integer(I4P), allocatable                     :: allowed_GeometryTypes(:)!< Dataitem Geometry Type list 
    !----------------------------------------------------------------- 
        !< @Note: Mixed topologies or variable number of node elements not allowed
        !< @Note: mixed topologies can be implemented with and array of celltypes (offset?)
        !< @Note: variable number of nodes can be implemented with and array of number of nodes (offset?)
        allowed_GeometryTypes = (/ &
                                XDMF_GEOMETRY_TYPE_XYZ, &
                                XDMF_GEOMETRY_TYPE_XY   &
                                /)
        is_valid = MINVAL(ABS(allowed_GeometryTypes - GeometryType)) == 0_I4P
        if(.not. is_valid .and. this%warn) call warning_message('Wrong Geometry Type: "'//trim(str(no_sign=.true., n=GeometryType))//'"')
    end function uniform_grid_descriptor_is_valid_GeometryType


    subroutine uniform_grid_descriptor_SetGeometryType(this, GeometryType)
    !-----------------------------------------------------------------
    !< Set XDMF geometry type
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this         !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: GeometryType !< Local grid geometry type
    !----------------------------------------------------------------
        if(this%is_valid_GeometryType(GeometryType)) then
            this%GeometryType = GeometryType
        else
            this%GeometryType = XDMF_NO_VALUE
        endif
    end subroutine uniform_grid_descriptor_SetGeometryType


    function uniform_grid_descriptor_GetGeometryType(this)
    !-----------------------------------------------------------------
    !< Return XDMF geometry type
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                         !< Local grid descriptor
        integer(I4P)                         :: uniform_grid_descriptor_GetGeometryType !< XDMF geometry type
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGeometryType = this%GeometryType
    end function uniform_grid_descriptor_GetGeometryType


    subroutine uniform_grid_descriptor_initialize(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
    !-----------------------------------------------------------------
    !< Uniform grid descriptor initization procedure
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
        integer(I8P),                     intent(IN)    :: NumberOfNodes    !< Number of nodes of the local grid
        integer(I8P),                     intent(IN)    :: NumberOfElements !< Number of elements of the local grid
        integer(I4P),                     intent(IN)    :: TopologyType     !< Topology type of the local grid
        integer(I4P),                     intent(IN)    :: GeometryType     !< Geometry type of the local grid
    !-----------------------------------------------------------------
        call this%SetNumberOfNodes(NumberOfNodes=NumberOfNodes)
        call this%SetNumberOfElements(NumberOfElements=NumberOfElements)
        call this%SetTopologyType(TopologyType=TopologyType)
        call this%SetGeometryType(GeometryType=GeometryType)
    end subroutine uniform_grid_descriptor_initialize


    subroutine uniform_grid_descriptor_Free(this)
    !-----------------------------------------------------------------
    !< Free Uniform grid descriptor 
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
    !----------------------------------------------------------------- 
        ! No allocatable variables. Default initialization
        this%GridType           = XDMF_GRID_TYPE_UNSTRUCTURED
        this%GeometryType       = XDMF_GEOMETRY_TYPE_XYZ
        this%TopologyType       = XDMF_NO_VALUE
        this%NumberOfNodes      = XDMF_NO_VALUE
        this%NumberOfElements   = XDMF_NO_VALUE
    end subroutine uniform_grid_descriptor_Free


end module uniform_grid_descriptor
