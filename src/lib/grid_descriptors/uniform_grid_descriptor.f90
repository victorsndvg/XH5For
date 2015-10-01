module uniform_grid_descriptor

use IR_Precision, only: I4P, I8P, str
use xdmf_utils,   only : warning_message
use XH5For_metadata
use XH5For_parameters

implicit none

private

    type :: uniform_grid_descriptor_t
    private
    !-----------------------------------------------------------------
    !< Save local grid information
    !----------------------------------------------------------------- 
        integer(I4P)                          :: GridType           = XDMF_GRID_TYPE_UNSTRUCTURED
        integer(I8P)                          :: NumberOfNodes      = XDMF_NO_VALUE
        integer(I8P)                          :: NumberOfElements   = XDMF_NO_VALUE
        integer(I4P)                          :: NumberOfAttributes = 0
        type(xh5for_metadata_t)               :: geometry_info
        type(xh5for_metadata_t)               :: topology_info
        type(xh5for_metadata_t),  allocatable :: attributes_info(:)
        logical      :: warn = .true.  !< Flag to show warnings on screen
    contains
    private
        procedure         :: is_valid_TopologyType    => uniform_grid_descriptor_is_valid_TopologyType
        procedure         :: is_valid_GeometryType    => uniform_grid_descriptor_is_valid_GeometryType
        procedure, public :: Initialize               => uniform_grid_descriptor_Initialize
        procedure, public :: Free                     => uniform_grid_descriptor_Free
        procedure, public :: SetNumberOfNodes         => uniform_grid_descriptor_SetNumberOfNodes
        procedure, public :: SetNumberOfElements      => uniform_grid_descriptor_SetNumberOfElements
        procedure, public :: SetTopologyType          => uniform_grid_descriptor_SetTopologyType
        procedure, public :: SetGeometryType          => uniform_grid_descriptor_SetGeometryType
        procedure, public :: GetNumberOfNodes         => uniform_grid_descriptor_GetNumberOfNodes
        procedure, public :: GetNumberOfAttributes    => uniform_grid_descriptor_GetNumberOfAttributes
        procedure, public :: GetNumberOfElements      => uniform_grid_descriptor_GetNumberOfElements
        procedure, public :: GetTopologyXPath         => uniform_grid_descriptor_GetTopologyXPath
        procedure, public :: GetTopologyType          => uniform_grid_descriptor_GetTopologyType
        procedure, public :: GetTopologyPrecision     => uniform_grid_descriptor_GetTopologyPrecision
        procedure, public :: GetTopologyDimension     => uniform_grid_descriptor_GetTopologyDimension
        procedure, public :: GetGeometryXPath         => uniform_grid_descriptor_GetGeometryXPath
        procedure, public :: GetGeometryType          => uniform_grid_descriptor_GetGeometryType
        procedure, public :: GetGeometryPrecision     => uniform_grid_descriptor_GetGeometryPrecision
        procedure, public :: GetGeometryDimension     => uniform_grid_descriptor_GetGeometryDimension
        procedure, public :: GetAttributeXPath        => uniform_grid_descriptor_GetAttributeXPath
        procedure, public :: GetAttributeType         => uniform_grid_descriptor_GetAttributeType
        procedure, public :: GetAttributePrecision    => uniform_grid_descriptor_GetAttributePrecision
        procedure, public :: GetAttributeDimension    => uniform_grid_descriptor_GetAttributeDimension
        procedure, public :: GetAttributeDataType     => uniform_grid_descriptor_GetAttributeDataType
        procedure, public :: GetAttributeCenter       => uniform_grid_descriptor_GetAttributeCenter
        procedure, public :: SetGeometryInfo          => uniform_grid_descriptor_SetGeometryInfo
        procedure, public :: SetTopologyInfo          => uniform_grid_descriptor_SetTopologyInfo
        procedure, public :: SetLastAttributeInfo     => uniform_grid_descriptor_SetLastAttributeInfo
        procedure, public :: UpdateNumberOfAttributes => uniform_grid_descriptor_UpdateNumberOfAttributes
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


    function uniform_grid_descriptor_GetNumberOfAttributes(this)
    !-----------------------------------------------------------------
    !< Return the number of attributes of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I8P) :: uniform_grid_descriptor_getNumberOfAttributes !< Number of attributes of the local grid
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetNumberOfAttributes = this%NumberOfAttributes
    end function uniform_grid_descriptor_GetNumberOfAttributes


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
            call this%topology_info%SetType(Type = TopologyType)
        else
            call this%topology_info%SetType(Type = XDMF_NO_VALUE)
        endif
    end subroutine uniform_grid_descriptor_SetTopologyType


    function uniform_grid_descriptor_GetTopologyType(this)
    !-----------------------------------------------------------------
    !< Return the XDMF topology type of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetTopologyType       !< XDMF Topology type
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetTopologyType = this%Topology_info%GetType()
    end function uniform_grid_descriptor_GetTopologyType


    function uniform_grid_descriptor_GetTopologyXPath(this)
    !-----------------------------------------------------------------
    !< Return XDMF topology XPath
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                   !< Local grid descriptor
        Character(len=:), allocatable :: uniform_grid_descriptor_GetTopologyXPath !< Topology XPath
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetTopologyXPath = this%tOPOLOGY_info%GetXPath()
    end function uniform_grid_descriptor_GetTopologyXPath


    function uniform_grid_descriptor_GetTopologyPrecision(this)
    !-----------------------------------------------------------------
    !< Return XDMF topology Precision
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetTopologyPrecision  !< Topology Precision
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetTopologyPrecision = this%Topology_info%GetPrecision()
    end function uniform_grid_descriptor_GetTopologyPrecision


    function uniform_grid_descriptor_GetTopologyDimension(this)
    !-----------------------------------------------------------------
    !< Return XDMF topology Dimension
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetTopologyDimension  !< Topology Dimension
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetTopologyDimension = this%Topology_info%GetDimension()
    end function uniform_grid_descriptor_GetTopologyDimension


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
            call this%Geometry_info%SetType(Type=GeometryType)
        else
            call this%Geometry_info%SetType(Type=XDMF_NO_VALUE)
        endif
    end subroutine uniform_grid_descriptor_SetGeometryType


    function uniform_grid_descriptor_GetGeometryType(this)
    !-----------------------------------------------------------------
    !< Return XDMF geometry type
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P)  :: uniform_grid_descriptor_GetGeometryType      !< XDMF geometry type
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGeometryType = this%Geometry_info%GetType()
    end function uniform_grid_descriptor_GetGeometryType


    function uniform_grid_descriptor_GetGeometryXPath(this)
    !-----------------------------------------------------------------
    !< Return XDMF geometry XPath
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                   !< Local grid descriptor
        Character(len=:), allocatable :: uniform_grid_descriptor_GetGeometryXPath !< Geometry XPath
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGeometryXPath = this%Geometry_info%GetXPath()
    end function uniform_grid_descriptor_GetGeometryXPath


    function uniform_grid_descriptor_GetGeometryPrecision(this)
    !-----------------------------------------------------------------
    !< Return XDMF geometry Precision
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetGeometryPrecision  !< Geometry Precision
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGeometryPrecision = this%Geometry_info%GetPrecision()
    end function uniform_grid_descriptor_GetGeometryPrecision


    function uniform_grid_descriptor_GetGeometryDimension(this)
    !-----------------------------------------------------------------
    !< Return XDMF geometry Dimension
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetGeometryDimension  !< Geometry Dimension
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGeometryDimension = this%Geometry_info%GetDimension()
    end function uniform_grid_descriptor_GetGeometryDimension


    function uniform_grid_descriptor_GetAttributeType(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF Attribute type
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this            !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber !< Attribute Number
        integer(I4P)  :: uniform_grid_descriptor_GetAttributeType          !< XDMF attribute type
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeType = this%Attributes_info(AttributeNumber)%GetType()
    end function uniform_grid_descriptor_GetAttributeType


    function uniform_grid_descriptor_GetAttributeXPath(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF attribute XPath
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                    !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber         !< Attribute Number
        Character(len=:), allocatable :: uniform_grid_descriptor_GetAttributeXPath !< Attribute XPath
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeXPath = this%Attributes_info(AttributeNumber)%GetXPath()
    end function uniform_grid_descriptor_GetAttributeXPath


    function uniform_grid_descriptor_GetAttributeDataType(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF attribute DataType
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                       !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber            !< Attribute Number
        Character(len=:), allocatable :: uniform_grid_descriptor_GetAttributeDataType !< Attribute DataType
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeDataType = this%Attributes_info(AttributeNumber)%GetDataType()
    end function uniform_grid_descriptor_GetAttributeDataType


    function uniform_grid_descriptor_GetAttributePrecision(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF Attribute Precision
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this            !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber !< Attribute Number
        integer(I4P) :: uniform_grid_descriptor_GetAttributePrecision      !< Attribute Precision
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributePrecision = this%Attributes_info(AttributeNumber)%GetPrecision()
    end function uniform_grid_descriptor_GetAttributePrecision


    function uniform_grid_descriptor_GetAttributeDimension(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF Attribute Dimension
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this            !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber !< Attribute Number
        integer(I4P) :: uniform_grid_descriptor_GetAttributeDimension      !< Attribute Dimension
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeDimension = this%Attributes_info(AttributeNumber)%GetDimension()
    end function uniform_grid_descriptor_GetAttributeDimension


    function uniform_grid_descriptor_GetAttributeCenter(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF Attribute Center
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this            !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber !< Attribute Number
        integer(I4P) :: uniform_grid_descriptor_GetAttributeCenter         !< Attribute Center
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeCenter = this%Attributes_info(AttributeNumber)%GetCenter()
    end function uniform_grid_descriptor_GetAttributeCenter


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


    subroutine uniform_grid_descriptor_SetGeometryInfo(this, XPath, Precision, Dimension)
    !-----------------------------------------------------------------
    !< Set Uniform Grid Descriptor geometry info
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this               !< Uniform Grid Descriptor 
        character(len=*),         intent(IN)    :: XPath              !< XPath to the HDF5 connetivities
        integer(I4P),             intent(IN)    :: Precision          !< Precision of the Coordinates in the HDF5 file
        integer(I4P),             intent(IN)    :: Dimension          !< Dimensions of the Coordinates array in the HDF5 file
    !-----------------------------------------------------------------
        call this%geometry_info%SetXPath(XPath = XPath)
        call this%geometry_info%SetPrecision(Precision = Precision)
        call this%geometry_info%SetDimension(Dimension = Dimension)
    end subroutine uniform_grid_descriptor_SetGeometryInfo


    subroutine uniform_grid_descriptor_SetTopologyInfo(this, XPath, Precision, Dimension)
    !-----------------------------------------------------------------
    !< Set Uniform Grid Descriptor topology info
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this               !< Uniform Grid Descriptor 
        character(len=*),         intent(IN)    :: XPath              !< XPath to the HDF5 coordinates
        integer(I4P),             intent(IN)    :: Precision          !< Precision of the coordinates in the HDF5 file
        integer(I4P),             intent(IN)    :: Dimension          !< Dimensions of the coordinates array in the HDF5 file
    !-----------------------------------------------------------------
        call this%topology_info%SetXPath(XPath = XPath)
        call this%topology_info%SetPrecision(Precision = Precision)
        call this%topology_info%SetDimension(Dimension = Dimension)
    end subroutine uniform_grid_descriptor_SetTopologyInfo


    subroutine uniform_grid_descriptor_SetLastAttributeInfo(this, XPath, Type, DataType, Center, Precision, Dimension)
    !-----------------------------------------------------------------
    !< Set XH5For geometry info
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this               !< Uniform Grid Descriptor 
        character(len=*),         intent(IN)    :: XPath              !< XPath to the HDF5 coordinates
        integer(I4P),             intent(IN)    :: Type               !< XH5For attribute type (Scalar, Vector, Tensor, etc.)
        character(len=*),         intent(IN)    :: DataType           !< XH5For attribute data type (Int or  Float)
        integer(I4P),             intent(IN)    :: Center             !< Center property of the attribute (Node, Face, Edge, Cell or Grid)
        integer(I4P),             intent(IN)    :: Precision          !< Precision of the attribute in the HDF5 file
        integer(I4P),             intent(IN)    :: Dimension          !< Dimensions of the attribute array in the HDF5 file
    !-----------------------------------------------------------------
        call this%attributes_info(this%NumberOfAttributes)%SetXPath(XPath = XPath)
        call this%attributes_info(this%NumberOfAttributes)%SetType(Type = Type)
        call this%attributes_info(this%NumberOfAttributes)%SetDataType(DataType = DataType)
        call this%attributes_info(this%NumberOfAttributes)%SetCenter(Center = Center)
        call this%attributes_info(this%NumberOfAttributes)%SetPrecision(Precision = Precision)
        call this%attributes_info(this%NumberOfAttributes)%SetDimension(Dimension = Dimension)
    end subroutine uniform_grid_descriptor_SetLastAttributeInfo


    subroutine uniform_grid_descriptor_UpdateNumberOfAttributes(this)
    !-----------------------------------------------------------------
    !< Increase the number of attributes and allocate the attributes_info array to the right size
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Uniform Grid Descriptor
        type(xh5for_metadata_t),  allocatable   :: aux_attrs_info(:)  !< Aux XH5For attributes metadata
    !-----------------------------------------------------------------
        if(.not. allocated(this%attributes_info)) then
            this%NumberOfAttributes = 0
            allocate(this%attributes_info(1))
        elseif(size(this%attributes_info) < (this%NumberOfAttributes+1)) then
            allocate(aux_attrs_info(this%NumberOfAttributes))
            aux_attrs_info(:) = this%attributes_info(:)
            deallocate(this%attributes_info); allocate(this%attributes_info(this%NumberOfAttributes+1))
            this%attributes_info(1:this%NumberOfAttributes) = aux_attrs_info(1:this%NumberOfAttributes)
            deallocate(aux_attrs_info)
        endif
        this%NumberOfAttributes = this%NumberOfAttributes + 1
    end subroutine uniform_grid_descriptor_UpdateNumberOfAttributes


    subroutine uniform_grid_descriptor_Free(this)
    !-----------------------------------------------------------------
    !< Free Uniform grid descriptor 
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P)                                    :: i          !< Index for to loop on attributes
    !----------------------------------------------------------------- 
        ! No allocatable variables. Default initialization
        this%GridType           = XDMF_GRID_TYPE_UNSTRUCTURED
        this%NumberOfNodes      = XDMF_NO_VALUE
        this%NumberOfElements   = XDMF_NO_VALUE
        if(allocated(this%Attributes_info)) then
            do i = 1, this%NumberOfAttributes
                call this%Attributes_info(i)%Free()
            enddo
            deallocate(this%Attributes_info)
        endif
        this%NumberOfAttributes = 0
        call this%Geometry_info%Free()
        call this%Topology_info%Free()
    end subroutine uniform_grid_descriptor_Free


end module uniform_grid_descriptor
