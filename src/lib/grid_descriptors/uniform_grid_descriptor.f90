module uniform_grid_descriptor

use IR_Precision, only: I4P, I8P, str
use XH5For_metadata
use XH5For_utils
use XH5For_parameters

implicit none

private

    type, abstract :: uniform_grid_descriptor_t
    private
    !-----------------------------------------------------------------
    !< Save local grid information
    !----------------------------------------------------------------- 
        integer(I4P)                          :: GridType         = XDMF_NO_VALUE
        integer(I8P)                          :: NumberOfNodes    = XDMF_NO_VALUE
        integer(I8P)                          :: NumberOfElements = XDMF_NO_VALUE
        integer(I8P)                          :: ConnectivitySize = XDMF_NO_VALUE
        integer(I4P)                          :: NumberOfAttributes = 0
        type(xh5for_metadata_t)               :: GeometryMetadata
        type(xh5for_metadata_t)               :: TopologyMetadata
        type(xh5for_metadata_t),  allocatable :: AttributesMetadata(:)
    contains
    private
        procedure, public :: Initialize                  => uniform_grid_descriptor_Initialize
        procedure, public :: Free                        => uniform_grid_descriptor_Free
        procedure, public :: FreeMetadata                => uniform_grid_descriptor_FreeMetadata
        procedure, public :: SetGridType                 => uniform_grid_descriptor_SetGridType
        procedure, public :: SetNumberOfNodes            => uniform_grid_descriptor_SetNumberOfNodes
        procedure, public :: SetNumberOfElements         => uniform_grid_descriptor_SetNumberOfElements
        procedure, public :: SetConnectivitySize         => uniform_grid_descriptor_SetConnectivitySize
        procedure, public :: SetTopologyType             => uniform_grid_descriptor_SetTopologyType
        procedure, public :: SetGeometryType             => uniform_grid_descriptor_SetGeometryType
        procedure, public :: GetGridType                 => uniform_grid_descriptor_GetGridType
        procedure, public :: GetNumberOfNodes            => uniform_grid_descriptor_GetNumberOfNodes
        procedure, public :: GetNumberOfAttributes       => uniform_grid_descriptor_GetNumberOfAttributes
        procedure, public :: GetNumberOfElements         => uniform_grid_descriptor_GetNumberOfElements
        procedure, public :: GetConnectivitySize         => uniform_grid_descriptor_GetConnectivitySize
        procedure, public :: GetTopologyName             => uniform_grid_descriptor_GetTopologyName
        procedure, public :: GetTopologyType             => uniform_grid_descriptor_GetTopologyType
        procedure, public :: GetTopologyPrecision        => uniform_grid_descriptor_GetTopologyPrecision
        procedure, public :: GetTopologyArrayDimensions  => uniform_grid_descriptor_GetTopologyArrayDimensions
        procedure, public :: GetGeometryName             => uniform_grid_descriptor_GetGeometryName
        procedure, public :: GetGeometryType             => uniform_grid_descriptor_GetGeometryType
        procedure, public :: GetGeometryPrecision        => uniform_grid_descriptor_GetGeometryPrecision
        procedure, public :: GetGeometryArrayDimensions  => uniform_grid_descriptor_GetGeometryArrayDimensions
        procedure, public :: GetAttributeName            => uniform_grid_descriptor_GetAttributeName
        procedure, public :: GetAttributeType            => uniform_grid_descriptor_GetAttributeType
        procedure, public :: GetAttributePrecision       => uniform_grid_descriptor_GetAttributePrecision
        procedure, public :: GetAttributeArrayDimensions => uniform_grid_descriptor_GetAttributeArrayDimensions
        procedure, public :: GetAttributeDataType        => uniform_grid_descriptor_GetAttributeDataType
        procedure, public :: GetAttributeCenter          => uniform_grid_descriptor_GetAttributeCenter
        procedure, public :: SetGeometryMetadata         => uniform_grid_descriptor_SetGeometryMetadata
        procedure, public :: SetTopologyMetadata         => uniform_grid_descriptor_SetTopologyMetadata
        procedure, public :: SetLastAttributeMetadata    => uniform_grid_descriptor_SetLastAttributeMetadata
        procedure, public :: UpdateNumberOfAttributes    => uniform_grid_descriptor_UpdateNumberOfAttributes
    end type uniform_grid_descriptor_t

public:: uniform_grid_descriptor_t

contains

    subroutine uniform_grid_descriptor_SetGridType(this, GridType)
    !-----------------------------------------------------------------
    !< Set the number of nodes of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: GridType   !< Grid Type
    !-----------------------------------------------------------------
        this%GridType = GridType
    end subroutine uniform_grid_descriptor_SetGridType


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


    subroutine uniform_grid_descriptor_SetConnectivitySize(this, ConnectivitySize)
    !-----------------------------------------------------------------
    !< Set the size of the connectivities array
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
        integer(I8P),                     intent(IN)    :: ConnectivitySize !< Size of the array of connectivities
    !-----------------------------------------------------------------
        this%ConnectivitySize = ConnectivitySize
    end subroutine uniform_grid_descriptor_SetConnectivitySize


    function uniform_grid_descriptor_GetGridType(this)
    !-----------------------------------------------------------------
    !< Return the number of nodes of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetGridType           !< Grid Type
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGridType = this%GridType
    end function uniform_grid_descriptor_GetGridType


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
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I8P) :: uniform_grid_descriptor_GetNumberOfElements   !< Number of elements of the local grid
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetNumberOfElements = this%NumberOfElements
    end function uniform_grid_descriptor_GetNumberOfElements


    function uniform_grid_descriptor_GetConnectivitySize(this) result(ConnectivitySize)
    !-----------------------------------------------------------------
    !< Get the size of the connectivities array
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
        integer(I8P)                                    :: ConnectivitySize !< Size of the array of connectivities
    !-----------------------------------------------------------------
        ConnectivitySize = this%ConnectivitySize 
    end function uniform_grid_descriptor_GetConnectivitySize


    subroutine uniform_grid_descriptor_SetTopologyType(this, TopologyType)
    !-----------------------------------------------------------------  
    !< Set XDMF topology type
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this         !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: TopologyType !< XDMF topology type
    !-----------------------------------------------------------------
        if(isSupportedTopologyType(TopologyType)) then
            call this%TopologyMetadata%SetType(Type = TopologyType)
        else
            call this%TopologyMetadata%SetType(Type = XDMF_NO_VALUE)
        endif
    end subroutine uniform_grid_descriptor_SetTopologyType


    function uniform_grid_descriptor_GetTopologyType(this)
    !-----------------------------------------------------------------
    !< Return the XDMF topology type of the local grid
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetTopologyType       !< XDMF Topology type
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetTopologyType = this%TopologyMetadata%GetType()
    end function uniform_grid_descriptor_GetTopologyType


    subroutine uniform_grid_descriptor_SetTopologyName(this, Name)
    !-----------------------------------------------------------------
    !< Set XDMF Topology Name
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        character(len=*),                 intent(IN)    :: Name       !< Topology Name
    !-----------------------------------------------------------------
        call this%TopologyMetadata%SetName(Name=Name)
    end subroutine uniform_grid_descriptor_SetTopologyName


    function uniform_grid_descriptor_GetTopologyName(this)
    !-----------------------------------------------------------------
    !< Return XDMF topology Name
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                  !< Local grid descriptor
        Character(len=:), allocatable :: uniform_grid_descriptor_GetTopologyName !< Topology Name
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetTopologyName = this%TopologyMetadata%GetName()
    end function uniform_grid_descriptor_GetTopologyName


    function uniform_grid_descriptor_GetTopologyPrecision(this)
    !-----------------------------------------------------------------
    !< Return XDMF topology Precision
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetTopologyPrecision  !< Topology Precision
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetTopologyPrecision = this%TopologyMetadata%GetPrecision()
    end function uniform_grid_descriptor_GetTopologyPrecision


    function uniform_grid_descriptor_GetTopologyArrayDimensions(this)
    !-----------------------------------------------------------------
    !< Return XDMF topology Dimension
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetTopologyArrayDimensions  !< Topology Dimension
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetTopologyArrayDimensions = this%TopologyMetadata%GetArrayDimensions()
    end function uniform_grid_descriptor_GetTopologyArrayDimensions


    subroutine uniform_grid_descriptor_SetGeometryType(this, GeometryType)
    !-----------------------------------------------------------------
    !< Set XDMF geometry type
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this         !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: GeometryType !< Local grid geometry type
    !----------------------------------------------------------------
        if(isSupportedGeometryType(GeometryType)) then
            call this%GeometryMetadata%SetType(Type=GeometryType)
        else
            call this%GeometryMetadata%SetType(Type=XDMF_NO_VALUE)
        endif
    end subroutine uniform_grid_descriptor_SetGeometryType


    function uniform_grid_descriptor_GetGeometryType(this)
    !-----------------------------------------------------------------
    !< Return XDMF geometry type
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P)  :: uniform_grid_descriptor_GetGeometryType      !< XDMF geometry type
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGeometryType = this%GeometryMetadata%GetType()
    end function uniform_grid_descriptor_GetGeometryType


    subroutine uniform_grid_descriptor_SetGeometryName(this, Name)
    !-----------------------------------------------------------------
    !< Set XDMF geometry Name
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        character(len=*),                 intent(IN)    :: Name       !< Geometry Name
    !-----------------------------------------------------------------
        call this%GeometryMetadata%SetName(Name=Name)
    end subroutine uniform_grid_descriptor_SetGeometryName


    function uniform_grid_descriptor_GetGeometryName(this)
    !-----------------------------------------------------------------
    !< Return XDMF geometry Name
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                  !< Local grid descriptor
        Character(len=:), allocatable :: uniform_grid_descriptor_GetGeometryName !< Geometry Name
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGeometryName = this%GeometryMetadata%GetName()
    end function uniform_grid_descriptor_GetGeometryName


    function uniform_grid_descriptor_GetGeometryPrecision(this)
    !-----------------------------------------------------------------
    !< Return XDMF geometry Precision
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetGeometryPrecision  !< Geometry Precision
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGeometryPrecision = this%GeometryMetadata%GetPrecision()
    end function uniform_grid_descriptor_GetGeometryPrecision


    function uniform_grid_descriptor_GetGeometryArrayDimensions(this)
    !-----------------------------------------------------------------
    !< Return XDMF geometry Dimension
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
        integer(I4P) :: uniform_grid_descriptor_GetGeometryArrayDimensions  !< Geometry Dimension
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetGeometryArrayDimensions = this%GeometryMetadata%GetArrayDimensions()
    end function uniform_grid_descriptor_GetGeometryArrayDimensions


    function uniform_grid_descriptor_GetAttributeType(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF Attribute type
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this            !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber !< Attribute Number
        integer(I4P)  :: uniform_grid_descriptor_GetAttributeType          !< XDMF attribute type
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeType = this%AttributesMetadata(AttributeNumber)%GetType()
    end function uniform_grid_descriptor_GetAttributeType


    function uniform_grid_descriptor_GetAttributeName(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF attribute Name
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                   !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber        !< Attribute Number
        Character(len=:), allocatable :: uniform_grid_descriptor_GetAttributeName !< Attribute Name
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeName = this%AttributesMetadata(AttributeNumber)%GetName()
    end function uniform_grid_descriptor_GetAttributeName


    function uniform_grid_descriptor_GetAttributeDataType(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF attribute DataType
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                       !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber            !< Attribute Number
        Character(len=:), allocatable :: uniform_grid_descriptor_GetAttributeDataType !< Attribute DataType
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeDataType = this%AttributesMetadata(AttributeNumber)%GetDataType()
    end function uniform_grid_descriptor_GetAttributeDataType


    function uniform_grid_descriptor_GetAttributePrecision(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF Attribute Precision
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this            !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber !< Attribute Number
        integer(I4P) :: uniform_grid_descriptor_GetAttributePrecision      !< Attribute Precision
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributePrecision = this%AttributesMetadata(AttributeNumber)%GetPrecision()
    end function uniform_grid_descriptor_GetAttributePrecision


    function uniform_grid_descriptor_GetAttributeArrayDimensions(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF Attribute Dimension
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber  !< Attribute Number
        integer(I4P) :: uniform_grid_descriptor_GetAttributeArrayDimensions !< Attribute Dimension
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeArrayDimensions = this%AttributesMetadata(AttributeNumber)%GetArrayDimensions()
    end function uniform_grid_descriptor_GetAttributeArrayDimensions


    function uniform_grid_descriptor_GetAttributeCenter(this, AttributeNumber)
    !-----------------------------------------------------------------
    !< Return XDMF Attribute Center
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this            !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber !< Attribute Number
        integer(I4P) :: uniform_grid_descriptor_GetAttributeCenter         !< Attribute Center
    !-----------------------------------------------------------------
        uniform_grid_descriptor_GetAttributeCenter = this%AttributesMetadata(AttributeNumber)%GetCenter()
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


    subroutine uniform_grid_descriptor_SetGeometryMetadata(this, Name, Precision, ArrayDimensions)
    !-----------------------------------------------------------------
    !< Set Uniform Grid Descriptor geometry info
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Uniform Grid Descriptor 
        character(len=*),         intent(IN)    :: Name               !< Name to the HDF5 connetivities
        integer(I4P),             intent(IN)    :: Precision          !< Precision of the Coordinates in the HDF5 file
        integer(I4P),             intent(IN)    :: ArrayDimensions    !< Dimensions of the Coordinates array in the HDF5 file
    !-----------------------------------------------------------------
        call this%GeometryMetadata%SetName(Name = Name)
        call this%GeometryMetadata%SetPrecision(Precision = Precision)
        call this%GeometryMetadata%SetArrayDimensions(ArrayDimensions = ArrayDimensions)
    end subroutine uniform_grid_descriptor_SetGeometryMetadata


    subroutine uniform_grid_descriptor_SetTopologyMetadata(this, Name, Precision, ArrayDimensions)
    !-----------------------------------------------------------------
    !< Set Uniform Grid Descriptor topology info
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Uniform Grid Descriptor 
        character(len=*),         intent(IN)    :: Name               !< Name to the HDF5 coordinates
        integer(I4P),             intent(IN)    :: Precision          !< Precision of the coordinates in the HDF5 file
        integer(I4P),             intent(IN)    :: ArrayDimensions    !< Dimensions of the coordinates array in the HDF5 file
    !-----------------------------------------------------------------
        call this%TopologyMetadata%SetName(Name = Name)
        call this%TopologyMetadata%SetPrecision(Precision = Precision)
        call this%TopologyMetadata%SetArrayDimensions(ArrayDimensions = ArrayDimensions)
    end subroutine uniform_grid_descriptor_SetTopologyMetadata


    subroutine uniform_grid_descriptor_SetLastAttributeMetadata(this, Name, Type, DataType, Center, Precision, ArrayDimensions)
    !-----------------------------------------------------------------
    !< Set XH5For geometry info
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Uniform Grid Descriptor 
        character(len=*),         intent(IN)    :: Name               !< Name to the HDF5 coordinates
        integer(I4P),             intent(IN)    :: Type               !< XH5For attribute type (Scalar, Vector, Tensor, etc.)
        character(len=*),         intent(IN)    :: DataType           !< XH5For attribute data type (Int or  Float)
        integer(I4P),             intent(IN)    :: Center             !< Center property of the attribute (Node, Face, Edge, Cell or Grid)
        integer(I4P),             intent(IN)    :: Precision          !< Precision of the attribute in the HDF5 file
        integer(I4P),             intent(IN)    :: ArrayDimensions    !< Dimensions of the attribute array in the HDF5 file
    !-----------------------------------------------------------------
        call this%AttributesMetadata(this%NumberOfAttributes)%SetName(Name = Name)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetType(Type = Type)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetDataType(DataType = DataType)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetCenter(Center = Center)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetPrecision(Precision = Precision)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetArrayDimensions(ArrayDimensions = ArrayDimensions)
    end subroutine uniform_grid_descriptor_SetLastAttributeMetadata


    subroutine uniform_grid_descriptor_UpdateNumberOfAttributes(this)
    !-----------------------------------------------------------------
    !< Increase the number of attributes and allocate the AttributesMetadata array to the right size
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Uniform Grid Descriptor
        type(xh5for_metadata_t),  allocatable   :: aux_attrs_info(:)  !< Aux XH5For attributes metadata
    !-----------------------------------------------------------------
        if(.not. allocated(this%AttributesMetadata)) then
            this%NumberOfAttributes = 0
            allocate(this%AttributesMetadata(1))
        elseif(size(this%AttributesMetadata) < (this%NumberOfAttributes+1)) then
            allocate(aux_attrs_info(this%NumberOfAttributes))
            aux_attrs_info(:) = this%AttributesMetadata(:)
            deallocate(this%AttributesMetadata); allocate(this%AttributesMetadata(this%NumberOfAttributes+1))
            this%AttributesMetadata(1:this%NumberOfAttributes) = aux_attrs_info(1:this%NumberOfAttributes)
            deallocate(aux_attrs_info)
        endif
        this%NumberOfAttributes = this%NumberOfAttributes + 1
    end subroutine uniform_grid_descriptor_UpdateNumberOfAttributes


    subroutine uniform_grid_descriptor_FreeMetadata(this)
    !-----------------------------------------------------------------
    !< Free Uniform grid descriptor Metadata
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P)                                    :: i          !< Index for to loop on attributes
    !----------------------------------------------------------------- 
        this%GridType           = XDMF_NO_VALUE
        this%NumberOfNodes      = XDMF_NO_VALUE
        this%NumberOfElements   = XDMF_NO_VALUE
        if(allocated(this%AttributesMetadata)) then
            do i = 1, this%NumberOfAttributes
                call this%AttributesMetadata(i)%Free()
            enddo
            deallocate(this%AttributesMetadata)
        endif
        this%NumberOfAttributes = 0
        call this%GeometryMetadata%Free()
        call this%TopologyMetadata%Free()
    end subroutine uniform_grid_descriptor_FreeMetadata


    subroutine uniform_grid_descriptor_Free(this)
    !-----------------------------------------------------------------
    !< Free Uniform grid descriptor 
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
    !----------------------------------------------------------------- 
        call this%FreeMetadata()
    end subroutine uniform_grid_descriptor_Free


end module uniform_grid_descriptor
