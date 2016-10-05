module uniform_grid_descriptor

use PENF, only: I4P, I8P, str
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
        integer(I8P)                          :: TopologySize     = XDMF_NO_VALUE
        integer(I4P)                          :: NumberOfAttributes = 0
        type(xh5for_metadata_t)               :: GeometryMetadata
        type(xh5for_metadata_t)               :: TopologyMetadata
        type(xh5for_metadata_t),  allocatable :: AttributesMetadata(:)
    contains
    private
        procedure(uniform_grid_descriptor_Unstructured_Initialize), deferred :: Unstructured_Initialize
        procedure(uniform_grid_descriptor_Structured_Initialize),   deferred :: Structured_Initialize

        procedure, public :: Free                        => uniform_grid_descriptor_Free
        procedure, public :: FreeMetadata                => uniform_grid_descriptor_FreeMetadata
        procedure, public :: FreeAttributesMetadata      => uniform_grid_descriptor_FreeAttributesMetadata
        procedure, public :: SetGridType                 => uniform_grid_descriptor_SetGridType
        procedure, public :: SetNumberOfNodes            => uniform_grid_descriptor_SetNumberOfNodes
        procedure, public :: SetNumberOfElements         => uniform_grid_descriptor_SetNumberOfElements
        procedure, public :: SetTopologySize             => uniform_grid_descriptor_SetTopologySize
        procedure, public :: SetTopologyType             => uniform_grid_descriptor_SetTopologyType
        procedure, public :: SetGeometryType             => uniform_grid_descriptor_SetGeometryType
        procedure, public :: GetGridType                 => uniform_grid_descriptor_GetGridType
        procedure, public :: GetNumberOfNodes            => uniform_grid_descriptor_GetNumberOfNodes
        procedure, public :: GetNumberOfAttributes       => uniform_grid_descriptor_GetNumberOfAttributes
        procedure, public :: GetNumberOfElements         => uniform_grid_descriptor_GetNumberOfElements
        procedure, public :: GetTopologySize             => uniform_grid_descriptor_GetTopologySize
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
        procedure, public :: AppendAttributeMetadata     => uniform_grid_descriptor_AppendAttributeMetadata
        procedure         :: UpdateNumberOfAttributes    => uniform_grid_descriptor_UpdateNumberOfAttributes
        generic,   public :: Initialize                  => Unstructured_Initialize, &
                                                            Structured_Initialize
    end type uniform_grid_descriptor_t

    abstract interface
        
        subroutine uniform_grid_descriptor_unstructured_initialize(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType, GridType)
            import I4P
            import I8P
            import uniform_grid_descriptor_t 
            class(uniform_grid_descriptor_t), intent(INOUT) :: this
            integer(I8P),                     intent(IN)    :: NumberOfNodes
            integer(I8P),                     intent(IN)    :: NumberOfElements
            integer(I4P),                     intent(IN)    :: TopologyType
            integer(I4P),                     intent(IN)    :: GeometryType
            integer(I4P),                     intent(IN)    :: GridType
        end subroutine uniform_grid_descriptor_unstructured_initialize

        subroutine uniform_grid_descriptor_structured_initialize(this, Xdim, YDim, ZDim, GridType)
            import I4P
            import I8P
            import uniform_grid_descriptor_t
            class(uniform_grid_descriptor_t), intent(INOUT) :: this
            integer(I8P),                     intent(IN)    :: XDim
            integer(I8P),                     intent(IN)    :: YDim
            integer(I8P),                     intent(IN)    :: ZDim
            integer(I4P),                     intent(IN)    :: GridType
        end subroutine uniform_grid_descriptor_structured_initialize
    end interface

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


    subroutine uniform_grid_descriptor_SetTopologySize(this, TopologySize)
    !-----------------------------------------------------------------
    !< Set the size of the connectivities array
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
        integer(I8P),                     intent(IN)    :: TopologySize !< Size of the array of connectivities
    !-----------------------------------------------------------------
        this%TopologySize = TopologySize
    end subroutine uniform_grid_descriptor_SetTopologySize


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


    function uniform_grid_descriptor_GetTopologySize(this) result(TopologySize)
    !-----------------------------------------------------------------
    !< Get the size of the connectivities array
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this             !< Local grid descriptor
        integer(I8P)                                    :: TopologySize !< Size of the array of connectivities
    !-----------------------------------------------------------------
        TopologySize = this%TopologySize 
    end function uniform_grid_descriptor_GetTopologySize


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


    function uniform_grid_descriptor_GetTopologyArrayDimensions(this) result(ArrayDimensions)
    !-----------------------------------------------------------------
    !< Return XDMF topology Dimension
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this                !< Local grid descriptor
        integer(I4P), allocatable                       :: ArrayDimensions(:)  !< Topology Dimensions
    !-----------------------------------------------------------------
        call this%TopologyMetadata%GetArrayDimensions(ArrayDimensions = ArrayDimensions)
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


    function uniform_grid_descriptor_GetGeometryArrayDimensions(this) result(ArrayDimensions)
    !-----------------------------------------------------------------
    !< Return XDMF geometry Dimension
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this               !< Local grid descriptor
        integer(I4P), allocatable                       :: ArrayDimensions(:) !< Geometry Dimension
    !-----------------------------------------------------------------
        call this%GeometryMetadata%GetArrayDimensions(ArrayDimensions = ArrayDimensions)
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


    function uniform_grid_descriptor_GetAttributeArrayDimensions(this, AttributeNumber) result(ArrayDimensions)
    !-----------------------------------------------------------------
    !< Return XDMF Attribute Dimension
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this               !< Local grid descriptor
        integer(I4P),                     intent(IN)    :: AttributeNumber    !< Attribute Number
        integer(I4P), allocatable                       :: ArrayDimensions(:) !< Attribute Dimension
    !-----------------------------------------------------------------
        call this%AttributesMetadata(AttributeNumber)%GetArrayDimensions(ArrayDimensions = ArrayDimensions)
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


    subroutine uniform_grid_descriptor_SetGeometryMetadata(this, Name, Precision, ArrayDimensions)
    !-----------------------------------------------------------------
    !< Set Uniform Grid Descriptor geometry info
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Uniform Grid Descriptor 
        character(len=*),         intent(IN)    :: Name               !< Name to the HDF5 connetivities
        integer(I4P),             intent(IN)    :: Precision          !< Precision of the Coordinates in the HDF5 file
        integer(I8P),             intent(IN)    :: ArrayDimensions(:) !< Dimensions of the Coordinates array in the HDF5 file
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
        integer(I8P),             intent(IN)    :: ArrayDimensions(:) !< Dimensions of the coordinates array in the HDF5 file
    !-----------------------------------------------------------------
        call this%TopologyMetadata%SetName(Name = Name)
        call this%TopologyMetadata%SetPrecision(Precision = Precision)
        call this%TopologyMetadata%SetArrayDimensions(ArrayDimensions = ArrayDimensions)
    end subroutine uniform_grid_descriptor_SetTopologyMetadata


    subroutine uniform_grid_descriptor_AppendAttributeMetadata(this, Name, Type, DataType, Center, Precision, ArrayDimensions)
    !-----------------------------------------------------------------
    !< Set XH5For geometry info
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Uniform Grid Descriptor 
        character(len=*),         intent(IN)    :: Name               !< Name to the HDF5 coordinates
        integer(I4P),             intent(IN)    :: Type               !< XH5For attribute type (Scalar, Vector, Tensor, etc.)
        character(len=*),         intent(IN)    :: DataType           !< XH5For attribute data type (Int or  Float)
        integer(I4P),             intent(IN)    :: Center             !< Center property of the attribute (Node, Face, Edge, Cell or Grid)
        integer(I4P),             intent(IN)    :: Precision          !< Precision of the attribute in the HDF5 file
        integer(I8P),             intent(IN)    :: ArrayDimensions(:) !< Dimensions of the attribute array in the HDF5 file
    !-----------------------------------------------------------------
        call this%UpdateNumberOfAttributes()
        call this%AttributesMetadata(this%NumberOfAttributes)%SetName(Name = Name)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetType(Type = Type)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetDataType(DataType = DataType)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetCenter(Center = Center)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetPrecision(Precision = Precision)
        call this%AttributesMetadata(this%NumberOfAttributes)%SetArrayDimensions(ArrayDimensions = ArrayDimensions)
    end subroutine uniform_grid_descriptor_AppendAttributeMetadata


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


    subroutine uniform_grid_descriptor_FreeAttributesMetadata(this)
    !-----------------------------------------------------------------
    !< Free Uniform grid descriptor Attributes Metadata
    !----------------------------------------------------------------- 
        class(uniform_grid_descriptor_t), intent(INOUT) :: this       !< Local grid descriptor
        integer(I4P)                                    :: i          !< Index for to loop on attributes
    !----------------------------------------------------------------- 
        if(allocated(this%AttributesMetadata)) then
            do i = 1, this%NumberOfAttributes
                call this%AttributesMetadata(i)%Free()
            enddo
            deallocate(this%AttributesMetadata)
        endif
        this%NumberOfAttributes = 0
    end subroutine uniform_grid_descriptor_FreeAttributesMetadata


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
        call this%FreeAttributesMetadata()
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
