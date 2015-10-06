module spatial_grid_descriptor
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use IR_Precision, only : I4P, I8P, R4P, R8P
use mpi_environment
use xdmf_utils
use XH5For_metadata

implicit none

private

    type spatial_grid_attribute_t
        integer(I4P)                          :: NumberOfAttributes = 0
        type(xh5for_metadata_t),  allocatable :: attributes_info(:)
    end type spatial_grid_attribute_t

    type :: spatial_grid_descriptor_t
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
    private
        integer(I4P)                                :: NumberOfGrids               !< Number of uniform grids of the spatial grid
        integer(I8P)                                :: GlobalNumberOfNodes = 0     !< Total number of nodes of the spatial grid
        integer(I8P)                                :: GlobalNumberOfElements = 0  !< Total number of elements of the spatial grid
        integer(I8P),                   allocatable :: NumberOfNodesPerGrid(:)     !< Array of number of nodes per grid
        integer(I8P),                   allocatable :: NumberOfElementsPerGrid(:)  !< Array of number of elements per grid
        type(xh5for_metadata_t),        allocatable :: TopologyPerGrid(:)          !< Array of Topology metadata per grid
        type(xh5for_metadata_t),        allocatable :: GeometryPerGrid(:)          !< Array of geometry metadata per grid
        type(spatial_grid_attribute_t), allocatable :: AttributesPerGrid(:)        !< Array of attribute metadata per grid
        type(mpi_env_t), pointer                    :: MPIEnvironment => null()    !< MPI environment 

    contains
    private
        procedure         :: Initialize_Writer              => spatial_grid_descriptor_Initialize_Writer
        procedure         :: Initialize_Reader              => spatial_grid_descriptor_Initialize_Reader
        procedure         :: SetGlobalNumberOfNodes         => spatial_grid_descriptor_SetGlobalNumberOfNodes
        procedure         :: SetGlobalNumberOfElements      => spatial_grid_descriptor_SetGlobalNumberOfElements
        procedure, public :: GetGlobalNumberOfNodes         => spatial_grid_descriptor_GetGlobalNumberOfNodes
        procedure, public :: GetGlobalNumberOfElements      => spatial_grid_descriptor_GetGlobalNumberOfElements
        procedure, public :: SetNumberOfNodesByGridID       => spatial_grid_descriptor_SetNumberOfNodesByGridID
        procedure, public :: SetNumberOfElementsByGridID    => spatial_grid_descriptor_SetNumberOfElementsByGridID
        procedure, public :: SetTopologyTypeByGridID        => spatial_grid_descriptor_SetTopologyTypeByGridID
        procedure, public :: SetTopologyXPathByGridID       => spatial_grid_descriptor_SetTopologyXPathByGridID
        procedure, public :: SetGeometryTypeByGridID        => spatial_grid_descriptor_SetGeometryTypeByGridID
        procedure, public :: SetGeometryXPathByGridID       => spatial_grid_descriptor_SetGeometryXPathByGridID
        procedure, public :: AllocateAttributesByGridID     => spatial_grid_descriptor_AllocateAttributesByGrid
        procedure, public :: SetAttributeTypeByGridID       => spatial_grid_descriptor_SetAttributeTypeByGridID
        procedure, public :: SetAttributeXPathByGridID      => spatial_grid_descriptor_SetAttributeXPathByGridID
        procedure, public :: SetAttributeCenterByGridID     => spatial_grid_descriptor_SetAttributeCenterByGridID
        procedure, public :: GetNumberOfNodesFromGridID     => spatial_grid_descriptor_GetNumberOfNodesFromGridID
        procedure, public :: GetNumberOfElementsFromGridID  => spatial_grid_descriptor_GetNumberOfElementsFromGridID
        procedure, public :: GetTopologyTypeFromGridID      => spatial_grid_descriptor_GetTopologyTypeFromGridID
        procedure, public :: GetTopologyXPathFromGridID     => spatial_grid_descriptor_GetTopologyXPathFromGridID
        procedure, public :: GetGeometryTypeFromGridID      => spatial_grid_descriptor_GetGeometryTypeFromGridID
        procedure, public :: GetGeometryXPathFromGridID     => spatial_grid_descriptor_GetGeometryXPathFromGridID
        procedure, public :: GetNodeOffsetFromGridID        => spatial_grid_descriptor_GetNodeOffsetFromGridID
        procedure, public :: GetElementOffsetFromGridID     => spatial_grid_descriptor_GetElementOffsetFromGridID
        generic,   public :: Initialize                     => Initialize_Writer, &
                                                               Initialize_Reader
        procedure, public :: Allocate                       => spatial_grid_descriptor_Allocate
        procedure, public :: DistributeData                 => spatial_grid_descriptor_DistributeData
        procedure, public :: Free                           => spatial_grid_descriptor_Free
    end type spatial_grid_descriptor_t

public :: spatial_grid_descriptor_t

contains

    subroutine spatial_grid_descriptor_Allocate(this, NumberOfGrids)
    !-----------------------------------------------------------------
    !< Set the total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this                 !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: NumberOfGrids        !< Total number of grids of the spatial grid
    !----------------------------------------------------------------- 
        this%NumberOfGrids = NumberOfGrids
        allocate(this%NumberOfNodesPerGrid(NumberOfGrids))
        allocate(this%NumberOfElementsPerGrid(NumberOfGrids))
        allocate(this%TopologyPerGrid(NumberOfGrids))
        allocate(this%GeometryPerGrid(NumberOfGrids))
        allocate(this%AttributesPerGrid(NumberOfGrids))
    end subroutine spatial_grid_descriptor_Allocate

    subroutine spatial_grid_descriptor_SetGlobalNumberOfNodes(this, GlobalNumberOfNodes)
    !-----------------------------------------------------------------
    !< Set the total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this                 !< Spatial grid descriptor type
        integer(I8P),                     intent(IN)    :: GlobalNumberOfNodes  !< Total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        this%GlobalNumberOfNodes = GlobalNumberOfNodes
    end subroutine spatial_grid_descriptor_SetGlobalNumberOfNodes


    function spatial_grid_descriptor_GetGlobalNumberOfNodes(this)
    !-----------------------------------------------------------------
    !< Return the total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this        !< Spatial grid descriptor type
        integer(I8P) :: spatial_grid_descriptor_GetGlobalNumberOfNodes !< Total number of nodes of the spatial grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetGlobalNumberOfNodes = this%GlobalNumberOfNodes
    end function spatial_grid_descriptor_GetGlobalNumberOfNodes


    subroutine spatial_grid_descriptor_SetGlobalNumberOfElements(this, GlobalNumberOfElements)
    !-----------------------------------------------------------------
    !< Set the total number of elements of the spatial grid
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this                   !< Spatial grid descriptor type
        integer(I8P),                     intent(IN)    :: GlobalNumberOfElements !< Total number of elements of the spatial grid
    !-----------------------------------------------------------------
        this%GlobalNumberOfElements = GlobalNumberOfelements
    end subroutine spatial_grid_descriptor_SetGlobalNumberOfElements


    function spatial_grid_descriptor_GetGlobalNumberOfElements(this)
    !-----------------------------------------------------------------
    !< Return the total number of elements of the spatial grid
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        integer(I8P) :: spatial_grid_descriptor_GetGlobalNumberOfElements !< Total number of elements of the spatial grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetGlobalNumberOfelements = this%GlobalNumberOfElements
    end function spatial_grid_descriptor_GetGlobalNumberOfElements


    subroutine spatial_grid_descriptor_SetNumberOfNodesByGridID(this, NumberOfNodes, ID)
    !-----------------------------------------------------------------
    !< Set the number of nodes of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        integer(I8P),                     intent(IN)    :: NumberOfNodes   !< Number of nodes of the grid ID
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
    !-----------------------------------------------------------------
        this%NumberOfNodesPerGrid(ID+1) = NumberOfNodes
    end subroutine spatial_grid_descriptor_SetNumberOfNodesByGridID


    function spatial_grid_descriptor_GetNumberOfNodesFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of nodes of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
        integer(I8P) :: spatial_grid_descriptor_GetNumberOfNodesFromGridID !< Number of nodes of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetNumberOfNodesFromGridID = this%NumberOfNodesPerGrid(ID+1)
    end function spatial_grid_descriptor_GetNumberOfNodesFromGridID


    subroutine spatial_grid_descriptor_SetNumberOfElementsByGridID(this, NumberOfElements, ID)
    !-----------------------------------------------------------------
    !< Set the number of nodes of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        integer(I8P),                     intent(IN)    :: NumberOfElements!< Number of elements of the grid ID
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
    !-----------------------------------------------------------------
        this%NumberOfElementsPerGrid(ID+1) = NumberOfElements
    end subroutine spatial_grid_descriptor_SetNumberOfElementsByGridID


    function spatial_grid_descriptor_GetNumberOfElementsFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of elements of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this               !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID                 !< Grid identifier
        integer(I8P) :: spatial_grid_descriptor_GetNumberOfElementsFromGridID !< Number of elements of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetNumberOfElementsFromGridID = this%NumberOfElementsPerGrid(ID+1)
    end function spatial_grid_descriptor_GetNumberOfElementsFromGridID


    subroutine spatial_grid_descriptor_SetTopologyTypeByGridID(this, TopologyType, ID)
    !-----------------------------------------------------------------
    !< Set the topology type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: TopologyType    !< Topology type of the grid ID
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
    !-----------------------------------------------------------------
        call this%TopologyPerGrid(ID+1)%setType(Type = TopologyType)
    end subroutine spatial_grid_descriptor_SetTopologyTypeByGridID


    subroutine spatial_grid_descriptor_SetTopologyXPathByGridID(this, XPath, ID)
    !-----------------------------------------------------------------
    !< Set the topology XPath of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        character(len=*),                 intent(IN)    :: XPath           !< Topology XPath of the grid ID
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
    !-----------------------------------------------------------------
        call this%TopologyPerGrid(ID+1)%setXPath(XPath = XPath)
    end subroutine spatial_grid_descriptor_SetTopologyXPathByGridID


    function spatial_grid_descriptor_GetTopologyTypeFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the topology type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID             !< Grid identifier
        integer(I4P) :: spatial_grid_descriptor_GetTopologyTypeFromGridID !< Topology type of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetTopologyTypeFromGridID = this%TopologyPerGrid(ID+1)%GetType()
    end function spatial_grid_descriptor_GetTopologyTypeFromGridID


    function spatial_grid_descriptor_GetTopologyXPathFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the topology XPath of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID             !< Grid identifier
        character(len=:), allocatable :: spatial_grid_descriptor_GetTopologyXPathFromGridID !< Topology XPath of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetTopologyxPathFromGridID = this%TopologyPerGrid(ID+1)%GetXPath()
    end function spatial_grid_descriptor_GetTopologyXPathFromGridID


    subroutine spatial_grid_descriptor_UpdateNumberOfAttributesByGrid(this, ID)
    !-----------------------------------------------------------------
    !< Increase the number of attributes and allocate the attributes_info array to the right size
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this       !< Uniform Grid Descriptor
        integer(I4P),                     intent(IN)    :: ID         !< Grid identifier
        type(xh5for_metadata_t),  allocatable   :: aux_attrs_info(:)  !< Aux XH5For attributes metadata
    !-----------------------------------------------------------------
        if(.not. allocated(this%AttributesPerGrid(ID+1)%attributes_info)) then
            this%AttributesPerGrid(ID+1)%NumberOfAttributes = 0
            allocate(this%AttributesPerGrid(ID+1)%attributes_info(1))
        elseif(size(this%AttributesPerGrid(ID+1)%attributes_info) < (this%AttributesPerGrid(ID+1)%NumberOfAttributes+1)) then
            allocate(aux_attrs_info(this%AttributesPerGrid(ID+1)%NumberOfAttributes))
            aux_attrs_info(:) = this%AttributesPerGrid(ID+1)%attributes_info(:)
            deallocate(this%AttributesPerGrid(ID+1)%attributes_info)
            allocate(this%AttributesPerGrid(ID+1)%attributes_info(this%AttributesPerGrid(ID+1)%NumberOfAttributes+1))
            this%AttributesPerGrid(ID+1)%attributes_info(1:this%AttributesPerGrid(ID+1)%NumberOfAttributes) = &
                                aux_attrs_info(1:this%AttributesPerGrid(ID+1)%NumberOfAttributes)
            deallocate(aux_attrs_info)
        endif
        this%AttributesPerGrid(ID)%NumberOfAttributes = this%AttributesPerGrid(ID)%NumberOfAttributes + 1
    end subroutine spatial_grid_descriptor_UpdateNumberOfAttributesByGrid


    subroutine spatial_grid_descriptor_AllocateAttributesByGrid(this, ID, NumberOfAttributes)
    !-----------------------------------------------------------------
    !< Increase the number of attributes and allocate the attributes_info array to the right size
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this               !< Uniform Grid Descriptor
        integer(I4P),                     intent(IN)    :: ID                 !< Grid identifier
        integer(I4P),                     intent(IN)    :: NumberOfAttributes !< NumberOfAttributes
        type(xh5for_metadata_t),  allocatable   :: aux_attrs_info(:)          !< Aux XH5For attributes metadata
    !-----------------------------------------------------------------
        if(allocated(this%AttributesPerGrid(ID+1)%attributes_info)) then
            deallocate(this%AttributesPerGrid(ID+1)%attributes_info)
            this%AttributesPerGrid(ID+1)%NumberOfAttributes = 0
        endif
        allocate(this%AttributesPerGrid(ID+1)%attributes_info(NumberOfAttributes))
        this%AttributesPerGrid(ID+1)%NumberOfAttributes = NumberOfAttributes
    end subroutine spatial_grid_descriptor_AllocateAttributesByGrid


    subroutine spatial_grid_descriptor_SetGeometryTypeByGridID(this, GeometryType, ID)
    !-----------------------------------------------------------------
    !< Set the topology type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: GeometryType    !< Geometry type of the grid ID
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
    !-----------------------------------------------------------------
        call this%GeometryPerGrid(ID+1)%SetType(Type = GeometryType)
    end subroutine spatial_grid_descriptor_SetGeometryTypeByGridID


    subroutine spatial_grid_descriptor_SetGeometryXPathByGridID(this, XPath, ID)
    !-----------------------------------------------------------------
    !< Set the topology XPath of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        character(len=*),                 intent(IN)    :: XPath           !< Geometry XPath of the grid ID
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
    !-----------------------------------------------------------------
        call this%GeometryPerGrid(ID+1)%setXPath(XPath = XPath)
    end subroutine spatial_grid_descriptor_SetGeometryXPathByGridID


    subroutine spatial_grid_descriptor_SetAttributeTypeByGridID(this, AttributeType, ID, NumberOfAttribute)
    !-----------------------------------------------------------------
    !< Set the attribute type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this              !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: AttributeType     !< Attribute type of the grid ID
        integer(I4P),                     intent(IN)    :: ID                !< Grid identifier
        integer(I4P),                     intent(IN)    :: NumberOfAttribute !< NumberOfAttribute
    !-----------------------------------------------------------------
        call this%AttributesPerGrid(ID+1)%Attributes_info(NumberOfAttribute)%SetType(Type = AttributeType)
    end subroutine spatial_grid_descriptor_SetAttributeTypeByGridID


    subroutine spatial_grid_descriptor_SetAttributeCenterByGridID(this, Center, ID, NumberOfAttribute)
    !-----------------------------------------------------------------
    !< Set the attribute center of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this              !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: Center            !< Attribute center of the grid ID
        integer(I4P),                     intent(IN)    :: ID                !< Grid identifier
        integer(I4P),                     intent(IN)    :: NumberOfAttribute !< NumberOfAttribute
    !-----------------------------------------------------------------
        call this%AttributesPerGrid(ID+1)%Attributes_info(NumberOfAttribute)%SetCenter(Center = Center)
    end subroutine spatial_grid_descriptor_SetAttributeCenterByGridID


    subroutine spatial_grid_descriptor_SetAttributeXPathByGridID(this, XPath, ID, NumberOfAttribute)
    !-----------------------------------------------------------------
    !< Set the topology XPath of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        character(len=*),                 intent(IN)    :: XPath           !< Topology XPath of the grid ID
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
        integer(I4P),                     intent(IN)    :: NumberOfAttribute !< NumberOfAttribute
    !-----------------------------------------------------------------
        call this%AttributesPerGrid(ID+1)%Attributes_info(NumberOfAttribute)%SetXPath(XPath = XPath)
    end subroutine spatial_grid_descriptor_SetAttributeXPathByGridID


    function spatial_grid_descriptor_GetGeometryTypeFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the geometry type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID             !< Grid identifier
        integer(I4P) :: spatial_grid_descriptor_GetGeometrytypeFromGridID !< Geometry type of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetGeometryTypeFromGridID = this%GeometryPerGrid(ID+1)%GetType()
    end function spatial_grid_descriptor_GetGeometryTypeFromGridID


    function spatial_grid_descriptor_GetGeometryXPathFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the geometry type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID             !< Grid identifier
        character(len=:), allocatable :: spatial_grid_descriptor_GetGeometryXPathFromGridID !< Geometry XPath of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetGeometryXPathFromGridID = this%GeometryPerGrid(ID+1)%GetXPath()
    end function spatial_grid_descriptor_GetGeometryXpathFromGridID


    function spatial_grid_descriptor_GetNodeOffsetFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the node offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this         !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID           !< Grid identifier
        integer(I8P) :: spatial_grid_descriptor_GetNodeOffsetFromGridID !< Node offset of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetNodeOffsetFromGridID = sum(this%NumberOfNodesPerGrid(:ID))
    end function spatial_grid_descriptor_GetNodeOffsetFromGridID


    function spatial_grid_descriptor_GetElementOffsetFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the element offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
        integer(I8P) :: spatial_grid_descriptor_GetElementOffsetFromGridID !< Element offset of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetElementOffsetFromGridID = sum(this%NumberOfElementsPerGrid(:ID))
    end function spatial_grid_descriptor_GetElementOffsetFromGridID


    subroutine spatial_grid_descriptor_Initialize_Writer(this, MPIEnvironment, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this                   !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment         !< MPI environment type
        integer(I8P),                     intent(IN)    :: NumberOfNodes          !< Number of nodes of the current grid
        integer(I8P),                     intent(IN)    :: NumberOfElements       !< Number of elements of the current grid
        integer(I4P),                     intent(IN)    :: TopologyType           !< Topology type of the current grid
        integer(I4P),                     intent(IN)    :: GeometryType           !< Geometry type of the current grid
        integer(I4P), allocatable                       :: TopologyTypePerGrid(:) !< Array of topology type per grid
        integer(I4P), allocatable                       :: GeometryTypePerGrid(:) !< Array of geometry type per grid
        integer(I4P)                                    :: i                      !< Loop index in NumberOfGrids
    !-----------------------------------------------------------------
        call this%Free()
        this%MPIEnvironment => MPIEnvironment
        call this%MPIEnvironment%mpi_allgather(NumberOfNodes, this%NumberOfNodesPerGrid)
        call this%MPIEnvironment%mpi_allgather(NumberOfElements, this%NumberOfElementsPerGrid)
        call this%MPIEnvironment%mpi_allgather(TopologyType, TopologyTypePerGrid)
        call this%MPIEnvironment%mpi_allgather(GeometryType, GeometryTypePerGrid)
        call this%SetGlobalNumberOfElements(sum(this%NumberOfElementsPerGrid))
        call this%SetGlobalNumberOfNodes(sum(this%NumberOfNodesPerGrid))
        this%NumberOfGrids = size(this%NumberOfNodesPerGrid, dim=1)
        allocate(this%TopologyPerGrid(this%NumberOfGrids))
        allocate(this%GeometryPerGrid(this%NumberOfGrids))
        allocate(this%AttributesPerGrid(this%NumberOfGrids))
        do i=1, this%NumberOfGrids
            call this%TopologyPerGrid(i)%SetType(Type = TopologyTypePerGrid(i))
            call this%GeometryPerGrid(i)%SetType(Type = GeometryTypePerGrid(i))
        enddo
    end subroutine spatial_grid_descriptor_Initialize_Writer


    subroutine spatial_grid_descriptor_Initialize_Reader(this, MPIEnvironment)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this                   !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment         !< MPI environment type
    !-----------------------------------------------------------------
        call this%Free()
        this%MPIEnvironment => MPIEnvironment
    end subroutine spatial_grid_descriptor_Initialize_Reader


    subroutine spatial_grid_descriptor_DistributeData(this)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this                   !< Spatial grid descriptor type
        integer(I4P),     allocatable                   :: TopologyTypePerGrid(:) !< Topology type for all  grids
        integer(I4P),     allocatable                   :: GeometryTypePerGrid(:) !< Geometry type for all  grids
        character(len=:), allocatable                   :: AllTopologyXPaths      !< Topologies XPath concatenated with &
        character(len=:), allocatable                   :: AllGeometryXPaths      !< Topologies XPath concatenated with &
        integer(I4P)                                    :: i                      !< Loop index in NumberOfGrids
        integer(I4P)                                    :: TopoPos                !< Position for Topology XPath token 
        integer(I4P)                                    :: GeoPos                 !< Position for Geometry XPath token 
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            allocate(TopologyTypePerGrid(this%NumberOfGrids))
            allocate(GeometryTypePerGrid(this%NumberOfGrids))
            AllTopologyXPaths=''
            AllGeometryXPaths=''
            do i=0, this%NumberOfGrids-1
                TopologyTypePerGrid(i+1) = this%GetTopologyTypeFromGridID(ID=i)
                geometryTypePerGrid(i+1) = this%GetGeometryTypeFromGridID(ID=i)
                AllTopologyXPaths = AllTopologyXPaths//'&'//this%GetTopologyXPathFromGridID(ID=i)
                AllGeometryXPaths = AllGeometryXPaths//'&'//this%GetGeometryXPathFromGridID(ID=i)
            enddo
        endif
        call this%MPIEnvironment%mpi_broadcast(this%NumberOfNodesPerGrid)
        call this%MPIEnvironment%mpi_broadcast(this%NumberOfElementsPerGrid)
        call this%MPIEnvironment%mpi_broadcast(TopologyTypePerGrid)
        call this%MPIEnvironment%mpi_broadcast(GeometryTypePerGrid)
        call this%MPIEnvironment%mpi_broadcast(AllTopologyXPaths)
        call this%MPIEnvironment%mpi_broadcast(AllGeometryXPaths)
        if(.not. this%MPIEnvironment%is_root()) then
            this%NumberOfGrids = size(this%NumberOfNodesPerGrid, dim=1)
            allocate(this%GeometryPerGrid(this%NumberOfGrids))
            allocate(this%TopologyPerGrid(this%NumberOfGrids))
            allocate(this%AttributesPerGrid(this%NumberOfGrids))
            TopoPos = 1
            GeoPos = 1
            do i=0, this%NumberOfGrids-1
                call this%SetTopologyTypeByGridID(ID = i, TopologyType = TopologyTypePerGrid(i+1))
                call this%SetTopologyXPathByGridID(ID = i, XPath = Next_Token(AllTopologyXPaths, TopoPos, '&'))
                call this%SetGeometryTypeByGridID(ID = i, GeometryType = GeometryTypePerGrid(i+1))
                call this%SetGeometryXPathByGridID(ID = i, XPath = Next_Token(AllGeometryXPaths, GeoPos, '&'))
            enddo
        endif
        call this%SetGlobalNumberOfElements(sum(this%NumberOfElementsPerGrid))
        call this%SetGlobalNumberOfNodes(sum(this%NumberOfNodesPerGrid))
        deallocate(TopologyTypePerGrid)
        deallocate(GeometryTypePerGrid)
        deallocate(AllTopologyXPaths)
        deallocate(AllGeometryXPaths)

    end subroutine spatial_grid_descriptor_DistributeData


    subroutine spatial_grid_descriptor_Free(this)
    !-----------------------------------------------------------------
    !< Free the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this       !< Spatial grid descriptor type
        integer(I4P)                                    :: i          !< Loop index in NumberOfGrids
        integer(I4P)                                    :: j          !< Loop index in NumberOfAttributes
    !----------------------------------------------------------------- 

        This%GlobalNumberOfNodes = 0
        This%GlobalNumberOfElements = 0
        if(allocated(this%NumberOfNodesPerGrid))    deallocate(this%NumberOfNodesPerGrid)
        if(allocated(this%NumberOfElementsPerGrid)) deallocate(this%NumberOfElementsPerGrid)
        if(allocated(this%TopologyPerGrid)) then
            do i=1, this%NumberOfGrids
                call this%TopologyPerGrid(i)%Free()
            enddo
            deallocate(this%TopologyPerGrid)
        endif
        if(allocated(this%GeometryPerGrid)) then
            do i=1, this%NumberOfGrids
                call this%GeometryPerGrid(i)%Free()
            enddo
            deallocate(this%GeometryPerGrid)
        endif
        if(allocated(this%AttributesPerGrid)) then
            do i=1, this%NumberOfGrids
                if(allocated(this%AttributesPerGrid(i)%Attributes_info)) then
                    do j=1, this%AttributesPerGrid(i)%NumberOfAttributes
                        call this%AttributesPerGrid(i)%Attributes_info(j)%Free
                    enddo
                    deallocate(this%AttributesPerGrid(i)%Attributes_info)
                endif
            enddo
            deallocate(this%AttributesPerGrid)
        endif
        nullify(this%MPIEnvironment)

    end subroutine spatial_grid_descriptor_Free


end module spatial_grid_descriptor
