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

    type :: spatial_grid_attribute_t
        integer(I4P)                          :: NumberOfAttributes = 0
        type(xh5for_metadata_t),  allocatable :: attributes_info(:)
    end type spatial_grid_attribute_t

    type, abstract :: spatial_grid_descriptor_t
    private
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
        integer(I4P)                                :: NumberOfGrids          = 0  !< Number of uniform grids of the spatial grid
        integer(I8P)                                :: GlobalNumberOfNodes    = 0  !< Total number of nodes of the spatial grid
        integer(I8P)                                :: GlobalNumberOfElements = 0  !< Total number of elements of the spatial grid
        integer(I8P),                   allocatable :: NumberOfNodesPerGrid(:)     !< Array of number of nodes per grid
        integer(I8P),                   allocatable :: NumberOfElementsPerGrid(:)  !< Array of number of elements per grid
        integer(I4P),                   allocatable :: GeometryTypePerGrid(:)      !< Array of geometry type per grid
        integer(I4P),                   allocatable :: TopologyTypePerGrid(:)      !< Array of topology type per grid
        type(mpi_env_t), pointer                    :: MPIEnvironment => null()    !< MPI environment 

    contains

        procedure(spatial_grid_descriptor_BroadcastMetadata),              deferred :: BroadcastMetadata

        procedure(spatial_grid_descriptor_SetTopologySizePerGridID),       deferred :: SetTopologySizePerGridID
        procedure(spatial_grid_descriptor_GetTopologySizePerGridID),       deferred :: GetTopologySizePerGridID
        procedure(spatial_grid_descriptor_GetTopologySizeOffsetPerGridID), deferred :: GetTopologySizeOffsetPerGridID
        procedure(spatial_grid_descriptor_SetGlobalTopologySize),          deferred :: SetGlobalTopologySize
        procedure(spatial_grid_descriptor_GetGlobalTopologySize),          deferred :: GetGlobalTopologySize

        procedure(spatial_grid_descriptor_SetGeometrySizePerGridID),       deferred :: SetGeometrySizePerGridID
        procedure(spatial_grid_descriptor_GetGeometrySizePerGridID),       deferred :: GetGeometrySizePerGridID
        procedure(spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID), deferred :: GetGeometrySizeOffsetPerGridID
        procedure(spatial_grid_descriptor_SetGlobalGeometrySize),          deferred :: SetGlobalGeometrySize
        procedure(spatial_grid_descriptor_GetGlobalGeometrySize),          deferred :: GetGlobalGeometrySize

        procedure :: SetNumberOfGrids                   => spatial_grid_descriptor_SetNumberOfGrids
        procedure :: GetNumberOfGrids                   => spatial_grid_descriptor_GetNumberOfGrids

        procedure :: NullifyMPIEnvironment              => spatial_grid_descriptor_NullifyMPIEnvironment
        procedure :: SetMPIEnvironment                  => spatial_grid_descriptor_SetMPIEnvironment
        procedure :: GetMPIEnvironment                  => spatial_grid_descriptor_GetMPIEnvironment

        procedure :: AllocateNumberOfNodesPerGrid       => spatial_grid_descriptor_AllocateNumberOfNodesPerGrid
        procedure :: DeallocateNumberOfNodesPerGrid     => spatial_grid_descriptor_DeallocateNumberOfNodesPerGrid
        procedure :: SetNumberOfNodesPerGridID          => spatial_grid_descriptor_SetNumberOfNodesPerGridID
        procedure :: GetNumberOfNodesPerGridID          => spatial_grid_descriptor_GetNumberOfNodesPerGridID
        procedure :: SetGlobalNumberOfNodes             => spatial_grid_descriptor_SetGlobalNumberOfNodes
        procedure :: GetGlobalNumberOfNodes             => spatial_grid_descriptor_GetGlobalNumberOfNodes

        procedure :: AllocateNumberOfElementsPerGrid    => spatial_grid_descriptor_AllocateNumberOfElementsPerGrid
        procedure :: DeallocateNumberOfElementsPerGrid  => spatial_grid_descriptor_DeallocateNumberOfElementsPerGrid
        procedure :: SetNumberOfElementsPerGridID       => spatial_grid_descriptor_SetNumberOfElementsPerGridID
        procedure :: GetNumberOfElementsPerGridID       => spatial_grid_descriptor_GetNumberOfElementsPerGridID
        procedure :: SetGlobalNumberOfElements          => spatial_grid_descriptor_SetGlobalNumberOfElements
        procedure :: GetGlobalNumberOfElements          => spatial_grid_descriptor_GetGlobalNumberOfElements

        procedure :: AllocateTopologyTypePerGrid        => spatial_grid_descriptor_AllocateTopologyTypePerGrid
        procedure :: DeallocateTopologyTypePerGrid      => spatial_grid_descriptor_DeallocateTopologyTypePerGrid
        procedure :: SetTopologyTypePerGridID           => spatial_grid_descriptor_SetTopologyTypePerGridID
        procedure :: GetTopologyTypePerGridID           => spatial_grid_descriptor_GetTopologyTypePerGridID

        procedure :: AllocateGeometryTypePerGrid        => spatial_grid_descriptor_AllocateGeometryTypePerGrid
        procedure :: DeallocateGeometryTypePerGrid      => spatial_grid_descriptor_DeallocateGeometryTypePerGrid
        procedure :: SetGeometryTypePerGridID           => spatial_grid_descriptor_SetGeometryTypePerGridID
        procedure :: GetGeometryTypePerGridID           => spatial_grid_descriptor_GetGeometryTypePerGridID

        procedure :: Initialize_Writer                  => spatial_grid_descriptor_Initialize_Writer
        procedure :: Initialize_Reader                  => spatial_grid_descriptor_Initialize_Reader

        procedure :: DefaultBroadcastMetadata           => spatial_grid_descriptor_DefaultBroadcastMetadata

        procedure :: GetNodeOffsetPerGridID             => spatial_grid_descriptor_GetNodeOffsetPerGridID
        procedure :: GetElementOffsetPerGridID          => spatial_grid_descriptor_GetElementOffsetPerGridID

        generic   :: Initialize                         => Initialize_Writer, &
                                                           Initialize_Reader

        procedure :: Allocate                           => spatial_grid_descriptor_Allocate
        procedure :: Free                               => spatial_grid_descriptor_Free
    end type spatial_grid_descriptor_t


    abstract interface
        subroutine spatial_grid_descriptor_BroadcastMetadata(this)
            import spatial_grid_descriptor_t
            class(spatial_grid_descriptor_t), intent(INOUT) :: this
        end subroutine spatial_grid_descriptor_BroadcastMetadata

        subroutine spatial_grid_descriptor_SetGlobalTopologySize(this, GlobalTopologySize)
            import spatial_grid_descriptor_t
            import I8P
            class(spatial_grid_descriptor_t), intent(INOUT) :: this
            integer(I8P),                     intent(IN)    :: GlobalTopologySize
        end subroutine spatial_grid_descriptor_SetGlobalTopologySize

        function spatial_grid_descriptor_GetGlobalTopologySize(this) result(GlobalTopologySize)
            import spatial_grid_descriptor_t
            import I8P
            class(spatial_grid_descriptor_t), intent(IN) :: this
            integer(I8P)                                :: GlobalTopologySize
        end function spatial_grid_descriptor_GetGlobalTopologySize

        subroutine spatial_grid_descriptor_SetTopologySizePerGridID(this, TopologySize, ID)
            import spatial_grid_descriptor_t
            import I8P
            import I4P
            class(spatial_grid_descriptor_t), intent(INOUT) :: this
            integer(I8P),                     intent(IN)    :: TopologySize
            integer(I4P),                     intent(IN)    :: ID
        end subroutine spatial_grid_descriptor_SetTopologySizePerGridID

        function spatial_grid_descriptor_GetTopologySizePerGridID(this, ID) result(TopologySize)
            import spatial_grid_descriptor_t
            import I8P
            import I4P
            class(spatial_grid_descriptor_t), intent(INOUT) :: this
            integer(I4P),                     intent(IN)    :: ID
            integer(I8P)                                    :: TopologySize
        end function spatial_grid_descriptor_GetTopologySizePerGridID

        function spatial_grid_descriptor_GetTopologySizeOffsetPerGridID(this, ID) result(TopologySizeOffset)
            import spatial_grid_descriptor_t
            import I8P
            import I4P
            class(spatial_grid_descriptor_t), intent(INOUT) :: this
            integer(I4P),                     intent(IN)    :: ID
            integer(I8P)                                    :: TopologySizeOffset
        end function spatial_grid_descriptor_GetTopologySizeOffsetPerGridID

        subroutine spatial_grid_descriptor_SetGlobalGeometrySize(this, GlobalGeometrySize)
            import spatial_grid_descriptor_t
            import I8P
            class(spatial_grid_descriptor_t), intent(INOUT) :: this
            integer(I8P),                     intent(IN)    :: GlobalGeometrySize
        end subroutine spatial_grid_descriptor_SetGlobalGeometrySize

        function spatial_grid_descriptor_GetGlobalGeometrySize(this) result(GlobalGeometrySize)
            import spatial_grid_descriptor_t
            import I8P
            class(spatial_grid_descriptor_t), intent(IN) :: this
            integer(I8P)                                :: GlobalGeometrySize
        end function spatial_grid_descriptor_GetGlobalGeometrySize

        subroutine spatial_grid_descriptor_SetGeometrySizePerGridID(this, GeometrySize, ID)
            import spatial_grid_descriptor_t
            import I8P
            import I4P
            class(spatial_grid_descriptor_t), intent(INOUT) :: this
            integer(I8P),                     intent(IN)    :: GeometrySize
            integer(I4P),                     intent(IN)    :: ID
        end subroutine spatial_grid_descriptor_SetGeometrySizePerGridID

        function spatial_grid_descriptor_GetGeometrySizePerGridID(this, ID) result(GeometrySize)
            import spatial_grid_descriptor_t
            import I8P
            import I4P
            class(spatial_grid_descriptor_t), intent(INOUT) :: this
            integer(I4P),                     intent(IN)    :: ID
            integer(I8P)                                    :: GeometrySize
        end function spatial_grid_descriptor_GetGeometrySizePerGridID

        function spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID(this, ID) result(GeometrySizeOffset)
            import spatial_grid_descriptor_t
            import I8P
            import I4P
            class(spatial_grid_descriptor_t), intent(INOUT) :: this
            integer(I4P),                     intent(IN)    :: ID
            integer(I8P)                                    :: GeometrySizeOffset
        end function spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID
    end interface

public :: spatial_grid_descriptor_t

contains

    subroutine spatial_grid_descriptor_NullifyMPIEnvironment(this)
    !-----------------------------------------------------------------
    !< Nullify the MPI environment
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
    !----------------------------------------------------------------- 
        nullify(this%MPIEnvironment)
    end subroutine spatial_grid_descriptor_NullifyMPIEnvironment


    subroutine spatial_grid_descriptor_SetMPIEnvironment(this, MPIEnvironment)
    !-----------------------------------------------------------------
    !< Set the MPI environment
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment !< MPI environment 
    !----------------------------------------------------------------- 
        this%MPIEnvironment => MPIEnvironment
    end subroutine spatial_grid_descriptor_SetMPIEnvironment


    function spatial_grid_descriptor_GetMPIEnvironment(this) result(MPIEnvironment)
    !-----------------------------------------------------------------
    !< Set the MPI environment
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(IN) :: this           !< Spatial grid descriptor type
        type(mpi_env_t), pointer                     :: MPIEnvironment !< MPI environment 
    !----------------------------------------------------------------- 
        MPIEnvironment => this%MPIEnvironment
    end function spatial_grid_descriptor_GetMPIEnvironment


    subroutine spatial_grid_descriptor_DeallocateNumberOfNodesPerGrid(this)
    !-----------------------------------------------------------------
    !< Deallocate NumberOfNodesPerGrid with size NumberOfGrids
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this          !< Spatial grid descriptor type
    !----------------------------------------------------------------- 
        if(allocated(this%NumberOfNodesPerGrid)) deallocate(this%NumberOfNodesPerGrid)
    end subroutine spatial_grid_descriptor_DeallocateNumberOfNodesPerGrid


    subroutine spatial_grid_descriptor_AllocateNumberOfNodesPerGrid(this, NumberOfGrids)
    !-----------------------------------------------------------------
    !< Allocate NumberOfNodesPerGrid with size NumberOfGrids
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this          !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: NumberOfGrids !< Total number of grids of the spatial grid
    !----------------------------------------------------------------- 
        call this%DeallocateNumberOfNodesPerGrid()
        allocate(this%numberOfnodesPerGrid(NumberOfGrids))
    end subroutine spatial_grid_descriptor_AllocateNumberOfNodesPerGrid


    subroutine spatial_grid_descriptor_DeallocateNumberOfElementsPerGrid(this)
    !-----------------------------------------------------------------
    !< Allocate NumberOfElementsPerGrid with size NumberOfGrids
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this          !< Spatial grid descriptor type
    !----------------------------------------------------------------- 
        if(allocated(this%NumberOfElementsPerGrid)) deallocate(this%NumberOfElementsPerGrid)
    end subroutine spatial_grid_descriptor_DeallocateNumberOfElementsPerGrid


    subroutine spatial_grid_descriptor_AllocateNumberOfElementsPerGrid(this, NumberOfGrids)
    !-----------------------------------------------------------------
    !< Allocate NumberOfElementsPerGrid with size NumberOfGrids
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this          !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: NumberOfGrids !< Total number of grids of the spatial grid
    !----------------------------------------------------------------- 
        call this%DeallocateNumberOfElementsPerGrid()
        allocate(this%NumberOfElementsPerGrid(NumberOfGrids))
    end subroutine spatial_grid_descriptor_AllocateNumberOfElementsPerGrid


    subroutine spatial_grid_descriptor_Allocate(this, NumberOfGrids)
    !-----------------------------------------------------------------
    !< Set the total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this          !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: NumberOfGrids !< Total number of grids of the spatial grid
    !----------------------------------------------------------------- 
        this%NumberOfGrids = NumberOfGrids
        allocate(this%NumberOfNodesPerGrid(NumberOfGrids))
        allocate(this%NumberOfElementsPerGrid(NumberOfGrids))
        allocate(this%TopologyTypePerGrid(NumberOfGrids))
        allocate(this%GeometryTypePerGrid(NumberOfGrids))
    end subroutine spatial_grid_descriptor_Allocate


    subroutine spatial_grid_descriptor_SetNumberOfGrids(this, NumberOfGrids)
    !-----------------------------------------------------------------
    !< Set the total number of grids
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this          !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: NumberOfGrids !< Total number of grids
    !----------------------------------------------------------------- 
        this%NumberofGrids = NumberOfGrids
    end subroutine spatial_grid_descriptor_SetNumberofGrids


    function spatial_grid_descriptor_GetNumberOfGrids(this)
    !-----------------------------------------------------------------
    !< Return the total number of grids
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this       !< Spatial grid descriptor type
        integer(I4P) :: spatial_grid_descriptor_GetNumberOfGrids      !< Total number of grids
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetNumberOfGrids = this%NumberOfGrids
    end function spatial_grid_descriptor_GetNumberOfGrids


    subroutine spatial_grid_descriptor_SetGlobalNumberOfNodes(this, GlobalNumberOfNodes)
    !-----------------------------------------------------------------
    !< Set the total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this                !< Spatial grid descriptor type
        integer(I8P),                     intent(IN)    :: GlobalNumberOfNodes !< Total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        this%GlobalNumberOfNodes = GlobalNumberOfNodes
    end subroutine spatial_grid_descriptor_SetGlobalNumberOfNodes


    function spatial_grid_descriptor_GetGlobalNumberOfNodes(this)
    !-----------------------------------------------------------------
    !< Return the total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(IN) :: this           !< Spatial grid descriptor type
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


    subroutine spatial_grid_descriptor_SetNumberOfNodesPerGridID(this, NumberOfNodes, ID)
    !-----------------------------------------------------------------
    !< Set the number of nodes of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this            !< Spatial grid descriptor type
        integer(I8P),                     intent(IN)    :: NumberOfNodes   !< Number of nodes of the grid ID
        integer(I4P),                     intent(IN)    :: ID              !< Grid identifier
    !-----------------------------------------------------------------
        this%NumberOfNodesPerGrid(ID+1) = NumberOfNodes
    end subroutine spatial_grid_descriptor_SetNumberOfNodesPerGridID


    function spatial_grid_descriptor_GetNumberOfNodesPerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of nodes of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID             !< Grid identifier
        integer(I8P) :: spatial_grid_descriptor_GetNumberOfNodesPerGridID !< Number of nodes of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetNumberOfNodesPerGridID = this%NumberOfNodesPerGrid(ID+1)
    end function spatial_grid_descriptor_GetNumberOfNodesPerGridID


    subroutine spatial_grid_descriptor_SetNumberOfElementsPerGridID(this, NumberOfElements, ID)
    !-----------------------------------------------------------------
    !< Set the number of nodes of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this             !< Spatial grid descriptor type
        integer(I8P),                     intent(IN)    :: NumberOfElements !< Number of elements of the grid ID
        integer(I4P),                     intent(IN)    :: ID               !< Grid identifier
    !-----------------------------------------------------------------
        this%NumberOfElementsPerGrid(ID+1) = NumberOfElements
    end subroutine spatial_grid_descriptor_SetNumberOfElementsPerGridID


    function spatial_grid_descriptor_GetNumberOfElementsPerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of elements of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this              !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID                !< Grid identifier
        integer(I8P) :: spatial_grid_descriptor_GetNumberOfElementsPerGridID !< Number of elements of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetNumberOfElementsPerGridID = this%NumberOfElementsPerGrid(ID+1)
    end function spatial_grid_descriptor_GetNumberOfElementsPerGridID


    subroutine spatial_grid_descriptor_DeallocateTopologyTypePerGrid(this)
    !-----------------------------------------------------------------
    !< Deallocate the topology type array
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this         !< Spatial grid descriptor type
    !-----------------------------------------------------------------
        if(allocated(this%TopologyTypePerGrid)) deallocate(this%TopologyTypePerGrid)
    end subroutine spatial_grid_descriptor_DeallocateTopologyTypePerGrid


    subroutine spatial_grid_descriptor_AllocateTopologyTypePerGrid(this, NumberOfGrids)
    !-----------------------------------------------------------------
    !< Allocate TopologyTypePerGrid to NumberOfGrids
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this         !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: NumberOfGrids!< Number of grids
    !-----------------------------------------------------------------
        call this%DeallocateTopologyTypePerGrid()
        allocate(this%TopologyTypePerGrid(NumberOfGrids))
    end subroutine spatial_grid_descriptor_AllocateTopologyTypePerGrid


    subroutine spatial_grid_descriptor_SetTopologyTypePerGridID(this, TopologyType, ID)
    !-----------------------------------------------------------------
    !< Set the topology type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this         !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: TopologyType !< Topology type of the grid ID
        integer(I4P),                     intent(IN)    :: ID           !< Grid identifier
    !-----------------------------------------------------------------
        this%TopologyTypePerGrid(ID+1) = TopologyType
    end subroutine spatial_grid_descriptor_SetTopologyTypePerGridID


    function spatial_grid_descriptor_GetTopologyTypePerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the topology type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this          !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID            !< Grid identifier
        integer(I4P) :: spatial_grid_descriptor_GetTopologyTypePerGridID !< Topology type of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetTopologyTypePerGridID = this%TopologyTypePerGrid(ID+1)
    end function spatial_grid_descriptor_GetTopologyTypePerGridID


    subroutine spatial_grid_descriptor_DeallocateGeometryTypePerGrid(this)
    !-----------------------------------------------------------------
    !< Deallocate TopologyTypePerGrid
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this         !< Spatial grid descriptor type
    !-----------------------------------------------------------------
        if(allocated(this%GeometryTypePerGrid)) deallocate(this%GeometryTypePerGrid)
    end subroutine spatial_grid_descriptor_DeallocateGeometryTypePerGrid


    subroutine spatial_grid_descriptor_AllocateGeometryTypePerGrid(this, NumberOfGrids)
    !-----------------------------------------------------------------
    !< Set the topology type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this         !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: NumberOfGrids!< Number of grids
    !-----------------------------------------------------------------
        call this%DeallocateGeometryTypePerGrid()
        allocate(this%GeometryTypePerGrid(NumberOfGrids))
    end subroutine spatial_grid_descriptor_AllocateGeometryTypePerGrid


    subroutine spatial_grid_descriptor_SetGeometryTypePerGridID(this, GeometryType, ID)
    !-----------------------------------------------------------------
    !< Set the topology type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this         !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: GeometryType !< Geometry type of the grid ID
        integer(I4P),                     intent(IN)    :: ID           !< Grid identifier
    !-----------------------------------------------------------------
        this%GeometryTypePerGrid(ID+1) = GeometryType
    end subroutine spatial_grid_descriptor_SetGeometryTypePerGridID


    function spatial_grid_descriptor_GetGeometryTypePerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the geometry type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(IN) :: this             !< Spatial grid descriptor type
        integer(I4P),                     intent(IN) :: ID               !< Grid identifier
        integer(I4P) :: spatial_grid_descriptor_GetGeometryTypePerGridID !< Geometry type of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetGeometryTypePerGridID = this%GeometryTypePerGrid(ID+1)
    end function spatial_grid_descriptor_GetGeometryTypePerGridID


    function spatial_grid_descriptor_GetNodeOffsetPerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the node offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this        !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID          !< Grid identifier
        integer(I8P) :: spatial_grid_descriptor_GetNodeOffsetPerGridID !< Node offset of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetNodeOffsetPerGridID = sum(this%NumberOfNodesPerGrid(:ID))
    end function spatial_grid_descriptor_GetNodeOffsetPerGridID


    function spatial_grid_descriptor_GetElementOffsetPerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the element offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID             !< Grid identifier
        integer(I8P) :: spatial_grid_descriptor_GetElementOffsetPerGridID !< Element offset of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetElementOffsetPerGridID = sum(this%NumberOfElementsPerGrid(:ID))
    end function spatial_grid_descriptor_GetElementOffsetPerGridID


    subroutine spatial_grid_descriptor_Initialize_Writer(this, MPIEnvironment, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this             !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment   !< MPI environment type
        integer(I8P),                     intent(IN)    :: NumberOfNodes    !< Number of nodes of the current grid
        integer(I8P),                     intent(IN)    :: NumberOfElements !< Number of elements of the current grid
        integer(I4P),                     intent(IN)    :: TopologyType     !< Topology type of the current grid
        integer(I4P),                     intent(IN)    :: GeometryType     !< Geometry type of the current grid
        integer(I4P)                                    :: i                !< Loop index in NumberOfGrids
    !-----------------------------------------------------------------
        call this%Free()
        this%MPIEnvironment => MPIEnvironment
        call this%MPIEnvironment%mpi_allgather(NumberOfNodes, this%NumberOfNodesPerGrid)
        call this%MPIEnvironment%mpi_allgather(NumberOfElements, this%NumberOfElementsPerGrid)
        call this%MPIEnvironment%mpi_allgather(TopologyType, this%TopologyTypePerGrid)
        call this%MPIEnvironment%mpi_allgather(GeometryType, this%GeometryTypePerGrid)
        call this%SetGlobalNumberOfElements(sum(this%NumberOfElementsPerGrid))
        call this%SetGlobalNumberOfNodes(sum(this%NumberOfNodesPerGrid))
        this%NumberOfGrids = size(this%NumberOfNodesPerGrid, dim=1)
    end subroutine spatial_grid_descriptor_Initialize_Writer


    subroutine spatial_grid_descriptor_Initialize_Reader(this, MPIEnvironment)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment !< MPI environment type
    !-----------------------------------------------------------------
        call this%Free()
        this%MPIEnvironment => MPIEnvironment
    end subroutine spatial_grid_descriptor_Initialize_Reader



    subroutine spatial_grid_descriptor_DefaultBroadcastMetadata(this)
    !-----------------------------------------------------------------
    !< Broadcast metadata after XDMF parsing
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this       !< Spatial grid descriptor type
    !-----------------------------------------------------------------
        call this%MPIEnvironment%mpi_broadcast(this%NumberOfNodesPerGrid)
        call this%MPIEnvironment%mpi_broadcast(this%NumberOfElementsPerGrid)
        call this%SetGlobalNumberOfElements(sum(this%NumberOfElementsPerGrid))
        call this%SetGlobalNumberOfNodes(sum(this%NumberOfNodesPerGrid))
        call this%MPIEnvironment%mpi_broadcast(this%TopologyTypePerGrid)
        call this%MPIEnvironment%mpi_broadcast(this%GeometryTypePerGrid)
		this%NumberOfGrids = size(this%NumberOfNodesPerGrid, dim=1)
    end subroutine spatial_grid_descriptor_DefaultBroadcastMetadata


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
        if(allocated(this%TopologyTypePerGrid)) deallocate(this%TopologyTypePerGrid)
        if(allocated(this%GeometryTypePerGrid)) deallocate(this%GeometryTypePerGrid)
        nullify(this%MPIEnvironment)

    end subroutine spatial_grid_descriptor_Free


end module spatial_grid_descriptor
