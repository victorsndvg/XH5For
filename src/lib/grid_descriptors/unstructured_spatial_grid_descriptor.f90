module unstructured_spatial_grid_descriptor

use IR_Precision, only : I4P, I8P
use spatial_grid_descriptor
use mpi_environment
use xh5for_utils

implicit none

private

    type, extends(spatial_grid_descriptor_t) :: unstructured_spatial_grid_descriptor_t
    private
        integer(I8P)                                :: GlobalGeometrySize     = 0  !< Total size of the coordinates of the spatial grid
        integer(I8P)                                :: GlobalTopologySize     = 0  !< Total size of the connectivities of the spatial grid
        integer(I8P),                   allocatable :: TopologySizePerGrid(:)      !< Array of sizes of array connectivities per grid
    contains
        procedure, public :: Allocate                           => unst_spatial_grid_descriptor_Allocate
        procedure, public :: SetGlobalTopologySize              => unst_spatial_grid_descriptor_SetGlobalTopologySize
        procedure, public :: GetGlobalTopologySize              => unst_spatial_grid_descriptor_GetGlobalTopologySize
        procedure, public :: SetTopologySizePerGridID           => unst_spatial_grid_descriptor_SetTopologySizePerGridID
        procedure, public :: GetTopologySizePerGridID           => unst_spatial_grid_descriptor_GetTopologySizePerGridID
        procedure, public :: GetTopologySizeOffsetPerGridID     => unst_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID

        procedure, public :: SetGlobalGeometrySize              => unst_spatial_grid_descriptor_SetGlobalGeometrySize
        procedure, public :: GetGlobalGeometrySize              => unst_spatial_grid_descriptor_GetGlobalGeometrySize
        procedure, public :: SetGeometrySizePerGridID           => unst_spatial_grid_descriptor_SetGeometrySizePerGridID
        procedure, public :: GetGeometrySizePerGridID           => unst_spatial_grid_descriptor_GetGeometrySizePerGridID
        procedure, public :: GetGeometrySizeOffsetPerGridID     => unst_spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID

        procedure, public :: BroadcastMetadata                  => unst_spatial_grid_descriptor_BroadcastMetadata
        procedure, public :: Free                               => unst_spatial_grid_descriptor_Free
    end type unstructured_spatial_grid_descriptor_t

public:: unstructured_spatial_grid_descriptor_t


contains

    subroutine unst_spatial_grid_descriptor_Allocate(this, NumberOfGrids)
    !-----------------------------------------------------------------
    !< Set the total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this          !< Spatial grid descriptor type
        integer(I4P),                                  intent(IN)    :: NumberOfGrids !< Total number of grids of the spatial grid
    !----------------------------------------------------------------- 
        call this%SetNumberOfGrids(NumberOfGrids = NumberOfGrids)
        call this%AllocateNumberOfNodesPerGrid(NumberOfGrids = NumberOfGrids)
        call this%AllocateNumberOfElementsPerGrid(NumberOfGrids = NumberOfGrids)
        call this%AllocateTopologyTypePerGrid(NumberOfGrids = NumberOfGrids)    
        call this%AllocateGeometryTypePerGrid(NumberOfGrids = NumberOfGrids)
        if(allocated(this%TopologySizePerGrid)) deallocate(this%TopologySizePerGrid)
        allocate(this%TopologySizePerGrid(NumberOfGrids))
    end subroutine unst_spatial_grid_descriptor_Allocate


    subroutine unst_spatial_grid_descriptor_SetGlobalTopologySize(this, GlobalTopologySize, Dimension)
    !-----------------------------------------------------------------
    !< Set the total topology size of the spatial grid
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this                !< Unstructured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: GlobalTopologySize  !< Total size of connectivities of the spatial grid
        integer(I4P), optional,                        intent(IN)    :: Dimension           !< Dimension of the topology (1=X,2=Y,3=Z)
    !----------------------------------------------------------------- 
        this%GlobalTopologySize = GlobalTopologySize
    end subroutine unst_spatial_grid_descriptor_SetGlobalTopologySize


    function unst_spatial_grid_descriptor_GetGlobalTopologySize(this, Dimension) result(GlobalTopologySize)
    !-----------------------------------------------------------------
    !< Get the total topology size of the spatial grid
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(IN) :: this                !< Unstructured Spatial grid descriptor type
        integer(I4P), optional,                        intent(IN) :: Dimension           !< Dimension of the topology (1=X,2=Y,3=Z)
        integer(I8P)                                              :: GlobalTopologySize  !< Total size of connectivities of the spatial grid
    !----------------------------------------------------------------- 
        GlobalTopologySize = this%GlobalTopologySize
    end function unst_spatial_grid_descriptor_GetGlobalTopologySize


    subroutine unst_spatial_grid_descriptor_SetTopologySizePerGridID(this, TopologySize, ID, Dimension)
    !-----------------------------------------------------------------
    !< Set the topology size of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< Unstructured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: TopologySize   !< Topology size of the grid ID
        integer(I4P),                                  intent(IN)    :: ID             !< Grid identifier
        integer(I4P), optional,                        intent(IN)    :: Dimension           !< Dimension of the topology (1=X,2=Y,3=Z)
        type(mpi_env_t), pointer                                     :: MPIEnvironment !< MPI Environment pointer
    !-----------------------------------------------------------------
        if(.not. allocated(this%TopologySizePerGrid)) then
            allocate(this%TopologySizePerGrid(this%GetNumberOfGrids()))
            MPIEnvironment => this%GetMPIEnvironment()
            call MPIEnvironment%mpi_allgather(TopologySize, this%TopologySizePerGrid)
            call this%SetGlobalTopologySize(sum(this%TopologySizePerGrid))
        endif
        this%TopologySizePerGrid(ID+1) = TopologySize
    end subroutine unst_spatial_grid_descriptor_SetTopologySizePerGridID


    function unst_spatial_grid_descriptor_GetTopologySizePerGridID(this, ID, Dimension) result(TopologySize)
    !-----------------------------------------------------------------
    !< Return the topology size of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this         !< Unstructured Spatial grid descriptor type
        integer(I4P),                                  intent(IN)    :: ID           !< Grid identifier
        integer(I4P), optional,                        intent(IN)    :: Dimension    !< Dimension of the topology (1=X,2=Y,3=Z)
        integer(I8P)                                                 :: TopologySize !< Topology Size of a grid
    !-----------------------------------------------------------------
        TopologySize = this%TopologySizePerGrid(ID+1)
    end function unst_spatial_grid_descriptor_GetTopologySizePerGridID


    function unst_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID(this, ID, Dimension) result(Offset)
    !-----------------------------------------------------------------
    !< Return the topology size offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this   !< Unstructured Spatial grid descriptor type
        integer(I4P),                                  intent(IN)    :: ID     !< Grid identifier
        integer(I4P), optional,                        intent(IN)    :: Dimension           !< Dimension of the topology (1=X,2=Y,3=Z)
        integer(I8P)                                                 :: Offset !< Topology size offset of a grid
    !-----------------------------------------------------------------
        Offset = sum(this%TopologySizePerGrid(:ID))
    end function unst_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID


    subroutine unst_spatial_grid_descriptor_SetGlobalGeometrySize(this, GlobalGeometrySize, Dimension)
    !-----------------------------------------------------------------
    !< Set the total Geometry size of the spatial grid
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this                !< Unstructured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: GlobalGeometrySize  !< Total size of coordinates of the spatial grid
        integer(I4P), optional,                        intent(IN)    :: Dimension           !< Dimension of the geometry (1=X,2=Y,3=Z)
    !----------------------------------------------------------------- 
        this%GlobalGeometrySize = GlobalGeometrySize
    end subroutine unst_spatial_grid_descriptor_SetGlobalGeometrySize


    function unst_spatial_grid_descriptor_GetGlobalGeometrySize(this, Dimension) result(GlobalGeometrySize)
    !-----------------------------------------------------------------
    !< Get the total geometry size of the spatial grid
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(IN) :: this                !< Unstructured Spatial grid descriptor type
        integer(I4P), optional,                        intent(IN) :: Dimension           !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I8P)                                              :: GlobalGeometrySize  !< Total size of coordinates of the spatial grid
        integer(I8P)                                              :: SpaceDimension      !< Space dimension
        type(mpi_env_t), pointer                                  :: MPIEnvironment      !< MPI Environmnet pointer
    !----------------------------------------------------------------- 
        MPIEnvironment => this%GetMPIEnvironment()
        SpaceDimension = GetSpaceDimension(this%GetGeometryTypePerGridID(ID=MPIEnvironment%get_rank()))
        GlobalGeometrySize = this%GetGlobalNumberOfNodes()*SpaceDimension
    end function unst_spatial_grid_descriptor_GetGlobalGeometrySize


    subroutine unst_spatial_grid_descriptor_SetGeometrySizePerGridID(this, GeometrySize, ID, Dimension)
    !-----------------------------------------------------------------
    !< Set the geometry size of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this             !< Unstructured Spatial grid descriptor type
        integer(I4P), optional,                        intent(IN)    :: Dimension           !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I8P),                                  intent(IN)    :: GeometrySize     !< Geometry size of the grid ID
        integer(I4P),                                  intent(IN)    :: ID               !< Grid identifier
        integer(I8P)                                                 :: SpaceDimension   !< Space dimension
    !-----------------------------------------------------------------
        SpaceDimension = GetSpaceDimension(this%GetGeometryTypePerGridID(ID=ID))
        call this%SetNumberOfNodesPerGridID(NumberOfNodes = GeometrySize/Spacedimension, ID = ID) 
    end subroutine unst_spatial_grid_descriptor_SetGeometrySizePerGridID


    function unst_spatial_grid_descriptor_GetGeometrySizePerGridID(this, ID, Dimension)
    !-----------------------------------------------------------------
    !< Return the Geometry size of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< Unstructured Spatial grid descriptor type
        integer(I4P),                                  intent(IN)    :: ID             !< Grid identifier
        integer(I4P), optional,                        intent(IN)    :: Dimension           !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I8P)                                                 :: SpaceDimension !< Space dimension
        integer(I8P) :: unst_spatial_grid_descriptor_GetGeometrySizePerGridID          !< Geometry Size of a grid
    !-----------------------------------------------------------------
        SpaceDimension = GetSpaceDimension(this%GetGeometryTypePerGridID(ID=ID))
        unst_spatial_grid_descriptor_GetGeometrySizePerGridID = this%GetNumberOfNodesPerGridID(ID)*SpaceDimension
    end function unst_spatial_grid_descriptor_GetGeometrySizePerGridID


    function unst_spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID(this, ID, Dimension)
    !-----------------------------------------------------------------
    !< Return the geometry size offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this   !< Unstructured Spatial grid descriptor type
        integer(I4P),                                  intent(IN)    :: ID     !< Grid identifier
        integer(I4P), optional,                        intent(IN)    :: Dimension           !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I8P)                                                 :: SpaceDimension !< Space dimension
        integer(I8P) :: unst_spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID !< Geometry size offset of a grid
    !-----------------------------------------------------------------
        SpaceDimension = GetSpaceDimension(this%GetGeometryTypePerGridID(ID=ID))
        unst_spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID = this%GetNodeOffsetPerGridID(ID=ID)*SpaceDimension
    end function unst_spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID


    subroutine unst_spatial_grid_descriptor_BroadcastMetadata(this)
    !-----------------------------------------------------------------
    !< Broadcast metadata after XDMF parsing
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< Unstructured Spatial grid descriptor type
        type(mpi_env_t), pointer                                     :: MPIEnvironment !< MPI Environmnet pointer
    !-----------------------------------------------------------------
        MPIEnvironment => this%GetMPIEnvironment()
        call MPIEnvironment%mpi_broadcast(this%TopologySizePerGrid)
        call this%SetGlobalTopologySize(sum(this%TopologySizePerGrid))
        call this%DefaultBroadcastMetadata()
    end subroutine unst_spatial_grid_descriptor_BroadcastMetadata


    subroutine unst_spatial_grid_descriptor_Free(this)
    !-----------------------------------------------------------------
    !< Free the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this !< Spatial grid descriptor type
        integer(I4P)                                                 :: i    !< Loop index in NumberOfGrids
        integer(I4P)                                                 :: j    !< Loop index in NumberOfAttributes
    !----------------------------------------------------------------- 
        if(allocated(this%TopologySizePerGrid)) deallocate(this%TopologySizePerGrid)
        call This%SetGlobalNumberOfNodes(GlobalNumberOfNodes = 0_I8P)
        call This%SetGlobalNumberOfElements(GlobalNumberOfElements = 0_I8P)
        call this%DeallocateNumberOfNodesPerGrid()
        call this%DeallocateNumberOfElementsPerGrid()
        call this%DeallocateTopologyTypePerGrid()
        call this%DeallocateGeometryTypePerGrid()
        call this%DeallocateGeometryTypePerGrid()
        call this%NullifyMPIEnvironment()
    end subroutine unst_spatial_grid_descriptor_Free



end module unstructured_spatial_grid_descriptor
