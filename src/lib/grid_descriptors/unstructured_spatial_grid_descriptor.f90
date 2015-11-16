module unstructured_spatial_grid_descriptor

use IR_Precision, only : I4P, I8P
use spatial_grid_descriptor

implicit none

private

    type, extends(spatial_grid_descriptor_t) :: unstructured_spatial_grid_descriptor_t
    private
        integer(I8P)                                :: GlobalTopologySize = 0      !< Total size of the connectivities of the spatial grid
        integer(I8P),                   allocatable :: TopologySizePerGrid(:)      !< Array of sizes of array connectivities per grid
    contains
        procedure, public :: Allocate                           => unst_spatial_grid_descriptor_Allocate
        procedure, public :: SetGlobalTopologySize              => unst_spatial_grid_descriptor_SetGlobalTopologySize
        procedure, public :: GetGlobalTopologySize              => unst_spatial_grid_descriptor_GetGlobalTopologySize
        procedure, public :: SetTopologySizePerGridID           => unst_spatial_grid_descriptor_SetTopologySizePerGridID
        procedure, public :: GetTopologySizePerGridID           => unst_spatial_grid_descriptor_GetTopologySizePerGridID
        procedure, public :: GetTopologySizeOffsetPerGridID     => unst_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID
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
        integer(I4P),                     intent(IN)    :: NumberOfGrids !< Total number of grids of the spatial grid
    !----------------------------------------------------------------- 
        this%NumberOfGrids = NumberOfGrids
        allocate(this%NumberOfNodesPerGrid(NumberOfGrids))
        allocate(this%NumberOfElementsPerGrid(NumberOfGrids))
        allocate(this%TopologySizePerGrid(NumberOfGrids))
        allocate(this%TopologyTypePerGrid(NumberOfGrids))
        allocate(this%GeometryTypePerGrid(NumberOfGrids))
    end subroutine unst_spatial_grid_descriptor_Allocate


    subroutine unst_spatial_grid_descriptor_SetGlobalTopologySize(this, GlobalTopologySize)
    !-----------------------------------------------------------------
    !< Set the total topology size of the spatial grid
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this                !< Unstructured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: GlobalTopologySize  !< Total size of connectivities of the spatial grid
    !----------------------------------------------------------------- 
        this%GlobalTopologySize = GlobalTopologySize
    end subroutine unst_spatial_grid_descriptor_SetGlobalTopologySize


    function unst_spatial_grid_descriptor_GetGlobalTopologySize(this) result(GlobalTopologySize)
    !-----------------------------------------------------------------
    !< Get the total topology size of the spatial grid
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(IN) :: this                !< Unstructured Spatial grid descriptor type
        integer(I8P)                                              :: GlobalTopologySize  !< Total size of connectivities of the spatial grid
    !----------------------------------------------------------------- 
        GlobalTopologySize = this%GlobalTopologySize
    end function unst_spatial_grid_descriptor_GetGlobalTopologySize


    subroutine unst_spatial_grid_descriptor_SetTopologySizePerGridID(this, TopologySize, ID)
    !-----------------------------------------------------------------
    !< Set the topology size of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this             !< Unstructured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: TopologySize !< Topology size of the grid ID
        integer(I4P),                                  intent(IN)    :: ID               !< Grid identifier
    !-----------------------------------------------------------------
        if(.not. allocated(this%TopologySizePerGrid)) then
            allocate(this%TopologySizePerGrid(this%GetNumberOfGrids()))
            call this%MPIEnvironment%mpi_allgather(TopologySize, this%TopologySizePerGrid)
            call this%SetGlobalTopologySize(sum(this%TopologySizePerGrid))
        endif
        this%TopologySizePerGrid(ID+1) = TopologySize
    end subroutine unst_spatial_grid_descriptor_SetTopologySizePerGridID


    function unst_spatial_grid_descriptor_GetTopologySizePerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the topology size of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this  !< Unstructured Spatial grid descriptor type
        integer(I4P),                                  intent(IN)    :: ID    !< Grid identifier
        integer(I8P) :: unst_spatial_grid_descriptor_GetTopologySizePerGridID !< Topology Size of a grid
    !-----------------------------------------------------------------
        unst_spatial_grid_descriptor_GetTopologySizePerGridID = this%TopologySizePerGrid(ID+1)
    end function unst_spatial_grid_descriptor_GetTopologySizePerGridID


    function unst_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the topology size offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this   !< Unstructured Spatial grid descriptor type
        integer(I4P),                                  intent(IN)    :: ID     !< Grid identifier
        integer(I8P) :: unst_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID !< Topology size offset of a grid
    !-----------------------------------------------------------------
        unst_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID = sum(this%TopologySizePerGrid(:ID))
    end function unst_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID


    subroutine unst_spatial_grid_descriptor_BroadcastMetadata(this)
    !-----------------------------------------------------------------
    !< Broadcast metadata after XDMF parsing
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this !< Unstructured Spatial grid descriptor type
    !-----------------------------------------------------------------
        call this%MPIEnvironment%mpi_broadcast(this%NumberOfNodesPerGrid)
        call this%MPIEnvironment%mpi_broadcast(this%NumberOfElementsPerGrid)
        call this%MPIEnvironment%mpi_broadcast(this%TopologySizePerGrid)
        call this%SetGlobalNumberOfElements(sum(this%NumberOfElementsPerGrid))
        call this%SetGlobalNumberOfNodes(sum(this%NumberOfNodesPerGrid))
        call this%SetGlobalTopologySize(sum(this%TopologySizePerGrid))
        call this%MPIEnvironment%mpi_broadcast(this%TopologyTypePerGrid)
        call this%MPIEnvironment%mpi_broadcast(this%GeometryTypePerGrid)
		this%NumberOfGrids = size(this%NumberOfNodesPerGrid, dim=1)
    end subroutine unst_spatial_grid_descriptor_BroadcastMetadata


    subroutine unst_spatial_grid_descriptor_Free(this)
    !-----------------------------------------------------------------
    !< Free the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this !< Spatial grid descriptor type
        integer(I4P)                                                 :: i    !< Loop index in NumberOfGrids
        integer(I4P)                                                 :: j    !< Loop index in NumberOfAttributes
    !----------------------------------------------------------------- 

        This%GlobalNumberOfNodes = 0
        This%GlobalNumberOfElements = 0
        if(allocated(this%NumberOfNodesPerGrid))    deallocate(this%NumberOfNodesPerGrid)
        if(allocated(this%NumberOfElementsPerGrid)) deallocate(this%NumberOfElementsPerGrid)
        if(allocated(this%TopologySizePerGrid)) deallocate(this%TopologySizePerGrid)
        if(allocated(this%TopologyTypePerGrid)) deallocate(this%TopologyTypePerGrid)
        if(allocated(this%GeometryTypePerGrid)) deallocate(this%GeometryTypePerGrid)
        nullify(this%MPIEnvironment)
    end subroutine unst_spatial_grid_descriptor_Free



end module unstructured_spatial_grid_descriptor
