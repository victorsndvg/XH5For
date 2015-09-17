module spatial_grid_descriptor
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use IR_Precision, only : I4P, I8P, R4P, R8P
use mpi_environment

implicit none

private

    type :: spatial_grid_descriptor_t
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
    private
        integer(I8P)                             :: GlobalNumberOfNodes         !< Total number of nodes of the spatial grid
        integer(I8P)                             :: GlobalNumberOfElements      !< Total number of elements of the spatial grid
        integer(I8P), allocatable                :: NumberOfNodesPerGrid(:)     !< Array of number of nodes per grid
        integer(I8P), allocatable                :: NumberOfElementsPerGrid(:)  !< Array of number of elements per grid
        integer(I4P), allocatable                :: TopologyTypePerGrid(:)      !< Array of topology type per grid
        integer(I4P), allocatable                :: GeometryTypePerGrid(:)      !< Array of geometry type per grid
        type(mpi_env_t), pointer                 :: MPIEnvironment              !< MPI environment 
    contains
    private
        procedure         :: SetGlobalNumberOfNodes         => spatial_grid_descriptor_SetGlobalNumberOfNodes
        procedure         :: SetGlobalNumberOfElements      => spatial_grid_descriptor_SetGlobalNumberOfElements
        procedure, public :: GetGlobalNumberOfNodes         => spatial_grid_descriptor_GetGlobalNumberOfNodes
        procedure, public :: GetGlobalNumberOfElements      => spatial_grid_descriptor_GetGlobalNumberOfElements
        procedure, public :: GetNumberOfNodesFromGridID     => spatial_grid_descriptor_GetNumberOfNodesFromGridID
        procedure, public :: GetNumberOfElementsFromGridID  => spatial_grid_descriptor_GetNumberOfElementsFromGridID
        procedure, public :: GetTopologyTypeFromGridID      => spatial_grid_descriptor_GetTopologyTypeFromGridID
        procedure, public :: GetGeometryTypeFromGridID      => spatial_grid_descriptor_GetGeometryTypeFromGridID
        procedure, public :: GetNodeOffsetFromGridID        => spatial_grid_descriptor_GetNodeOffsetFromGridID
        procedure, public :: GetElementOffsetFromGridID     => spatial_grid_descriptor_GetElementOffsetFromGridID
        procedure, public :: initialize                     => spatial_grid_descriptor_initialize
    end type spatial_grid_descriptor_t

public :: spatial_grid_descriptor_t

contains

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


    function spatial_grid_descriptor_GetTopologyTypeFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the topology type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID             !< Grid identifier
        integer(I4P) :: spatial_grid_descriptor_GetTopologyTypeFromGridID !< Topology type of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetTopologyTypeFromGridID = this%TopologyTypePerGrid(ID+1)
    end function spatial_grid_descriptor_GetTopologyTypeFromGridID


    function spatial_grid_descriptor_GetGeometryTypeFromGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the geometry type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this           !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: ID             !< Grid identifier
        integer(I4P) :: spatial_grid_descriptor_GetGeometrytypeFromGridID !< Geometry type of a grid
    !-----------------------------------------------------------------
        spatial_grid_descriptor_GetGeometryTypeFromGridID = this%GeometryTypePerGrid(ID+1)
    end function spatial_grid_descriptor_GetGeometryTypeFromGridID


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


    subroutine spatial_grid_descriptor_initialize(this, MPIEnvironment, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this             !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment   !< MPI environment type
        integer(I8P),                     intent(IN)    :: NumberOfNodes    !< Number of nodes of the current grid
        integer(I8P),                     intent(IN)    :: NumberOfElements !< Number of elements of the current grid
        integer(I4P),                     intent(IN)    :: TopologyType     !< Topology type of the current grid
        integer(I4P),                     intent(IN)    :: GeometryType     !< Geometry type of the current grid
    !-----------------------------------------------------------------
        this%MPIEnvironment => MPIEnvironment
        call this%MPIEnvironment%mpi_allgather_single_int_value(NumberOfNodes, this%NumberOfNodesPerGrid)
        call this%MPIEnvironment%mpi_allgather_single_int_value(NumberOfElements, this%NumberOfElementsPerGrid)
        call this%MPIEnvironment%mpi_allgather_single_int_value(TopologyType, this%TopologyTypePerGrid)
        call this%MPIEnvironment%mpi_allgather_single_int_value(GeometryType, this%GeometryTypePerGrid)
        call this%SetGlobalNumberOfElements(sum(this%NumberOfElementsPerGrid))
        call this%SetGlobalNumberOfNodes(sum(this%NumberOfNodesPerGrid))
    end subroutine spatial_grid_descriptor_initialize

end module spatial_grid_descriptor
