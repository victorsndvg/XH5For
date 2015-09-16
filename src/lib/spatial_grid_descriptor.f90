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
        integer(I8P)                             :: GlobalNumberOfNodes
        integer(I8P)                             :: GlobalNumberOfElements
        integer(I8P), allocatable                :: NumberOfNodesPerGrid(:)
        integer(I8P), allocatable                :: NumberOfElementsPerGrid(:)
        integer(I4P), allocatable                :: TopologyTypePerGrid(:)
        integer(I4P), allocatable                :: GeometryTypePerGrid(:)
        type(mpi_env_t), pointer                 :: MPIEnvironment                 !< MPI environment 
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
        procedure, public :: initialize                     => spatial_grid_descriptor_initialize
    end type spatial_grid_descriptor_t

public :: spatial_grid_descriptor_t

contains

    subroutine spatial_grid_descriptor_SetGlobalNumberOfNodes(this, GlobalNumberOfNodes)
        class(spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I8P)                              , intent(IN)    :: GlobalNumberOfNodes

        this%GlobalNumberOfNodes = GlobalNumberOfNodes
    end subroutine spatial_grid_descriptor_SetGlobalNumberOfNodes

    function spatial_grid_descriptor_GetGlobalNumberOfNodes(this)
        class(spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I8P)                         :: spatial_grid_descriptor_GetGlobalNumberOfNodes

        spatial_grid_descriptor_GetGlobalNumberOfNodes = this%GlobalNumberOfNodes
    end function spatial_grid_descriptor_GetGlobalNumberOfNodes

    subroutine spatial_grid_descriptor_SetGlobalNumberOfElements(this, GlobalNumberOfElements)
        class(spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I8P)                              , intent(IN)    :: GlobalNumberOfElements

        this%GlobalNumberOfElements = GlobalNumberOfelements
    end subroutine spatial_grid_descriptor_SetGlobalNumberOfElements

    Function spatial_grid_descriptor_GetGlobalNumberOfElements(this)
        class(spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I8P)                         :: spatial_grid_descriptor_GetGlobalNumberOfElements

        spatial_grid_descriptor_GetGlobalNumberOfelements = this%GlobalNumberOfElements
    end function spatial_grid_descriptor_GetGlobalNumberOfElements

    function spatial_grid_descriptor_GetNumberOfNodesFromGridID(this, ID)
        class(spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I4P),                      intent(IN)    :: ID
        integer(I8P)                         :: spatial_grid_descriptor_GetNumberOfNodesFromGridID

        spatial_grid_descriptor_GetNumberOfNodesFromGridID = this%NumberOfNodesPerGrid(ID)
    end function spatial_grid_descriptor_GetNumberOfNodesFromGridID

    function spatial_grid_descriptor_GetNumberOfElementsFromGridID(this, ID)
        class(spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I4P),                      intent(IN)    :: ID
        integer(I8P)                         :: spatial_grid_descriptor_GetNumberOfElementsFromGridID

        spatial_grid_descriptor_GetNumberOfElementsFromGridID = this%NumberOfElementsPerGrid(ID)
    end function spatial_grid_descriptor_GetNumberOfElementsFromGridID

    function spatial_grid_descriptor_GetTopologyTypeFromGridID(this, ID)
        class(spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I4P),                      intent(IN)    :: ID
        integer(I4P)                         :: spatial_grid_descriptor_GetTopologyTypeFromGridID

        spatial_grid_descriptor_GetTopologyTypeFromGridID = this%TopologyTypePerGrid(ID)
    end function spatial_grid_descriptor_GetTopologyTypeFromGridID

    function spatial_grid_descriptor_GetGeometryTypeFromGridID(this, ID)
        class(spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I4P),                      intent(IN)    :: ID
        integer(I4P)                         :: spatial_grid_descriptor_GetGeometrytypeFromGridID

        spatial_grid_descriptor_GetGeometryTypeFromGridID = this%GeometryTypePerGrid(ID)
    end function spatial_grid_descriptor_GetGeometryTypeFromGridID

    subroutine spatial_grid_descriptor_initialize(this, MPIEnvironment, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
        class(spatial_grid_descriptor_t), intent(INOUT) :: this
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment
        integer(I8P),                     intent(IN)    :: NumberOfNodes
        integer(I8P),                     intent(IN)    :: NumberOfElements
        integer(I4P),                     intent(IN)    :: TopologyType
        integer(I4P),                     intent(IN)    :: GeometryType

        this%MPIEnvironment => MPIEnvironment
        call this%MPIEnvironment%mpi_allgather_single_int_value(NumberOfNodes, this%NumberOfNodesPerGrid)
        call this%MPIEnvironment%mpi_allgather_single_int_value(NumberOfElements, this%NumberOfElementsPerGrid)
        call this%MPIEnvironment%mpi_allgather_single_int_value(TopologyType, this%TopologyTypePerGrid)
        call this%MPIEnvironment%mpi_allgather_single_int_value(GeometryType, this%GeometryTypePerGrid)

        if(this%MPIEnvironment%is_root()) then
            call this%SetGlobalNumberOfElements(sum(this%NumberOfElementsPerGrid))
            call this%SetGlobalNumberOfNodes(sum(this%NumberOfNodesPerGrid))
        endif

    end subroutine spatial_grid_descriptor_initialize

end module spatial_grid_descriptor
