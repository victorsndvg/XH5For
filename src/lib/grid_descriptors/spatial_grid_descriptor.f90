module spatial_grid_descriptor
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use IR_Precision, only : I4P, I8P, R4P, R8P
use mpi_environment
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
        integer(I4P),                   allocatable :: TopologyTypePerGrid(:)      !< Array of topology type per grid
        integer(I4P),                   allocatable :: GeometryTypePerGrid(:)      !< Array of geometry type per grid
        type(spatial_grid_attribute_t), allocatable :: AttributesPerGrid(:)        !< Array of attribute metadata per grid
        type(mpi_env_t), pointer                    :: MPIEnvironment => null()    !< MPI environment 

    contains
    private
        procedure         :: SetGlobalNumberOfNodes         => spatial_grid_descriptor_SetGlobalNumberOfNodes
        procedure         :: SetGlobalNumberOfElements      => spatial_grid_descriptor_SetGlobalNumberOfElements
        procedure, public :: GetGlobalNumberOfNodes         => spatial_grid_descriptor_GetGlobalNumberOfNodes
        procedure, public :: GetGlobalNumberOfElements      => spatial_grid_descriptor_GetGlobalNumberOfElements
        procedure, public :: SetNumberOfNodesByGridID       => spatial_grid_descriptor_SetNumberOfNodesByGridID
        procedure, public :: SetNumberOfElementsByGridID    => spatial_grid_descriptor_SetNumberOfElementsByGridID
        procedure, public :: SetTopologyTypeByGridID        => spatial_grid_descriptor_SetTopologyTypeByGridID
        procedure, public :: SetGeometryTypeByGridID        => spatial_grid_descriptor_SetGeometryTypeByGridID
        procedure, public :: AllocateAttributesByGridID     => spatial_grid_descriptor_AllocateAttributesByGrid
        procedure, public :: SetAttributeTypeByGridID       => spatial_grid_descriptor_SetAttributeTypeByGridID
!        procedure, public :: SetAttributeNameByGridID       => spatial_grid_descriptor_SetAttributeNameByGridID
!        procedure, public :: SetAttributeCenterByGridID     => spatial_grid_descriptor_SetAttributeCenterByGridID
        procedure, public :: GetNumberOfNodesFromGridID     => spatial_grid_descriptor_GetNumberOfNodesFromGridID
        procedure, public :: GetNumberOfElementsFromGridID  => spatial_grid_descriptor_GetNumberOfElementsFromGridID
        procedure, public :: GetTopologyTypeFromGridID      => spatial_grid_descriptor_GetTopologyTypeFromGridID
        procedure, public :: GetGeometryTypeFromGridID      => spatial_grid_descriptor_GetGeometryTypeFromGridID
        procedure, public :: GetNodeOffsetFromGridID        => spatial_grid_descriptor_GetNodeOffsetFromGridID
        procedure, public :: GetElementOffsetFromGridID     => spatial_grid_descriptor_GetElementOffsetFromGridID
        procedure, public :: Initialize                     => spatial_grid_descriptor_Initialize
        procedure, public :: Allocate                       => spatial_grid_descriptor_Allocate
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
        call this%free()
        this%NumberOfGrids = NumberOfGrids
        allocate(this%NumberOfNodesPerGrid(NumberOfGrids))
        allocate(this%NumberOfElementsPerGrid(NumberOfGrids))
        allocate(this%TopologyTypePerGrid(NumberOfGrids))
        allocate(this%GeometryTypePerGrid(NumberOfGrids))
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
        integer(I4P),                     intent(IN)    :: NumberOfNodes   !< Number of nodes of the grid ID
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
        this%TopologyTypePerGrid(ID+1) = TopologyType
    end subroutine spatial_grid_descriptor_SetTopologyTypeByGridID


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
        this%GeometryTypePerGrid(ID+1) = GeometryType
    end subroutine spatial_grid_descriptor_SetGeometryTypeByGridID


    subroutine spatial_grid_descriptor_SetAttributeTypeByGridID(this, AttributeType, ID, NumberOfAttribute)
    !-----------------------------------------------------------------
    !< Set the topology type of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(spatial_grid_descriptor_t), intent(INOUT) :: this              !< Spatial grid descriptor type
        integer(I4P),                     intent(IN)    :: AttributeType     !< Geometry type of the grid ID
        integer(I4P),                     intent(IN)    :: ID                !< Grid identifier
        integer(I4P),                     intent(IN)    :: NumberOfAttribute !< NumberOfAttribute
    !-----------------------------------------------------------------
        call this%AttributesPerGrid(ID+1)%Attributes_info(NumberOfAttribute)%SetType(Type = AttributeType)
    end subroutine spatial_grid_descriptor_SetAttributeTypeByGridID


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
        this%NumberOfGrids = size(this%NumberOfNodesPerGrid, dim=1)
    end subroutine spatial_grid_descriptor_initialize

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
        if(allocated(this%TopologyTypePerGrid))     deallocate(this%TopologyTypePerGrid)
        if(allocated(this%GeometryTypePerGrid))     deallocate(this%GeometryTypePerGrid)
        if(allocated(this%AttributesPerGrid)) then
            do i=1, size(this%AttributesPerGrid, dim=1)
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
