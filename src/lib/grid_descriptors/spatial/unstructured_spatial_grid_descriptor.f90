module unstructured_spatial_grid_descriptor

use PENF, only : I4P, I8P
use spatial_grid_descriptor
use mpi_environment
use xh5for_utils
use xh5for_parameters

implicit none

private

    type, extends(spatial_grid_descriptor_t) :: unstructured_spatial_grid_descriptor_t
    private
        integer(I8P)                                :: GlobalGeometrySize     = 0  !< Total size of the coordinates of the spatial grid
        integer(I8P)                                :: GlobalTopologySize     = 0  !< Total size of the connectivities of the spatial grid
        integer(I8P),                   allocatable :: TopologySizePerGrid(:)      !< Array of sizes of array connectivities per grid
    contains
        procedure, public :: InitializeUnstructuredWriter       => unst_spatial_grid_descriptor_InitializeUnstructuredWriter
        procedure, public :: InitializeStructuredWriter         => unst_spatial_grid_descriptor_InitializeStructuredWriter

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

        procedure, public :: SetXSizePerGridID                  => unst_spatial_grid_descriptor_SetXSizePerGridID
        procedure, public :: SetYSizePerGridID                  => unst_spatial_grid_descriptor_SetYSizePerGridID
        procedure, public :: SetZSizePerGridID                  => unst_spatial_grid_descriptor_SetZSizePerGridID
        procedure, public :: GetXSizePerGridID                  => unst_spatial_grid_descriptor_GetXSizePerGridID
        procedure, public :: GetYSizePerGridID                  => unst_spatial_grid_descriptor_GetYSizePerGridID
        procedure, public :: GetZSizePerGridID                  => unst_spatial_grid_descriptor_GetZSizePerGridID

        procedure, public :: SetGlobalXSize                     => unst_spatial_grid_descriptor_SetGlobalXSize
        procedure, public :: SetGlobalYSize                     => unst_spatial_grid_descriptor_SetGlobalYSize
        procedure, public :: SetGlobalZSize                     => unst_spatial_grid_descriptor_SetGlobalZSize
        procedure, public :: GetGlobalXSize                     => unst_spatial_grid_descriptor_GetGlobalXSize
        procedure, public :: GetGlobalYSize                     => unst_spatial_grid_descriptor_GetGlobalYSize
        procedure, public :: GetGlobalZSize                     => unst_spatial_grid_descriptor_GetGlobalZSize

        procedure, public :: GetXSizeOffsetPerGridID            => unst_spatial_grid_descriptor_GetXSizeOffsetPerGridID
        procedure, public :: GetYSizeOffsetPerGridID            => unst_spatial_grid_descriptor_GetYSizeOffsetPerGridID
        procedure, public :: GetZSizeOffsetPerGridID            => unst_spatial_grid_descriptor_GetZSizeOffsetPerGridID

        procedure, public :: BroadcastMetadata                  => unst_spatial_grid_descriptor_BroadcastMetadata
        procedure, public :: Free                               => unst_spatial_grid_descriptor_Free
    end type unstructured_spatial_grid_descriptor_t

public:: unstructured_spatial_grid_descriptor_t


contains


    subroutine unst_spatial_grid_descriptor_InitializeUnstructuredWriter(this, MPIEnvironment, NumberOfNodes, NumberOfElements, TopologyType, GeometryType, GridType, StaticGrid)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment    !< MPI environment type
        integer(I8P),                     intent(IN)    :: NumberOfNodes     !< Number of nodes of the current grid
        integer(I8P),                     intent(IN)    :: NumberOfElements  !< Number of elements of the current grid
        integer(I4P),                     intent(IN)    :: TopologyType      !< Topology type of the current grid
        integer(I4P),                     intent(IN)    :: GeometryType      !< Geometry type of the current grid
        integer(I4P),                     intent(IN)    :: GridType          !< Grid type of the current grid
        logical,      optional,           intent(IN)    :: StaticGrid        !< Static Grid flag
        integer(I4P)                                    :: i                 !< Loop index in NumberOfGrids
    !-----------------------------------------------------------------
        call this%Free()
        call this%DefaultInitializeWriter(MPIEnvironment = MPIEnvironment,     &
                                          NumberOfNodes = NumberOfNodes,       &
                                          NumberOfElements = NumberOfElements, &
                                          TopologyType = TopologyType,         &
                                          GeometryType = GeometryType,         &
                                          GridType = GridType,                 &
                                          StaticGrid = StaticGrid)
    end subroutine unst_spatial_grid_descriptor_InitializeUnstructuredWriter


    subroutine unst_spatial_grid_descriptor_InitializeStructuredWriter(this, MPIEnvironment, XDim, YDim, ZDim, GridType, StaticGrid)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment    !< MPI environment type
        integer(I8P),                     intent(IN)    :: XDim              !< Number of points of the X axis
        integer(I8P),                     intent(IN)    :: YDim              !< Number of points of the Y axis
        integer(I8P),                     intent(IN)    :: ZDim              !< Number of points of the Z axis
        integer(I4P),                     intent(IN)    :: GridType          !< Grid type of the current grid
        logical,      optional,           intent(IN)    :: StaticGrid        !< Static Grid flag
        integer(I8P)                                    :: NumberOfNodes     !< Number of nodes of the current grid
        integer(I8P)                                    :: NumberOfElements  !< Number of elements of the current grid
        integer(I4P)                                    :: TopologyType      !< Topology type of the current grid
        integer(I4P)                                    :: GeometryType      !< Geometry type of the current grid
        integer(I4P)                                    :: i                 !< Loop index in NumberOfGrids
    !-----------------------------------------------------------------
        call this%Free()
        select case(GridType)
            case (XDMF_GRID_TYPE_UNSTRUCTURED)
                if(ZDim == 0_I8P) then
                    TopologyType=XDMF_TOPOLOGY_TYPE_QUADRILATERAL
                    GeometryType=XDMF_GEOMETRY_TYPE_XY
                else
                    TopologyType=XDMF_TOPOLOGY_TYPE_HEXAHEDRON
                    GeometryType=XDMF_GEOMETRY_TYPE_XYZ
                endif
            case DEFAULT
                ! Error
        end select
        NumberOfNodes = MAX(1,XDim)*MAX(1,YDim)*MAX(1,ZDim)
        NumberOfElements = (MAX(2,XDim)-1)*(MAX(2,YDim)-1)*(MAX(2,ZDim)-1)
        call this%DefaultInitializeWriter(MPIEnvironment = MPIEnvironment,     &
                                          NumberOfNodes = NumberOfNodes,       &
                                          NumberOfElements = NumberOfElements, &
                                          TopologyType = TopologyType,         &
                                          GeometryType = GeometryType,         &
                                          GridType = GridType,                 &
                                          StaticGrid = StaticGrid)
    end subroutine unst_spatial_grid_descriptor_InitializeStructuredWriter


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


    subroutine unst_spatial_grid_descriptor_SetXSizePerGridID(this, XSize, ID)
    !-----------------------------------------------------------------
    !< Set the the number of X points for a particular grid given its ID.
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: XSize          !< Number Of X points of the grid ID
        integer(I4P),                                  intent(IN)    :: ID             !< Grid identifier
    !-----------------------------------------------------------------
    end subroutine unst_spatial_grid_descriptor_SetXSizePerGridID


    subroutine unst_spatial_grid_descriptor_SetYSizePerGridID(this, YSize, ID)
    !-----------------------------------------------------------------
    !< Set the the number of Y points for a particular grid given its ID.
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: YSize          !< Number Of Y points of the grid ID
        integer(I4P),                                  intent(IN)    :: ID             !< Grid identifier
    !-----------------------------------------------------------------
    end subroutine unst_spatial_grid_descriptor_SetYSizePerGridID


    subroutine unst_spatial_grid_descriptor_SetZSizePerGridID(this, ZSize, ID)
    !-----------------------------------------------------------------
    !< Set the the number of Z points for a particular grid given its ID.
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: ZSize          !< Number Of Z points of the grid ID
        integer(I4P),                                  intent(IN)    :: ID             !< Grid identifier
    !-----------------------------------------------------------------
    end subroutine unst_spatial_grid_descriptor_SetZSizePerGridID


    function unst_spatial_grid_descriptor_GetXSizePerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of X points for a particular grid given its ID
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(IN) :: this  !< structured Spatial grid descriptor type
        integer(I4P),                                  intent(IN) :: ID    !< Grid identifier
        integer(I8P) :: unst_spatial_grid_descriptor_GetXSizePerGridID     !< Number of X points of the grid ID
    !-----------------------------------------------------------------
        unst_spatial_grid_descriptor_GetXSizePerGridID = -1_I8P
    end function unst_spatial_grid_descriptor_GetXSizePerGridID


    function unst_spatial_grid_descriptor_GetYSizePerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of Y points for a particular grid given its ID
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(IN) :: this  !< structured Spatial grid descriptor type
        integer(I4P),                                  intent(IN) :: ID    !< Grid identifier
        integer(I8P) :: unst_spatial_grid_descriptor_GetYSizePerGridID     !< Number of Y points of the grid ID
    !-----------------------------------------------------------------
        unst_spatial_grid_descriptor_GetYSizePerGridID = -1_I8P
    end function unst_spatial_grid_descriptor_GetYSizePerGridID


    function unst_spatial_grid_descriptor_GetZSizePerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of Z points for a particular grid given its ID
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(IN) :: this  !< structured Spatial grid descriptor type
        integer(I4P),                                  intent(IN) :: ID    !< Grid identifier
        integer(I8P) :: unst_spatial_grid_descriptor_GetZSizePerGridID     !< Number of Z points of the grid ID
    !-----------------------------------------------------------------
        unst_spatial_grid_descriptor_GetZSizePerGridID = -1_I8P
    end function unst_spatial_grid_descriptor_GetZSizePerGridID


    subroutine unst_spatial_grid_descriptor_SetGlobalXSize(this, GlobalXSize)
    !-----------------------------------------------------------------
    !< Set the the global number of X points for the spatial grid
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: GlobalXSize    !< Global number Of X points of the spatial grid 
    !-----------------------------------------------------------------
    end subroutine unst_spatial_grid_descriptor_SetGlobalXSize


    subroutine unst_spatial_grid_descriptor_SetGlobalYSize(this, GlobalYSize)
    !-----------------------------------------------------------------
    !< Set the the global number of Y points for the spatial grid
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: GlobalYSize    !< Global number Of Y points of the spatial grid 
    !-----------------------------------------------------------------
    end subroutine unst_spatial_grid_descriptor_SetGlobalYSize


    subroutine unst_spatial_grid_descriptor_SetGlobalZSize(this, GlobalZSize)
    !-----------------------------------------------------------------
    !< Set the the global number of Z points for the spatial grid
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I8P),                                  intent(IN)    :: GlobalZSize    !< Global number Of Z points of the spatial grid 
    !-----------------------------------------------------------------
    end subroutine unst_spatial_grid_descriptor_SetGlobalZSize

    function unst_spatial_grid_descriptor_GetGlobalXSize(this)
    !-----------------------------------------------------------------
    !< Return the global number of X points for the spatial grid
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(IN) :: this  !< structured Spatial grid descriptor type
        integer(I8P) :: unst_spatial_grid_descriptor_GetGlobalXSize        !< Global number of X points of the grid ID
    !-----------------------------------------------------------------
        unst_spatial_grid_descriptor_GetGlobalXSize = -1_I8P
    end function unst_spatial_grid_descriptor_GetGlobalXSize


    function unst_spatial_grid_descriptor_GetGlobalYSize(this)
    !-----------------------------------------------------------------
    !< Return the global number of Y points for the spatial grid
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(IN) :: this  !< structured Spatial grid descriptor type
        integer(I8P) :: unst_spatial_grid_descriptor_GetGlobalYSize        !< Global number of Y points of the grid ID
    !-----------------------------------------------------------------
        unst_spatial_grid_descriptor_GetGlobalYSize = -1_I8P
    end function unst_spatial_grid_descriptor_GetGlobalYSize


    function unst_spatial_grid_descriptor_GetGlobalZSize(this)
    !-----------------------------------------------------------------
    !< Return the global number of Z points for the spatial grid
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(IN) :: this  !< structured Spatial grid descriptor type
        integer(I8P) :: unst_spatial_grid_descriptor_GetGlobalZSize        !< Global number of Z points of the grid ID
    !-----------------------------------------------------------------
        unst_spatial_grid_descriptor_GetGlobalZSize = 1_I8P
    end function unst_spatial_grid_descriptor_GetGlobalZSize


    function unst_spatial_grid_descriptor_GetXSizeOffsetPerGridID(this, ID) result(XSizeOffset)
    !-----------------------------------------------------------------
    !< Return the offset of X size given a grid ID
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I4P),                                  intent(IN)    :: ID
        integer(I8P)                                                 :: XSizeOffset
    !----------------------------------------------------------------- 
        XSizeOffset = 1_I8P
    end function unst_spatial_grid_descriptor_GetXSizeOffsetPerGridID

    function unst_spatial_grid_descriptor_GetYSizeOffsetPerGridID(this, ID) result(YSizeOffset)
    !----------------------------------------------------------------- 
    !< Return the offset of Y size given a grid ID
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I4P),                                  intent(IN)    :: ID
        integer(I8P)                                                 :: YSizeOffset
    !----------------------------------------------------------------- 
        YSizeOffset = 1_I8P
    end function unst_spatial_grid_descriptor_GetYSizeOffsetPerGridID


    function unst_spatial_grid_descriptor_GetZSizeOffsetPerGridID(this, ID) result(ZSizeOffset)
    !----------------------------------------------------------------- 
    !< Return the offset of Z size given a grid ID
    !< Not used with unstructured grids
    !< TODO: Implement in the parent class and raise an exception
    !----------------------------------------------------------------- 
        class(unstructured_spatial_grid_descriptor_t), intent(INOUT) :: this
        integer(I4P),                                  intent(IN)    :: ID
        integer(I8P)                                                 :: ZSizeOffset
    !----------------------------------------------------------------- 
        ZSizeOffset = 1_I8P
    end function unst_spatial_grid_descriptor_GetZSizeOffsetPerGridID


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
