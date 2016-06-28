module structured_spatial_grid_descriptor

use IR_Precision, only : I4P, I8P
use spatial_grid_descriptor
use mpi_environment
use xh5for_utils
use xh5for_parameters

implicit none

private

    type, extends(spatial_grid_descriptor_t) :: structured_spatial_grid_descriptor_t
    private
        integer(I8P)                                :: GlobalXSize = 0  !< Total size of X coordinates of the spatial grid
        integer(I8P)                                :: GlobalYSize = 0  !< Total size of Y coordinates of the spatial grid
        integer(I8P)                                :: GlobalZSize = 0  !< Total size of Z coordinates of the spatial grid
        integer(I8P),                   allocatable :: XSizePerGrid(:)  !< Array of sizes of X coordinates per grid
        integer(I8P),                   allocatable :: YSizePerGrid(:)  !< Array of sizes of Y coordinates per grid
        integer(I8P),                   allocatable :: ZSizePerGrid(:)  !< Array of sizes of Z coordinates per grid
    contains
        procedure, public :: InitializeUnstructuredWriter   => str_spatial_grid_descriptor_InitializeUnstructuredWriter
        procedure, public :: InitializeStructuredWriter     => str_spatial_grid_descriptor_InitializeStructuredWriter

        procedure, public :: Allocate                       => str_spatial_grid_descriptor_Allocate

        procedure, public :: SetGlobalXSize                 => str_spatial_grid_descriptor_SetGlobalXSize
        procedure, public :: SetGlobalYSize                 => str_spatial_grid_descriptor_SetGlobalYSize
        procedure, public :: SetGlobalZSize                 => str_spatial_grid_descriptor_SetGlobalZSize
        procedure, public :: GetGlobalXSize                 => str_spatial_grid_descriptor_GetGlobalXSize
        procedure, public :: GetGlobalYSize                 => str_spatial_grid_descriptor_GetGlobalYSize
        procedure, public :: GetGlobalZSize                 => str_spatial_grid_descriptor_GetGlobalZSize

        procedure, public :: SetXSizePerGridID              => str_spatial_grid_descriptor_SetXSizePerGridID
        procedure, public :: SetYSizePerGridID              => str_spatial_grid_descriptor_SetYSizePerGridID
        procedure, public :: SetZSizePerGridID              => str_spatial_grid_descriptor_SetZSizePerGridID
        procedure, public :: GetXSizePerGridID              => str_spatial_grid_descriptor_GetXSizePerGridID
        procedure, public :: GetYSizePerGridID              => str_spatial_grid_descriptor_GetYSizePerGridID
        procedure, public :: GetZSizePerGridID              => str_spatial_grid_descriptor_GetZSizePerGridID

        procedure, public :: GetXSizeOffsetPerGridID        => str_spatial_grid_descriptor_GetXSizeOffsetPerGridID
        procedure, public :: GetYSizeOffsetPerGridID        => str_spatial_grid_descriptor_GetYSizeOffsetPerGridID
        procedure, public :: GetZSizeOffsetPerGridID        => str_spatial_grid_descriptor_GetZSizeOffsetPerGridID

        procedure, public :: SetGlobalTopologySize          => str_spatial_grid_descriptor_SetGlobalTopologySize
        procedure, public :: GetGlobalTopologySize          => str_spatial_grid_descriptor_GetGlobalTopologySize
        procedure, public :: SetTopologySizePerGridID       => str_spatial_grid_descriptor_SetTopologySizePerGridID
        procedure, public :: GetTopologySizePerGridID       => str_spatial_grid_descriptor_GetTopologySizePerGridID
        procedure, public :: GetTopologySizeOffsetPerGridID => str_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID

        procedure, public :: SetGlobalGeometrySize          => str_spatial_grid_descriptor_SetGlobalGeometrySize
        procedure, public :: GetGlobalGeometrySize          => str_spatial_grid_descriptor_GetGlobalGeometrySize
        procedure, public :: SetGeometrySizePerGridID       => str_spatial_grid_descriptor_SetGeometrySizePerGridID
        procedure, public :: GetGeometrySizePerGridID       => str_spatial_grid_descriptor_GetGeometrySizePerGridID
        procedure, public :: GetGeometrySizeOffsetPerGridID => str_spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID

        procedure, public :: BroadcastMetadata              => str_spatial_grid_descriptor_BroadcastMetadata
        procedure, public :: Free                           => str_spatial_grid_descriptor_Free
    end type structured_spatial_grid_descriptor_t

public:: structured_spatial_grid_descriptor_t


contains


    subroutine str_spatial_grid_descriptor_InitializeUnstructuredWriter(this, MPIEnvironment, NumberOfNodes, NumberOfElements, TopologyType, GeometryType, GridType, StaticGrid)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this  !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment   !< MPI environment type
        integer(I8P),                     intent(IN)    :: NumberOfNodes    !< Number of nodes of the current grid
        integer(I8P),                     intent(IN)    :: NumberOfElements !< Number of elements of the current grid
        integer(I4P),                     intent(IN)    :: TopologyType     !< Topology type of the current grid
        integer(I4P),                     intent(IN)    :: GeometryType     !< Geometry type of the current grid
        integer(I4P),                     intent(IN)    :: GridType         !< Grid type of the current grid
        logical,      optional,           intent(IN)    :: StaticGrid        !< Static Grid flag
        integer(I4P)                                    :: i                !< Loop index in NumberOfGrids
    !-----------------------------------------------------------------
        call this%Free()
        ! Error. Not supported
    end subroutine str_spatial_grid_descriptor_InitializeUnstructuredWriter


    subroutine str_spatial_grid_descriptor_InitializeStructuredWriter(this, MPIEnvironment, XDim, YDim, ZDim, GridType, StaticGrid)
    !-----------------------------------------------------------------
    !< Initilized the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this  !< Spatial grid descriptor type
        type(mpi_env_t), target,          intent(IN)    :: MPIEnvironment   !< MPI environment type
        integer(I8P),                     intent(IN)    :: XDim             !< Number of points of the X axis
        integer(I8P),                     intent(IN)    :: YDim             !< Number of points of the Y axis
        integer(I8P),                     intent(IN)    :: ZDim             !< Number of points of the Z axis
        integer(I4P),                     intent(IN)    :: GridType         !< Grid type of the current grid
        logical,      optional,           intent(IN)    :: StaticGrid       !< Static grid flag
        integer(I8P)                                    :: NumberOfNodes    !< Number of nodes of the current grid
        integer(I8P)                                    :: NumberOfElements !< Number of elements of the current grid
        integer(I4P)                                    :: TopologyType     !< Topology type of the current grid
        integer(I4P)                                    :: GeometryType     !< Geometry type of the current grid
        integer(I4P)                                    :: i                !< Loop index in NumberOfGrids
    !-----------------------------------------------------------------
        call this%Free()
        select case(GridType)
            case (XDMF_GRID_TYPE_CURVILINEAR)
                if(ZDim == 0_I8P) then
                    TopologyType=XDMF_TOPOLOGY_TYPE_2DSMESH
                    GeometryType=XDMF_GEOMETRY_TYPE_VXVY
                else
                    TopologyType=XDMF_TOPOLOGY_TYPE_3DSMESH
                    GeometryType=XDMF_GEOMETRY_TYPE_VXVYVZ
                endif
            case (XDMF_GRID_TYPE_RECTILINEAR)
                if(ZDim == 0_I8P) then
                    TopologyType=XDMF_TOPOLOGY_TYPE_2DRECTMESH
                    GeometryType=XDMF_GEOMETRY_TYPE_VXVY
                else
                    TopologyType=XDMF_TOPOLOGY_TYPE_3DRECTMESH
                    GeometryType=XDMF_GEOMETRY_TYPE_VXVYVZ
                endif
            case (XDMF_GRID_TYPE_REGULAR)
                if(ZDim == 0_I8P) then
                    TopologyType=XDMF_TOPOLOGY_TYPE_2DCORECTMESH
                    GeometryType=XDMF_GEOMETRY_TYPE_ORIGIN_DXDY
                else
                    TopologyType=XDMF_TOPOLOGY_TYPE_3DCORECTMESH
                    GeometryType=XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ
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
        call MPIEnvironment%mpi_allgather(XDim, this%XSizePerGrid)
        call MPIEnvironment%mpi_allgather(YDim, this%YSizePerGrid)
        call MPIEnvironment%mpi_allgather(ZDim, this%ZSizePerGrid)
        call this%SetGlobalXsize(sum(this%XSizePerGrid))
        call this%SetGlobalYsize(sum(this%YSizePerGrid))
        call this%SetGlobalZsize(sum(this%ZSizePerGrid))
    end subroutine str_spatial_grid_descriptor_InitializeStructuredWriter


    subroutine str_spatial_grid_descriptor_Allocate(this, NumberOfGrids)
    !-----------------------------------------------------------------
    !< Set the total number of nodes of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this          !< Spatial grid descriptor type
        integer(I4P),                                intent(IN)    :: NumberOfGrids !< Total number of grids of the spatial grid
    !----------------------------------------------------------------- 
        call this%SetNumberOfGrids(NumberOfGrids = NumberOfGrids)
        call this%AllocateNumberOfNodesPerGrid(NumberOfGrids = NumberOfGrids)
        call this%AllocateNumberOfElementsPerGrid(NumberOfGrids = NumberOfGrids)
        call this%AllocateTopologyTypePerGrid(NumberOfGrids = NumberOfGrids)    
        call this%AllocateGeometryTypePerGrid(NumberOfGrids = NumberOfGrids)
        if(allocated(this%XSizePerGrid)) deallocate(this%XSizePerGrid)
        if(allocated(this%YSizePerGrid)) deallocate(this%YSizePerGrid)
        if(allocated(this%ZSizePerGrid)) deallocate(this%ZSizePerGrid)
        allocate(this%XSizePerGrid(NumberOfGrids))
        allocate(this%YSizePerGrid(NumberOfGrids))
        allocate(this%ZSizePerGrid(NumberOfGrids))
    end subroutine str_spatial_grid_descriptor_Allocate


    subroutine str_spatial_grid_descriptor_SetGlobalXSize(this, GlobalXSize)
    !-----------------------------------------------------------------
    !< Set the total number of X points of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this         !< structured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: GlobalXSize  !< Total size of X points of the spatial grid
    !----------------------------------------------------------------- 
        this%GlobalXSize = GlobalXSize
    end subroutine str_spatial_grid_descriptor_SetGlobalXSize


    subroutine str_spatial_grid_descriptor_SetGlobalYSize(this, GlobalYSize)
    !-----------------------------------------------------------------
    !< Set the total number Y of points of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this         !< structured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: GlobalYSize  !< Total size of Y points of the spatial grid
    !----------------------------------------------------------------- 
        this%GlobalYSize = GlobalYSize
    end subroutine str_spatial_grid_descriptor_SetGlobalYSize


    subroutine str_spatial_grid_descriptor_SetGlobalZSize(this, GlobalZSize)
    !-----------------------------------------------------------------
    !< Set the total number Z of points of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this         !< structured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: GlobalZSize  !< Total size of Z points of the spatial grid
    !----------------------------------------------------------------- 
        this%GlobalZSize = GlobalZSize
    end subroutine str_spatial_grid_descriptor_SetGlobalZSize


    function str_spatial_grid_descriptor_GetGlobalXSize(this) result(GlobalXSize)
    !-----------------------------------------------------------------
    !< Get the total number X of points of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(IN) :: this         !< structured Spatial grid descriptor type
        integer(I8P)                                            :: GlobalXSize  !< Total size of X points of the spatial grid
    !----------------------------------------------------------------- 
        GlobalXSize = this%GlobalXSize
    end function str_spatial_grid_descriptor_GetGlobalXSize


    function str_spatial_grid_descriptor_GetGlobalYSize(this) result(GlobalYSize)
    !-----------------------------------------------------------------
    !< Get the total number Y of points of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(IN) :: this         !< structured Spatial grid descriptor type
        integer(I8P)                                            :: GlobalYSize  !< Total size of Y points of the spatial grid
    !----------------------------------------------------------------- 
        GlobalYSize = this%GlobalYSize
    end function str_spatial_grid_descriptor_GetGlobalYSize


    function str_spatial_grid_descriptor_GetGlobalZSize(this) result(GlobalZSize)
    !-----------------------------------------------------------------
    !< Get the total number Z of points of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(IN) :: this         !< structured Spatial grid descriptor type
        integer(I8P)                                            :: GlobalZSize  !< Total size of Z points of the spatial grid
    !----------------------------------------------------------------- 
        GlobalZSize = this%GlobalZSize
    end function str_spatial_grid_descriptor_GetGlobalZSize


    subroutine str_spatial_grid_descriptor_SetXSizePerGridID(this, XSize, ID)
    !-----------------------------------------------------------------
    !< Set the the number of X points for a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: XSize          !< Number Of X points of the grid ID
        integer(I4P),                                intent(IN)    :: ID             !< Grid identifier
        type(mpi_env_t), pointer                                   :: MPIEnvironment !< MPI Environment pointer
    !-----------------------------------------------------------------
        if(.not. allocated(this%XSizePerGrid)) then
            allocate(this%XSizePerGrid(this%GetNumberOfGrids()))
            MPIEnvironment => this%GetMPIEnvironment()
            call MPIEnvironment%mpi_allgather(XSize, this%XSizePerGrid)
            call this%SetGlobalXSize(sum(this%XSizePerGrid))
        endif
        this%XSizePerGrid(ID+1) = XSize
    end subroutine str_spatial_grid_descriptor_SetXSizePerGridID


    subroutine str_spatial_grid_descriptor_SetYSizePerGridID(this, YSize, ID)
    !-----------------------------------------------------------------
    !< Set the number of Y points for a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: YSize          !< Number Of Y points of the grid ID
        integer(I4P),                                intent(IN)    :: ID             !< Grid identifier
        type(mpi_env_t), pointer                                   :: MPIEnvironment !< MPI Environment pointer
    !-----------------------------------------------------------------
        if(.not. allocated(this%YSizePerGrid)) then
            allocate(this%YSizePerGrid(this%GetNumberOfGrids()))
            MPIEnvironment => this%GetMPIEnvironment()
            call MPIEnvironment%mpi_allgather(YSize, this%YSizePerGrid)
            call this%SetGlobalYSize(sum(this%YSizePerGrid))
        endif
        this%YSizePerGrid(ID+1) = YSize
    end subroutine str_spatial_grid_descriptor_SetYSizePerGridID


    subroutine str_spatial_grid_descriptor_SetZSizePerGridID(this, ZSize, ID)
    !-----------------------------------------------------------------
    !< Set the number of Z points for a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: ZSize          !< Number Of Z points of the grid ID
        integer(I4P),                                intent(IN)    :: ID             !< Grid identifier
        type(mpi_env_t), pointer                                   :: MPIEnvironment !< MPI Environment pointer
    !-----------------------------------------------------------------
        if(.not. allocated(this%ZSizePerGrid)) then
            allocate(this%ZSizePerGrid(this%GetNumberOfGrids()))
            MPIEnvironment => this%GetMPIEnvironment()
            call MPIEnvironment%mpi_allgather(ZSize, this%ZSizePerGrid)
            call this%SetGlobalZSize(sum(this%ZSizePerGrid))
        endif
        this%ZSizePerGrid(ID+1) = ZSize
    end subroutine str_spatial_grid_descriptor_SetZSizePerGridID


    function str_spatial_grid_descriptor_GetXSizePerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of X points for a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(IN) :: this  !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN) :: ID    !< Grid identifier
        integer(I8P) :: str_spatial_grid_descriptor_GetXSizePerGridID    !< Number of X points of the grid ID
    !-----------------------------------------------------------------
        str_spatial_grid_descriptor_GetXSizePerGridID = this%XSizePerGrid(ID+1)
    end function str_spatial_grid_descriptor_GetXSizePerGridID


    function str_spatial_grid_descriptor_GetYSizePerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of Y points for a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(IN) :: this  !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN) :: ID    !< Grid identifier
        integer(I8P) :: str_spatial_grid_descriptor_GetYSizePerGridID    !< Number of Y points of the grid ID
    !-----------------------------------------------------------------
        str_spatial_grid_descriptor_GetYSizePerGridID = this%YSizePerGrid(ID+1)
    end function str_spatial_grid_descriptor_GetYSizePerGridID


    function str_spatial_grid_descriptor_GetZSizePerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the number of Z points for a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(IN) :: this  !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN) :: ID    !< Grid identifier
        integer(I8P) :: str_spatial_grid_descriptor_GetZSizePerGridID    !< Number of Z points of the grid ID
    !-----------------------------------------------------------------
        str_spatial_grid_descriptor_GetZSizePerGridID = this%ZSizePerGrid(ID+1)
    end function str_spatial_grid_descriptor_GetZSizePerGridID


    function str_spatial_grid_descriptor_GetXSizeOffsetPerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the X offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this   !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN)    :: ID     !< Grid identifier
        integer(I8P) :: str_spatial_grid_descriptor_GetXSizeOffsetPerGridID  !< X size offset of a grid
    !-----------------------------------------------------------------
        str_spatial_grid_descriptor_GetXSizeOffsetPerGridID = sum(this%XSizePerGrid(:ID))
    end function str_spatial_grid_descriptor_GetXSizeOffsetPerGridID


    function str_spatial_grid_descriptor_GetYSizeOffsetPerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the Y offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this   !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN)    :: ID     !< Grid identifier
        integer(I8P) :: str_spatial_grid_descriptor_GetYSizeOffsetPerGridID  !< Y size offset of a grid
    !-----------------------------------------------------------------
        str_spatial_grid_descriptor_GetYSizeOffsetPerGridID = sum(this%YSizePerGrid(:ID))
    end function str_spatial_grid_descriptor_GetYSizeOffsetPerGridID


    function str_spatial_grid_descriptor_GetZSizeOffsetPerGridID(this, ID)
    !-----------------------------------------------------------------
    !< Return the Z offset of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this   !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN)    :: ID     !< Grid identifier
        integer(I8P) :: str_spatial_grid_descriptor_GetZSizeOffsetPerGridID  !< Z size offset of a grid
    !-----------------------------------------------------------------
        str_spatial_grid_descriptor_GetZSizeOffsetPerGridID = sum(this%ZSizePerGrid(:ID))
    end function str_spatial_grid_descriptor_GetZSizeOffsetPerGridID


    subroutine str_spatial_grid_descriptor_SetTopologySizePerGridID(this, TopologySize, ID, Dimension)
    !-----------------------------------------------------------------
    !< Set the topology size of a particular grid given its ID
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< Unstructured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: TopologySize   !< Topology size of the grid ID
        integer(I4P),                                intent(IN)    :: ID             !< Grid identifier
        integer(I4P), optional,                      intent(IN)    :: Dimension      !< Dimension of the topology (1=X,2=Y,3=Z)
        integer(I4P)                                               :: AuxDim         !< Dimension aux variable
    !-----------------------------------------------------------------
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        call this%SetGeometrySizePerGridID(GeometrySize = TopologySize, ID = ID, Dimension = AuxDim)
    end subroutine str_spatial_grid_descriptor_SetTopologySizePerGridID


    function str_spatial_grid_descriptor_GetTopologySizePerGridID(this, ID, Dimension) result(TopologySize)
    !-----------------------------------------------------------------
    !< Return the Geometry size of a particular grid given its ID (and dimension)
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN)    :: ID             !< Grid identifier
        integer(I4P), optional,                      intent(IN)    :: Dimension      !< Dimension of the topology (1=X,2=Y,3=Z)
        integer(I8P)                                               :: SpaceDimension !< Space dimension
        integer(I8P)                                               :: TopologySize   !< Topology Size of a grid
        integer(I4P)                                               :: AuxDim         !< Dimension aux variable
    !-----------------------------------------------------------------
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        TopologySize = this%GetGeometrySizePerGridID(ID=ID, Dimension=AuxDim)
    end function str_spatial_grid_descriptor_GetTopologySizePerGridID


    subroutine str_spatial_grid_descriptor_SetGlobalTopologySize(this, GlobalTopologySize, Dimension)
    !-----------------------------------------------------------------
    !< Set the total Geometry size of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this                !< structured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: GlobalTopologySize  !< Total size of coordinates of the spatial grid
        integer(I4P), optional,                      intent(IN)    :: Dimension           !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I4P)                                               :: AuxDim              !< Dimension aux variable
    !----------------------------------------------------------------- 
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        call this%SetGlobalGeometrySize(GlobalGeometrySize = GlobalTopologySize, Dimension = AuxDim)
    end subroutine str_spatial_grid_descriptor_SetGlobalTopologySize


    function str_spatial_grid_descriptor_GetGlobalTopologySize(this, Dimension) result(GlobalTopologySize)
    !-----------------------------------------------------------------
    !< Set the total topology size of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(IN) :: this               !< Unstructured Spatial grid descriptor type
        integer(I4P), optional,                      intent(IN) :: Dimension          !< Dimension of the topology (1=X,2=Y,3=Z)
        integer(I8P)                                            :: GlobalTopologySize !< Topology size of the grid ID
        integer(I4P)                                            :: AuxDim             !< Dimension aux variable
    !-----------------------------------------------------------------
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        GlobalTopologySize = this%GetGlobalGeometrySize(Dimension = AuxDim)
    end function str_spatial_grid_descriptor_GetGlobalTopologySize


    function str_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID(this, ID, Dimension) result(Offset)
    !-----------------------------------------------------------------
    !< Return the Topology size offset of a particular grid given its ID (and dimension)
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN)    :: ID             !< Grid identifier
        integer(I4P), optional,                      intent(IN)    :: Dimension      !< Dimension of the Topology (1=X,2=Y,3=Z)
        integer(I8P)                                               :: SpaceDimension !< Space dimension
        integer(I8P)                                               :: Offset         !< Topology size offset of a grid
        integer(I4P)                                               :: AuxDim         !< Dimension aux variable
    !-----------------------------------------------------------------
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        select case (AuxDim)
            case (1) ! X
                Offset = this%GetXSizeOffsetPerGridID(ID=ID)
            case (2) ! Y
                Offset = this%GetYSizeOffsetPerGridID(ID=ID)
            case (3) ! Z
                Offset = this%GetZSizeOffsetPerGridID(ID=ID)
        end select
    end function str_spatial_grid_descriptor_GetTopologySizeOffsetPerGridID


    subroutine str_spatial_grid_descriptor_SetGlobalGeometrySize(this, GlobalGeometrySize, Dimension)
    !-----------------------------------------------------------------
    !< Set the total Geometry size of the spatial grid
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this                !< structured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: GlobalGeometrySize  !< Total size of coordinates of the spatial grid
        integer(I4P), optional,                      intent(IN)    :: Dimension           !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I4P)                                               :: AuxDim              !< Dimension aux variable
    !----------------------------------------------------------------- 
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        select case (AuxDim)
            case (1) ! X
                this%GlobalXSize = GlobalGeometrySize
            case (2) ! Y
                this%GlobalYSize = GlobalGeometrySize
            case (3) ! Z
                this%GlobalZSize = GlobalGeometrySize
        end select
    end subroutine str_spatial_grid_descriptor_SetGlobalGeometrySize


    function str_spatial_grid_descriptor_GetGlobalGeometrySize(this, Dimension) result(GlobalGeometrySize)
    !-----------------------------------------------------------------
    !< Get the total geometry size of the spatial grid  (with dimension)
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(IN) :: this               !< structured Spatial grid descriptor type
        integer(I4P), optional,                      intent(IN) :: Dimension          !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I8P)                                            :: GlobalGeometrySize !< Total size of coordinates of the spatial grid
        integer(I8P)                                            :: SpaceDimension     !< Space dimension
        integer(I4P)                                            :: AuxDim             !< Dimension aux variable
        type(mpi_env_t), pointer                                :: MPIEnvironment     !< MPI Environmnet pointer
    !----------------------------------------------------------------- 
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        MPIEnvironment => this%GetMPIEnvironment()
        select case (AuxDim)
            case (1) ! X
                GlobalGeometrySize = this%GlobalXSize
            case (2) ! Y
                GlobalGeometrySize = this%GlobalYSize
            case (3) ! Z
                GlobalGeometrySize = this%GlobalZSize
        end select
    end function str_spatial_grid_descriptor_GetGlobalGeometrySize


    subroutine str_spatial_grid_descriptor_SetGeometrySizePerGridID(this, GeometrySize, ID, Dimension)
    !-----------------------------------------------------------------
    !< Set the geometry size of a particular grid given its ID (and dimension)
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this             !< structured Spatial grid descriptor type
        integer(I8P),                                intent(IN)    :: GeometrySize     !< Geometry size of the grid ID
        integer(I4P),                                intent(IN)    :: ID               !< Grid identifier
        integer(I4P), optional,                      intent(IN)    :: Dimension        !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I8P)                                               :: SpaceDimension   !< Space dimension
        integer(I4P)                                               :: AuxDim           !< Dimension aux variable
    !-----------------------------------------------------------------
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        select case (AuxDim)
            case (1) ! X
                call this%SetXSizePerGridID(XSize = GeometrySize, ID = ID) 
            case (2) ! Y
                call this%SetYSizePerGridID(YSize = GeometrySize, ID = ID) 
            case (3) ! Z
                call this%SetZSizePerGridID(ZSize = GeometrySize, ID = ID) 
        end select
    end subroutine str_spatial_grid_descriptor_SetGeometrySizePerGridID


    function str_spatial_grid_descriptor_GetGeometrySizePerGridID(this, ID, Dimension) result(GeometrySize)
    !-----------------------------------------------------------------
    !< Return the Geometry size of a particular grid given its ID (and dimension)
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN)    :: ID             !< Grid identifier
        integer(I4P), optional,                      intent(IN)    :: Dimension      !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I8P)                                               :: SpaceDimension !< Space dimension
        integer(I8P)                                               :: GeometrySize   !< Geometry Size of a grid
        integer(I4P)                                               :: AuxDim         !< Dimension aux variable
    !-----------------------------------------------------------------
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        select case (AuxDim)
            case (1) ! X
                GeometrySize = this%GetXSizePerGridID(ID = ID) 
            case (2) ! Y
                GeometrySize = this%GetYSizePerGridID(ID = ID) 
            case (3) ! Z
                GeometrySize = this%GetZSizePerGridID(ID = ID) 
        end select
    end function str_spatial_grid_descriptor_GetGeometrySizePerGridID


    function str_spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID(this, ID, Dimension) result(Offset)
    !-----------------------------------------------------------------
    !< Return the geometry size offset of a particular grid given its ID (and dimension)
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this           !< structured Spatial grid descriptor type
        integer(I4P),                                intent(IN)    :: ID             !< Grid identifier
        integer(I4P), optional,                      intent(IN)    :: Dimension      !< Dimension of the geometry (1=X,2=Y,3=Z)
        integer(I8P)                                               :: SpaceDimension !< Space dimension
        integer(I8P)                                               :: Offset         !< Geometry size offset of a grid
        integer(I4P)                                               :: AuxDim         !< Dimension aux variable
    !-----------------------------------------------------------------
        AuxDim = 1 ! X
        if(present(Dimension)) AuxDim = Dimension
        select case (AuxDim)
            case (1) ! X
                Offset = this%GetXSizeOffsetPerGridID(ID=ID)
            case (2) ! Y
                Offset = this%GetYSizeOffsetPerGridID(ID=ID)
            case (3) ! Z
                Offset = this%GetZSizeOffsetPerGridID(ID=ID)
        end select
    end function str_spatial_grid_descriptor_GetGeometrySizeOffsetPerGridID


    subroutine str_spatial_grid_descriptor_BroadcastMetadata(this)
    !-----------------------------------------------------------------
    !< Broadcast metadata after XDMF parsing
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this             !< structured Spatial grid descriptor type
        type(mpi_env_t), pointer                                   :: MPIEnvironment !< MPI Environmnet pointer
    !-----------------------------------------------------------------
        MPIEnvironment => this%GetMPIEnvironment()
        call MPIEnvironment%mpi_broadcast(this%XSizePerGrid)
        call MPIEnvironment%mpi_broadcast(this%YSizePerGrid)
        call MPIEnvironment%mpi_broadcast(this%ZSizePerGrid)
        call this%SetGlobalXSize(sum(this%XSizePerGrid))
        call this%SetGlobalYSize(sum(this%YSizePerGrid))
        call this%SetGlobalZSize(sum(this%ZSizePerGrid))
        call this%DefaultBroadcastMetadata()
    end subroutine str_spatial_grid_descriptor_BroadcastMetadata


    subroutine str_spatial_grid_descriptor_Free(this)
    !-----------------------------------------------------------------
    !< Free the spatial grid descriptor type
    !----------------------------------------------------------------- 
        class(structured_spatial_grid_descriptor_t), intent(INOUT) :: this !< Spatial grid descriptor type
        integer(I4P)                                               :: i    !< Loop index in NumberOfGrids
        integer(I4P)                                               :: j    !< Loop index in NumberOfAttributes
    !----------------------------------------------------------------- 
        if(allocated(this%XSizePerGrid)) deallocate(this%XSizePerGrid)
        if(allocated(this%YSizePerGrid)) deallocate(this%YSizePerGrid)
        if(allocated(this%ZSizePerGrid)) deallocate(this%ZSizePerGrid)
        call This%SetGlobalNumberOfNodes(GlobalNumberOfNodes = 0_I8P)
        call This%SetGlobalNumberOfElements(GlobalNumberOfElements = 0_I8P)
        call this%DeallocateNumberOfNodesPerGrid()
        call this%DeallocateNumberOfElementsPerGrid()
        call this%DeallocateTopologyTypePerGrid()
        call this%DeallocateGeometryTypePerGrid()
        call this%DeallocateGeometryTypePerGrid()
        call this%NullifyMPIEnvironment()
    end subroutine str_spatial_grid_descriptor_Free



end module structured_spatial_grid_descriptor
