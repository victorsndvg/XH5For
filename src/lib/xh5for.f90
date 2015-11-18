module xh5for

use mpi_environment
use xh5for_utils
use xh5for_parameters
use uniform_grid_descriptor
use spatial_grid_descriptor
use xdmf_handler
use hdf5_handler
use unstructured_contiguous_hyperslab_factory
use IR_Precision, only: I4P, I8P, str


implicit none

    type :: xh5for_t
    private
        integer(I4P)                                  :: Strategy = XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB
        integer(I4P)                                  :: GridType = XDMF_GRID_TYPE_UNSTRUCTURED
        integer(I4P)                                  :: Action   = XDMF_ACTION_WRITE
        type(mpi_env_t)                               :: MPIEnvironment
        class(uniform_grid_descriptor_t), allocatable :: UniformGridDescriptor
        class(spatial_grid_descriptor_t), allocatable :: SpatialGridDescriptor
        class(xdmf_handler_t),            allocatable :: LightData
        class(hdf5_handler_t),            allocatable :: HeavyData
    contains
    private
        procedure         :: xh5for_Initialize_Unstructured_Reader
        procedure         :: xh5for_Initialize_Unstructured_Writer_I4P
        procedure         :: xh5for_Initialize_Unstructured_Writer_I8P
        procedure         :: xh5for_Initialize_Structured_Writer_I4P
        procedure         :: xh5for_Initialize_Structured_Writer_I8P
        procedure         :: xh5for_WriteGeometry_XYZ_R4P
        procedure         :: xh5for_WriteGeometry_XYZ_R8P
        procedure         :: xh5for_WriteGeometry_X_Y_Z_R4P
        procedure         :: xh5for_WriteGeometry_X_Y_Z_R8P
        procedure         :: xh5for_ReadGeometry_XYZ_R4P
        procedure         :: xh5for_ReadGeometry_XYZ_R8P
        procedure         :: xh5for_ReadGeometry_X_Y_Z_R4P
        procedure         :: xh5for_ReadGeometry_X_Y_Z_R8P
        procedure         :: xh5for_WriteTopology_I4P
        procedure         :: xh5for_WriteTopology_I8P
        procedure         :: xh5for_ReadTopology_I4P
        procedure         :: xh5for_ReadTopology_I8P
        procedure         :: xh5for_WriteAttribute_I4P
        procedure         :: xh5for_WriteAttribute_I8P
        procedure         :: xh5for_WriteAttribute_R4P
        procedure         :: xh5for_WriteAttribute_R8P
        procedure         :: xh5for_ReadAttribute_I4P
        procedure         :: xh5for_ReadAttribute_I8P
        procedure         :: xh5for_ReadAttribute_R4P
        procedure         :: xh5for_ReadAttribute_R8P
        procedure, public :: SetStrategy           => xh5for_SetStrategy
        procedure, public :: SetGridType           => xh5for_SetGridType
        generic,   public :: Initialize            => xh5for_Initialize_Unstructured_Writer_I4P, &
                                                      xh5for_Initialize_Unstructured_Writer_I8P, &
                                                      xh5for_Initialize_Structured_Writer_I4P,   &
                                                      xh5for_Initialize_Structured_Writer_I8P,   &
                                                      xh5for_Initialize_Unstructured_Reader
        procedure, public :: Free                  => xh5for_Free
        procedure, public :: Open                  => xh5for_Open
        procedure, public :: Parse                 => xh5for_Parse
        procedure, public :: Close                 => xh5for_Close
        generic,   public :: WriteTopology         => xh5for_WriteTopology_I4P, &
                                                      xh5for_WriteTopology_I8P
        generic,   public :: ReadTopology          => xh5for_ReadTopology_I4P, &
                                                      xh5for_ReadTopology_I8P
        generic,   public :: WriteGeometry         => xh5for_WriteGeometry_XYZ_R4P,   &
                                                      xh5for_WriteGeometry_XYZ_R8P,   &
                                                      xh5for_WriteGeometry_X_Y_Z_R4P, &
                                                      xh5for_WriteGeometry_X_Y_Z_R8P
        generic,   public :: ReadGeometry          => xh5for_ReadGeometry_XYZ_R4P,   &
                                                      xh5for_ReadGeometry_XYZ_R8P,   &
                                                      xh5for_ReadGeometry_X_Y_Z_R4P, &
                                                      xh5for_ReadGeometry_X_Y_Z_R8P
        generic,   public :: WriteAttribute        => xh5for_WriteAttribute_I4P, &
                                                      xh5for_WriteAttribute_I8P, &
                                                      xh5for_WriteAttribute_R4P, &
                                                      xh5for_WriteAttribute_R8P
        generic,   public :: ReadAttribute         => xh5for_ReadAttribute_I4P, &
                                                      xh5for_ReadAttribute_I8P, &
                                                      xh5for_ReadAttribute_R4P, &
                                                      xh5for_ReadAttribute_R8P
    end type xh5for_t


contains

    subroutine xh5for_SetStrategy(this, Strategy)
    !----------------------------------------------------------------- 
    !< Set the strategy of data handling
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT)  :: this
        integer(I4P),    intent(IN)     :: Strategy
    !----------------------------------------------------------------- 
        if(isSupportedStrategy(Strategy)) this%Strategy = Strategy
    end subroutine xh5for_SetStrategy


    subroutine xh5for_SetGridType(this, GridType)
    !----------------------------------------------------------------- 
    !< Set the strategy of data handling
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT)  :: this
        integer(I4P),    intent(IN)     :: GridType
    !----------------------------------------------------------------- 
        if(isSupportedGridType(GridType)) this%GridType = GridType
    end subroutine xh5for_SetGridType


    subroutine xh5for_Free(this)
    !----------------------------------------------------------------- 
    !< Free XH5For derived type
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this
    !----------------------------------------------------------------- 
        this%Strategy = XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB
        call this%MPIEnvironment%Free()
        if(allocated(this%UniformGridDescriptor)) then
            call this%UniformGridDescriptor%Free()
            deallocate(this%UniformGridDescriptor)
        endif
        if(allocated(this%SpatialGridDescriptor)) then
            call this%SpatialGridDescriptor%Free()
            deallocate(this%SpatialGridDescriptor)
        endif
        if(allocated(this%LightData)) then
            call this%LightData%Free()
            deallocate(this%LightData)
        endif
        if(allocated(this%HeavyData)) then
            call this%HeavyData%Free()
            deallocate(this%HeavyData)
        endif
    end subroutine xh5for_Free


    subroutine xh5for_Initialize_Unstructured_Reader(this, comm, root)
    !----------------------------------------------------------------- 
    !< Apply strategy and initialize lightdata and heavydata handlers
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer, optional, intent(IN)     :: comm                     !< MPI communicator
        integer, optional, intent(IN)     :: root                     !< MPI root procesor
        integer                           :: error                    !< Error variable
        integer                           :: r_root = 0               !< Real MPI root procesor
    !----------------------------------------------------------------- 
        this%Action = XDMF_ACTION_READ
        if(present(root)) r_root = root
        call this%Free()
        ! MPI environment initialization
        if(present(comm)) then
            call This%MPIEnvironment%Initialize(comm = comm, root = r_root, mpierror = error)
        else
            call This%MPIEnvironment%Initialize(root = r_root, mpierror = error)
        endif
        ! Build components from factory
        call TheUnstructuredContiguousHyperslabFactory%CreateUniformGridDescriptor(this%UniformGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateSpatialGridDescriptor(this%SpatialGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateXDMFHandler(this%LightData)
        call TheUnstructuredContiguousHyperslabFactory%CreateHDF5Handler(this%HeavyData)
        call this%SpatialGridDescriptor%Initialize(MPIEnvironment = this%MPIEnvironment)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
    end subroutine xh5for_Initialize_Unstructured_Reader


    subroutine xh5for_Initialize_Unstructured_Writer_I4P(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType, comm, root)
    !----------------------------------------------------------------- 
    !< Apply strategy and initialize lightdata and heavydata handlers
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer(I4P),      intent(IN)     :: NumberOfNodes            !< Number of nodes of the current grid (I4P)
        integer(I4P),      intent(IN)     :: NumberOfElements         !< Number of elements of the current grid (I4P)
        integer(I4P),      intent(IN)     :: TopologyType             !< Topology type of the current grid
        integer(I4P),      intent(IN)     :: GeometryType             !< Geometry type of the current grid
        integer, optional, intent(IN)     :: comm                     !< MPI communicator
        integer, optional, intent(IN)     :: root                     !< MPI root procesor
        integer                           :: error                    !< Error variable
        integer                           :: r_root = 0               !< Real MPI root procesor
    !----------------------------------------------------------------- 
        this%Action = XDMF_ACTION_WRITE
        if(present(root)) r_root = root
        call this%Free()
        ! MPI environment initialization
        if(present(comm)) then
            call This%MPIEnvironment%Initialize(comm = comm, root = r_root, mpierror = error)
        else
            call This%MPIEnvironment%Initialize(root = r_root, mpierror = error)
        endif
        ! Build components from factory
        call TheUnstructuredContiguousHyperslabFactory%CreateUniformGridDescriptor(this%UniformGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateSpatialGridDescriptor(this%SpatialGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateXDMFHandler(this%LightData)
        call TheUnstructuredContiguousHyperslabFactory%CreateHDF5Handler(this%HeavyData)
        call this%SpatialGridDescriptor%Initialize(MPIEnvironment = this%MPIEnvironment)
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize(           &
                NumberOfNodes    = int(NumberOfNodes,I8P),    &
                NumberOfElements = int(NumberOfElements,I8P), &
                TopologyType     = TopologyType,              &
                GeometryType     = GeometryType,              &
                GridType         = this%GridType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(            &
                MPIEnvironment   = this%MPIEnvironment,        &
                NumberOfNodes    = int(NumberOfNodes,I8P),     &
                NumberOfElements = int(NumberOfElements,I8P),  &
                TopologyType     = TopologyType,               &
                GeometryType     = GeometryType)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
    end subroutine xh5for_Initialize_Unstructured_Writer_I4P


    subroutine xh5for_Initialize_Unstructured_Writer_I8P(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType, comm, root)
    !----------------------------------------------------------------- 
    !< Apply strategy and initialize lightdata and heavydata handlers
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer(I8P),      intent(IN)     :: NumberOfNodes            !< Number of nodes of the current grid (I4P)
        integer(I8P),      intent(IN)     :: NumberOfElements         !< Number of elements of the current grid (I4P)
        integer(I4P),      intent(IN)     :: TopologyType             !< Topology type of the current grid
        integer(I4P),      intent(IN)     :: GeometryType             !< Geometry type of the current grid
        integer, optional, intent(IN)     :: comm                     !< MPI communicator
        integer, optional, intent(IN)     :: root                     !< MPI root procesor
        integer                           :: error                    !< Error variable
        integer                           :: r_root = 0               !< Real MPI root procesor
    !----------------------------------------------------------------- 
        this%Action = XDMF_ACTION_WRITE
        if(present(root)) r_root = root
        call this%Free()
        ! MPI environment initialization
        if(present(comm)) then
            call This%MPIEnvironment%Initialize(comm = comm, root = r_root, mpierror = error)
        else
            call This%MPIEnvironment%Initialize(root = r_root, mpierror = error)
        endif
        ! Build components from factory
        call TheUnstructuredContiguousHyperslabFactory%CreateUniformGridDescriptor(this%UniformGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateSpatialGridDescriptor(this%SpatialGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateXDMFHandler(this%LightData)
        call TheUnstructuredContiguousHyperslabFactory%CreateHDF5Handler(this%HeavyData)
        call this%SpatialGridDescriptor%Initialize(MPIEnvironment = this%MPIEnvironment)
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize(           &
                NumberOfNodes    = int(NumberOfNodes,I8P),    &
                NumberOfElements = int(NumberOfElements,I8P), &
                TopologyType     = TopologyType,              &
                GeometryType     = GeometryType,              &
                GridType         = this%GridType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(           &
                MPIEnvironment   = this%MPIEnvironment,       &
                NumberOfNodes    = int(NumberOfNodes,I8P),    &
                NumberOfElements = int(NumberOfElements,I8P), &
                TopologyType     = TopologyType,              &
                GeometryType     = GeometryType)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
    end subroutine xh5for_Initialize_Unstructured_Writer_I8P


    subroutine xh5for_Initialize_Structured_Writer_I4P(this, GridShape, comm, root)
    !----------------------------------------------------------------- 
    !< Apply strategy and initialize lightdata and heavydata handlers
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer(I4P),      intent(IN)     :: GridShape(3)             !< Shape of the grid
        integer, optional, intent(IN)     :: comm                     !< MPI communicator
        integer, optional, intent(IN)     :: root                     !< MPI root procesor
        integer(I8P)                      :: NumberOfNodes            !< Number of nodes of the current grid (I4P)
        integer(I8P)                      :: NumberOfElements         !< Number of elements of the current grid (I4P)
        integer(I4P)                      :: TopologyType             !< Topology type of the current grid
        integer(I4P)                      :: GeometryType             !< Geometry type of the current grid
        integer                           :: error                    !< Error variable
        integer                           :: r_root = 0               !< Real MPI root procesor
    !----------------------------------------------------------------- 
        this%Action = XDMF_ACTION_WRITE
        if(present(root)) r_root = root
        call this%Free()
        ! MPI environment initialization
        if(present(comm)) then
            call This%MPIEnvironment%Initialize(comm = comm, root = r_root, mpierror = error)
        else
            call This%MPIEnvironment%Initialize(root = r_root, mpierror = error)
        endif
        ! Build components from factory
        call TheUnstructuredContiguousHyperslabFactory%CreateUniformGridDescriptor(this%UniformGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateSpatialGridDescriptor(this%SpatialGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateXDMFHandler(this%LightData)
        call TheUnstructuredContiguousHyperslabFactory%CreateHDF5Handler(this%HeavyData)
        call this%SpatialGridDescriptor%Initialize(MPIEnvironment = this%MPIEnvironment)
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize(                   &
                                    XDim     = int(GridShape(1),I8P), &
                                    YDim     = int(GridShape(2),I8P), &
                                    ZDim     = int(GridShape(3),I8P), &
                                    GridType = this%GridType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(            &
                MPIEnvironment   = this%MPIEnvironment,        &
                NumberOfNodes    = int(NumberOfNodes,I8P),     &
                NumberOfElements = int(NumberOfElements,I8P),  &
                TopologyType     = TopologyType,               &
                GeometryType     = GeometryType)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
    end subroutine xh5for_Initialize_Structured_Writer_I4P


    subroutine xh5for_Initialize_Structured_Writer_I8P(this, GridShape, comm, root)
    !----------------------------------------------------------------- 
    !< Apply strategy and initialize lightdata and heavydata handlers
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer(I8P),      intent(IN)     :: GridShape(3)             !< GridShape
        integer, optional, intent(IN)     :: comm                     !< MPI communicator
        integer, optional, intent(IN)     :: root                     !< MPI root procesor
        integer(I8P)                      :: NumberOfNodes            !< Number of nodes of the current grid (I4P)
        integer(I8P)                      :: NumberOfElements         !< Number of elements of the current grid (I4P)
        integer(I4P)                      :: TopologyType             !< Topology type of the current grid
        integer(I4P)                      :: GeometryType             !< Geometry type of the current grid
        integer                           :: error                    !< Error variable
        integer                           :: r_root = 0               !< Real MPI root procesor
    !----------------------------------------------------------------- 
        this%Action = XDMF_ACTION_WRITE
        if(present(root)) r_root = root
        call this%Free()
        ! MPI environment initialization
        if(present(comm)) then
            call This%MPIEnvironment%Initialize(comm = comm, root = r_root, mpierror = error)
        else
            call This%MPIEnvironment%Initialize(root = r_root, mpierror = error)
        endif
        ! Build components from factory
        call TheUnstructuredContiguousHyperslabFactory%CreateUniformGridDescriptor(this%UniformGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateSpatialGridDescriptor(this%SpatialGridDescriptor)
        call TheUnstructuredContiguousHyperslabFactory%CreateXDMFHandler(this%LightData)
        call TheUnstructuredContiguousHyperslabFactory%CreateHDF5Handler(this%HeavyData)
        call this%SpatialGridDescriptor%Initialize(MPIEnvironment = this%MPIEnvironment)
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize( &
                XDim = GridShape(1),                &
                YDim = GridShape(2),                &
                ZDim = GridShape(3),                &
                GridType = this%GridType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(            &
                MPIEnvironment   = this%MPIEnvironment,        &
                NumberOfNodes    = int(NumberOfNodes,I8P),     &
                NumberOfElements = int(NumberOfElements,I8P),  &
                TopologyType     = TopologyType,               &
                GeometryType     = GeometryType)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
    end subroutine xh5for_Initialize_Structured_Writer_I8P


    subroutine xh5for_Open(this, action, fileprefix)
    !-----------------------------------------------------------------
    !< Open a XDMF and HDF5 files
    !----------------------------------------------------------------- 
        class(xh5for_t),        intent(INOUT) :: this                 !< XH5For derived type
        character(len=*),       intent(IN)    :: fileprefix           !< XDMF filename prefix
        integer(I4P), optional, intent(IN)    :: action               !< XDMF Open file action (Read or Write)
    !-----------------------------------------------------------------
        if(present(action)) this%action = action
        call this%HeavyData%OpenFile(action=this%action, fileprefix=fileprefix)
        call this%LightData%OpenFile(action=this%action, fileprefix=fileprefix)
    end subroutine xh5for_Open


    subroutine xh5for_Parse(this)
    !-----------------------------------------------------------------
    !< Open a XDMF and HDF5 files
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
    !-----------------------------------------------------------------
        if(this%Action == XDMF_ACTION_READ) call this%LightData%ParseFile()
    end subroutine xh5for_Parse


    subroutine xh5for_Close(this)
    !-----------------------------------------------------------------
    !< Open a XDMF and HDF5 files
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
    !-----------------------------------------------------------------
        call this%HeavyData%CloseFile()
        if(this%action == XDMF_ACTION_WRITE) then
            !< XDMF deferred writing when hdf5 closes    
            call this%LightData%Serialize()
            call this%LightData%CloseFile()
        endif
    end subroutine xh5for_Close


    subroutine xh5for_WriteGeometry_XYZ_R4P(this, XYZ, Name)
    !----------------------------------------------------------------- 
    !< Write R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P),                  intent(IN)    :: XYZ(:)           !< R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = XYZ, Name = Name)
            call this%HeavyData%WriteGeometry(XYZ = XYZ, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = XYZ, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(XYZ = XYZ, Name = 'Coordinates')
        endif
    end subroutine xh5for_WriteGeometry_XYZ_R4P


    subroutine xh5for_WriteGeometry_XYZ_R8P(this, XYZ, Name)
    !----------------------------------------------------------------- 
    !< Write R8P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type                        
        real(R8P),                  intent(IN)    :: XYZ(:)           !< R8P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = XYZ, Name = Name)
            call this%HeavyData%WriteGeometry(XYZ = XYZ, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = XYZ, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(XYZ = XYZ, Name = 'Coordinates')
        endif
    end subroutine xh5for_WriteGeometry_XYZ_R8P


    subroutine xh5for_WriteGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !----------------------------------------------------------------- 
    !< Write R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P),                  intent(IN)    :: X(:)             !< X R4P grid geometry coordinates
        real(R4P),                  intent(IN)    :: Y(:)             !< Y R4P grid geometry coordinates
        real(R4P),                  intent(IN)    :: Z(:)             !< Z R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = X, Name = Name)
            call this%HeavyData%WriteGeometry(X = X, Y = Y, Z = Z, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = X, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(X = X, Y = Y, Z = Z, Name = 'Coordinates')
        endif
    end subroutine xh5for_WriteGeometry_X_Y_Z_R4P


    subroutine xh5for_WriteGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !----------------------------------------------------------------- 
    !< Write R8P X_Y_Z Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R8P),                  intent(IN)    :: X(:)             !< X R4P grid geometry coordinates
        real(R8P),                  intent(IN)    :: Y(:)             !< Y R4P grid geometry coordinates
        real(R8P),                  intent(IN)    :: Z(:)             !< Z R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = X, Name = Name)
            call this%HeavyData%WriteGeometry(X = X, Y = Y, Z = Z, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = X, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(X = X, Y = Y, Z = Z, Name = 'Coordinates')
        endif
    end subroutine xh5for_WriteGeometry_X_Y_Z_R8P


    subroutine xh5for_ReadGeometry_XYZ_R4P(this, XYZ, Name)
    !----------------------------------------------------------------- 
    !< Read XY[Z] R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P), allocatable,     intent(OUT)   :: XYZ(:)           !< R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(XYZ = XYZ, Name = Name)
            call this%LightData%SetGeometry(XYZ = XYZ, Name = Name)
        else
            call this%HeavyData%ReadGeometry(XYZ = XYZ, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = XYZ, Name = 'Coordinates')
        endif
    end subroutine xh5for_ReadGeometry_XYZ_R4P


    subroutine xh5for_ReadGeometry_XYZ_R8P(this, XYZ, Name)
    !----------------------------------------------------------------- 
    !< Read XY[Z] R8P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R8P), allocatable,     intent(OUT)   :: XYZ(:)   !< R8P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(XYZ = XYZ, Name = Name)
            call this%LightData%SetGeometry(XYZ = XYZ, Name = Name)
        else
            call this%HeavyData%ReadGeometry(XYZ = XYZ, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = XYZ, Name = 'Coordinates')
        endif
    end subroutine xh5for_ReadGeometry_XYZ_R8P


    subroutine xh5for_ReadGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !----------------------------------------------------------------- 
    !< Read X_Y_Z R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P), allocatable,     intent(OUT)   :: X(:)             !< X R4P grid geometry coordinates
        real(R4P), allocatable,     intent(OUT)   :: Y(:)             !< Y R4P grid geometry coordinates
        real(R4P), allocatable,     intent(OUT)   :: Z(:)             !< Z R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(X = X, Y = Y, Z = Z, Name = Name)
            call this%LightData%SetGeometry(XYZ = X, Name = Name)
        else
            call this%HeavyData%ReadGeometry(X = X, Y = Y, Z = Z, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = X, Name = 'Coordinates')
        endif
    end subroutine xh5for_ReadGeometry_X_Y_Z_R4P


    subroutine xh5for_ReadGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !----------------------------------------------------------------- 
    !< Read X_Y_Z R8P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R8P), allocatable,     intent(OUT)   :: X(:)             !< X R8P grid geometry coordinates
        real(R8P), allocatable,     intent(OUT)   :: Y(:)             !< Y R8P grid geometry coordinates
        real(R8P), allocatable,     intent(OUT)   :: Z(:)             !< Z R8P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(X = X, Y = Y, Z = Z, Name = Name)
            call this%LightData%SetGeometry(XYZ = X, Name = Name)
        else
            call this%HeavyData%ReadGeometry(X = X, Y = Y, Z = Z, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = X, Name = 'Coordinates')
        endif
    end subroutine xh5for_ReadGeometry_X_Y_Z_R8P


    subroutine xh5for_WriteTopology_I4P(this, Connectivities, Name)
    !----------------------------------------------------------------- 
    !< Write I4P Topology
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this              !< XH5For derived type
        integer(I4P),               intent(IN)    :: Connectivities(:) !< I4P grid topology connectivities
        character(len=*), optional, intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
            call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = 'Connectivities')
            call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = 'Connectivities')
        endif
    end subroutine xh5for_WriteTopology_I4P


    subroutine xh5for_WriteTopology_I8P(this, Connectivities, Name)
    !----------------------------------------------------------------- 
    !< Write I8P Topology
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this              !< XH5For derived type
        integer(I8P),               intent(IN)    :: Connectivities(:) !< I8P grid topology connectivities
        character(len=*), optional, intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
            call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = 'Connectivities')
            call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = 'Connectivities')
        endif
    end subroutine xh5for_WriteTopology_I8P


    subroutine xh5for_ReadTopology_I4P(this, Connectivities, Name)
    !----------------------------------------------------------------- 
    !< Read I4P Topology
    !----------------------------------------------------------------- 
        class(xh5for_t),           intent(INOUT) :: this              !< XH5For derived type
        integer(I4P), allocatable, intent(OUT)   :: Connectivities(:) !< I4P grid topology connectivities
        character(len=*),optional, intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = Name)
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = 'Connectivities')
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = 'Connectivities')
        endif
    end subroutine xh5for_ReadTopology_I4P


    subroutine xh5for_ReadTopology_I8P(this, Connectivities, Name)
    !----------------------------------------------------------------- 
    !< Read I8P Topology
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this              !< XH5For derived type
        integer(I8P), allocatable,  intent(OUT)   :: Connectivities(:) !< I8P grid topology connectivities
        character(len=*), optional, intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = Name)
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = 'Connectivities')
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = 'Connectivities')
        endif
    end subroutine xh5for_ReadTopology_I8P


    subroutine xh5for_WriteAttribute_I4P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Write I4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
        character(len=*),intent(IN)    :: Name                        !< Attribute name
        integer(I4P),    intent(IN)    :: Type                        !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),    intent(IN)    :: Center                      !< Attribute centered at (Node, Cell, etc.)
        integer(I4P),    intent(IN)    :: Values(:)                   !< I4P grid attribute values
    !-----------------------------------------------------------------
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_WriteAttribute_I4P


    subroutine xh5for_WriteAttribute_I8P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Write I4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
        character(len=*),intent(IN)    :: Name                        !< Attribute name
        integer(I4P),    intent(IN)    :: Type                        !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),    intent(IN)    :: Center                      !< Attribute centered at (Node, Cell, etc.)
        integer(I8P),    intent(IN)    :: Values(:)                   !< I8P grid attribute values
    !-----------------------------------------------------------------
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_WriteAttribute_I8P


    subroutine xh5for_WriteAttribute_R4P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Write I4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
        character(len=*),intent(IN)    :: Name                        !< Attribute name
        integer(I4P),    intent(IN)    :: Type                        !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),    intent(IN)    :: Center                      !< Attribute centered at (Node, Cell, etc.)
        real(R4P),       intent(IN)    :: Values(:)                   !< R4P grid attribute values
    !-----------------------------------------------------------------
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_WriteAttribute_R4P


    subroutine xh5for_WriteAttribute_R8P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Write I4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
        character(len=*),intent(IN)    :: Name                        !< Attribute name
        integer(I4P),    intent(IN)    :: Type                        !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),    intent(IN)    :: Center                      !< Attribute centered at (Node, Cell, etc.)
        real(R8P),       intent(IN)    :: Values(:)                   !< R8P grid attribute values
    !-----------------------------------------------------------------
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_WriteAttribute_R8P


    subroutine xh5for_ReadAttribute_I4P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Read I4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t),           intent(INOUT) :: this              !< XH5For derived type
        character(len=*),          intent(IN)    :: Name              !< Attribute name
        integer(I4P),              intent(IN)    :: Type              !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),              intent(IN)    :: Center            !< Attribute centered at (Node, Cell, etc.)
        integer(I4P), allocatable, intent(OUT)   :: Values(:)         !< I4P grid attribute values
    !-----------------------------------------------------------------
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_ReadAttribute_I4P


    subroutine xh5for_ReadAttribute_I8P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Read I8P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t),           intent(INOUT) :: this              !< XH5For derived type
        character(len=*),          intent(IN)    :: Name              !< Attribute name
        integer(I4P),              intent(IN)    :: Type              !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),              intent(IN)    :: Center            !< Attribute centered at (Node, Cell, etc.)
        integer(I8P), allocatable, intent(OUT)   :: Values(:)         !< I8P grid attribute values
    !-----------------------------------------------------------------
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_ReadAttribute_I8P


    subroutine xh5for_ReadAttribute_R4P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Read R4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t),        intent(INOUT) :: this                 !< XH5For derived type
        character(len=*),       intent(IN)    :: Name                 !< Attribute name
        integer(I4P),           intent(IN)    :: Type                 !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),           intent(IN)    :: Center               !< Attribute centered at (Node, Cell, etc.)
        real(R4P), allocatable, intent(OUT)   :: Values(:)            !< R4P grid attribute values
    !-----------------------------------------------------------------
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_ReadAttribute_R4P


    subroutine xh5for_ReadAttribute_R8P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Read R8P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t),        intent(INOUT) :: this                 !< XH5For derived type
        character(len=*),       intent(IN)    :: Name                 !< Attribute name
        integer(I4P),           intent(IN)    :: Type                 !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),           intent(IN)    :: Center               !< Attribute centered at (Node, Cell, etc.)
        real(R8P), allocatable, intent(OUT)   :: Values(:)            !< R8P grid attribute values
    !-----------------------------------------------------------------
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_ReadAttribute_R8P

end module xh5for
