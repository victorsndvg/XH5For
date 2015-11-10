module xh5for

use xh5for_handler
use mpi_environment
use xh5for_utils
use xh5for_parameters
use xh5for_handler_factory
use uniform_grid_descriptor
use spatial_grid_descriptor
use IR_Precision, only: I4P, I8P, str


implicit none

    type :: xh5for_t
    private
        integer(I4P)                         :: Strategy = XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB
        integer(I4P)                         :: Action   = XDMF_ACTION_WRITE
        type(mpi_env_t)                      :: MPIEnvironment
        type(uniform_grid_descriptor_t)      :: UniformGridDescriptor
        type(spatial_grid_descriptor_t)      :: SpatialGridDescriptor
        class(xh5for_handler_t), allocatable :: Handler
    contains
    private
        procedure         :: xh5for_Initialize_Reader
        procedure         :: xh5for_Initialize_Writer_I4P
        procedure         :: xh5for_Initialize_Writer_I8P
        procedure         :: xh5for_WriteGeometry_R4P
        procedure         :: xh5for_WriteGeometry_R8P
        procedure         :: xh5for_ReadGeometry_R4P
        procedure         :: xh5for_ReadGeometry_R8P
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
        generic,   public :: Initialize            => xh5for_Initialize_Writer_I4P, &
                                                      xh5for_Initialize_Writer_I8P, &
                                                      xh5for_Initialize_Reader
        procedure, public :: Free                  => xh5for_Free
        procedure, public :: Open                  => xh5for_Open
        procedure, public :: Parse                 => xh5for_Parse
        procedure, public :: Close                 => xh5for_Close
        generic,   public :: WriteTopology         => xh5for_WriteTopology_I4P, &
                                                      xh5for_WriteTopology_I8P
        generic,   public :: ReadTopology          => xh5for_ReadTopology_I4P, &
                                                      xh5for_ReadTopology_I8P
        generic,   public :: WriteGeometry         => xh5for_WriteGeometry_R4P, &
                                                      xh5for_WriteGeometry_R8P
        generic,   public :: ReadGeometry          => xh5for_ReadGeometry_R4P, &
                                                      xh5for_ReadGeometry_R8P
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


    subroutine xh5for_Free(this)
    !----------------------------------------------------------------- 
    !< Free XH5For derived type
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this
    !----------------------------------------------------------------- 
        if(allocated(this%Handler)) call this%Handler%Free()
        this%Strategy = XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB
        call this%MPIEnvironment%Free()
        call this%UniformGridDescriptor%Free()
        call this%SpatialGridDescriptor%Free()
    end subroutine xh5for_Free


    subroutine xh5for_Initialize_Reader(this, comm, root)
    !----------------------------------------------------------------- 
    !< Apply strategy and initialize lightdata and heavydata handlers
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer, optional, intent(IN)     :: comm                     !< MPI communicator
        integer, optional, intent(IN)     :: root                     !< MPI root procesor
        type(xh5for_handler_factory_t)    :: XH5ForHandlerFactory     !< Handler factory to get the concrete strategy implementation
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
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(MPIEnvironment = this%MPIEnvironment)
        ! Get the concrete handler
        call XH5ForHandlerFactory%GetHandler(Strategy=this%Strategy, Handler=this%Handler)
        ! XH5For handler initialization
        call this%Handler%Initialize(                             &
                MPIEnvironment=this%MPIEnvironment,               &
                SpatialGridDescriptor=this%SpatialGridDescriptor, &
                UniformGridDescriptor=this%UniformGridDescriptor)

    end subroutine xh5for_Initialize_Reader


    subroutine xh5for_Initialize_Writer_I4P(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType, comm, root)
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
        type(xh5for_handler_factory_t)    :: XH5ForHandlerFactory     !< Handler factory to get the concrete strategy implementation
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
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize(           &
                NumberOfNodes = int(NumberOfNodes,I8P),       &
                NumberOfElements = int(NumberOfElements,I8P), &
                TopologyType = TopologyType,                  &
                GeometryType = GeometryType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(            &
                MPIEnvironment = this%MPIEnvironment,          &
                NumberOfNodes = int(NumberOfNodes,I8P),        &
                NumberOfElements = int(NumberOfElements,I8P),  &
                TopologyType = TopologyType,                   &
                GeometryType = GeometryType)
        ! Get the concrete handler
        call XH5ForHandlerFactory%GetHandler(Strategy=this%Strategy, Handler=this%Handler)
        ! XH5For handler initialization
        call this%Handler%Initialize(                             &
                MPIEnvironment=this%MPIEnvironment,               &
                SpatialGridDescriptor=this%SpatialGridDescriptor, &
                UniformGridDescriptor=this%UniformGridDescriptor)

    end subroutine xh5for_Initialize_Writer_I4P


    subroutine xh5for_Initialize_Writer_I8P(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType, comm, root)
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
        type(xh5for_handler_factory_t)    :: XH5ForHandlerFactory     !< Handler factory to get the concrete strategy implementation
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
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize(  &
                NumberOfNodes = NumberOfNodes,       &
                NumberOfElements = NumberOfElements, &
                TopologyType = TopologyType,         &
                GeometryType = GeometryType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(&
                MPIEnvironment = this%MPIEnvironment, &
                NumberOfNodes = NumberOfNodes,        &
                NumberOfElements = NumberOfElements,  &
                TopologyType = TopologyType,          &
                GeometryType = GeometryType)
        ! Get the concrete handler
        call XH5ForHandlerFactory%GetHandler(Strategy=this%Strategy, Handler=this%Handler)
        ! XH5For handler initialization
        call this%Handler%Initialize(                              &
                MPIEnvironment=this%MPIEnvironment,               &
                SpatialGridDescriptor=this%SpatialGridDescriptor, &
                UniformGridDescriptor=this%UniformGridDescriptor)
    end subroutine xh5for_Initialize_Writer_I8P


    subroutine xh5for_Open(this, action, fileprefix)
    !-----------------------------------------------------------------
    !< Open a XDMF and HDF5 files
    !----------------------------------------------------------------- 
        class(xh5for_t),        intent(INOUT) :: this                 !< XH5For derived type
        character(len=*),       intent(IN)    :: fileprefix           !< XDMF filename prefix
        integer(I4P), optional, intent(IN)    :: action               !< XDMF Open file action (Read or Write)
    !-----------------------------------------------------------------
        if(present(action)) this%action = action
        call this%Handler%Open(action=this%action, fileprefix=fileprefix)
    end subroutine xh5for_Open


    subroutine xh5for_Parse(this)
    !-----------------------------------------------------------------
    !< Open a XDMF and HDF5 files
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
    !-----------------------------------------------------------------
        if(this%Action == XDMF_ACTION_READ) call this%Handler%Parse()
    end subroutine xh5for_Parse


    subroutine xh5for_Close(this)
    !-----------------------------------------------------------------
    !< Open a XDMF and HDF5 files
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
    !-----------------------------------------------------------------
        call this%Handler%Close(action = this%action)
    end subroutine xh5for_Close


    subroutine xh5for_WriteGeometry_R4P(this, Coordinates, Name)
    !----------------------------------------------------------------- 
    !< Write R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P),                  intent(IN)    :: Coordinates(:)   !< R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%Handler%WriteGeometry(Coordinates = Coordinates, Name = Name)
        else
            call this%Handler%WriteGeometry(Coordinates = Coordinates, Name = 'Coordinates')
        endif
    end subroutine xh5for_WriteGeometry_R4P


    subroutine xh5for_WriteGeometry_R8P(this, Coordinates, Name)
    !----------------------------------------------------------------- 
    !< Write R8P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type                        
        real(R8P),                  intent(IN)    :: Coordinates(:)   !< R8P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%Handler%WriteGeometry(Coordinates = Coordinates, Name = Name)
        else
            call this%Handler%WriteGeometry(Coordinates = Coordinates, Name = 'Coordinates')
        endif
    end subroutine xh5for_WriteGeometry_R8P


    subroutine xh5for_ReadGeometry_R4P(this, Coordinates, Name)
    !----------------------------------------------------------------- 
    !< Read R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P), allocatable,     intent(OUT)   :: Coordinates(:)   !< R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%Handler%ReadGeometry(Coordinates = Coordinates, Name = Name )
        else
            call this%Handler%ReadGeometry(Coordinates = Coordinates, Name ='Coordinates')
        endif
    end subroutine xh5for_ReadGeometry_R4P


    subroutine xh5for_ReadGeometry_R8P(this, Coordinates, Name)
    !----------------------------------------------------------------- 
    !< Read R8P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R8P), allocatable,     intent(OUT)   :: Coordinates(:)   !< R8P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        if(present(Name)) then
            call this%Handler%ReadGeometry(Coordinates = Coordinates, Name = Name )
        else
            call this%Handler%ReadGeometry(Coordinates = Coordinates, Name ='Coordinates')
        endif

    end subroutine xh5for_ReadGeometry_R8P


    subroutine xh5for_WriteTopology_I4P(this, Connectivities, Name)
    !----------------------------------------------------------------- 
    !< Write I4P Topology
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this              !< XH5For derived type
        integer(I4P),               intent(IN)    :: Connectivities(:) !< I4P grid topology connectivities
        character(len=*), optional, intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetConnectivitySize(int(size(connectivities,dim=1),I8P))
        call this%SpatialGridDescriptor%AllgatherConnectivitySize(int(size(connectivities,dim=1),I8P))
        if(present(Name)) then
            call this%Handler%WriteTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%Handler%WriteTopology(Connectivities = Connectivities, Name = 'Connectivities')
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
        call this%UniformGridDescriptor%SetConnectivitySize(int(size(connectivities,dim=1),I8P))
        call this%SpatialGridDescriptor%AllgatherConnectivitySize(int(size(connectivities,dim=1),I8P))
        if(present(Name)) then
            call this%Handler%WriteTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%Handler%WriteTopology(Connectivities = Connectivities, Name = 'Connectivities')
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
            call this%Handler%ReadTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%Handler%ReadTopology(Connectivities = Connectivities, Name = 'Connectivities')
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
            call this%Handler%ReadTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%Handler%ReadTopology(Connectivities = Connectivities, Name = 'Connectivities')
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
        call this%Handler%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
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
        call this%Handler%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
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
        call this%Handler%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
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
        call this%Handler%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
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
        call this%Handler%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
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
        call this%Handler%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
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
        call this%Handler%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
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
        call this%Handler%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_ReadAttribute_R8P

end module xh5for
