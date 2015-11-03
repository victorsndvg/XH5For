module xh5for_contiguous_hyperslab_handler

use xh5for_handler
use xh5for_parameters
use IR_Precision, only : I4P, I8P, R4P, R8P
use xdmf_contiguous_hyperslab_handler
use hdf5_contiguous_hyperslab_handler

implicit none

private

    type, extends(xh5for_handler_t) :: xh5for_contiguous_hyperslab_handler_t
        type(xdmf_contiguous_hyperslab_handler_t) :: LightData
        type(hdf5_contiguous_hyperslab_handler_t) :: HeavyData
    contains
        procedure         :: WriteGeometry_R4P  => xh5for_contiguous_hyperslab_handler_WriteGeometry_R4P
        procedure         :: WriteGeometry_R8P  => xh5for_contiguous_hyperslab_handler_WriteGeometry_R8P
        procedure         :: ReadGeometry_R4P   => xh5for_contiguous_hyperslab_handler_ReadGeometry_R4P
        procedure         :: ReadGeometry_R8P   => xh5for_contiguous_hyperslab_handler_ReadGeometry_R8P
        procedure         :: WriteTopology_I4P  => xh5for_contiguous_hyperslab_handler_WriteTopology_I4P
        procedure         :: WriteTopology_I8P  => xh5for_contiguous_hyperslab_handler_WriteTopology_I8P
        procedure         :: ReadTopology_I4P   => xh5for_contiguous_hyperslab_handler_ReadTopology_I4P
        procedure         :: ReadTopology_I8P   => xh5for_contiguous_hyperslab_handler_ReadTopology_I8P
        procedure         :: WriteAttribute_I4P => xh5for_contiguous_hyperslab_handler_WriteAttribute_I4P
        procedure         :: WriteAttribute_I8P => xh5for_contiguous_hyperslab_handler_WriteAttribute_I8P
        procedure         :: WriteAttribute_R4P => xh5for_contiguous_hyperslab_handler_WriteAttribute_R4P
        procedure         :: WriteAttribute_R8P => xh5for_contiguous_hyperslab_handler_WriteAttribute_R8P
        procedure         :: ReadAttribute_I4P => xh5for_contiguous_hyperslab_handler_ReadAttribute_I4P
        procedure         :: ReadAttribute_I8P => xh5for_contiguous_hyperslab_handler_ReadAttribute_I8P
        procedure         :: ReadAttribute_R4P => xh5for_contiguous_hyperslab_handler_ReadAttribute_R4P
        procedure         :: ReadAttribute_R8P => xh5for_contiguous_hyperslab_handler_ReadAttribute_R8P
        procedure, public :: Initialize         => xh5for_contiguous_hyperslab_handler_Initialize
        procedure, public :: Free               => xh5for_contiguous_hyperslab_handler_Free
        procedure, public :: Open               => xh5for_contiguous_hyperslab_handler_Open
        procedure, public :: Parse              => xh5for_contiguous_hyperslab_handler_Parse
        procedure, public :: Close              => xh5for_contiguous_hyperslab_handler_Close
    end type xh5for_contiguous_hyperslab_handler_t

public :: xh5for_contiguous_hyperslab_handler_t

contains

    subroutine xh5for_contiguous_hyperslab_handler_Initialize(this, MPIEnvironment, UniformGridDescriptor, SpatialGridDescriptor)
    !-----------------------------------------------------------------
    !< XH5FOR initialization procedure
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this                  !< XH5For contiguous hyperslab handler
        type(mpi_env_t),                 target,      intent(IN)    :: MPIEnvironment        !< MPI environment 
        type(uniform_grid_descriptor_t), target,      intent(IN)    :: UniformGridDescriptor !< Uniform grid descriptor
        type(spatial_grid_descriptor_t), target,      intent(IN)    :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        call this%Free()
        ! Light data initialization
        call this%LightData%Initialize(                        &
                MPIEnvironment        = MPIEnvironment,        &
                UniformGridDescriptor = UniformGridDescriptor, &
                SpatialGridDescriptor = SpatialGridDescriptor)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                        &
                MPIEnvironment        = MPIEnvironment,        &
                UniformGridDescriptor = UniformGridDescriptor, &
                SpatialGridDescriptor = SpatialGridDescriptor)
    end subroutine xh5for_contiguous_hyperslab_handler_Initialize


    subroutine xh5for_contiguous_hyperslab_handler_Free(this)
    !-----------------------------------------------------------------
    !< XH5FOR Free procedure
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler
    !----------------------------------------------------------------- 
        call this%LightData%Free()
        call this%HeavyData%Free()
    end subroutine xh5for_contiguous_hyperslab_handler_Free

    subroutine xh5for_contiguous_hyperslab_handler_Open(this, action, fileprefix)
    !-----------------------------------------------------------------
    !< Open the lightdata and the heavydata files
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XH5For contiguous hyperslab handler
        integer(I4P),                                 intent(IN)    :: action       !< XH5For Open action (Read or Write)
        character(len=*),                             intent(IN)    :: fileprefix   !< Filename prefix
    !-----------------------------------------------------------------
        call this%HeavyData%OpenFile(action=action, fileprefix=fileprefix)
        call this%LightData%OpenFile(action=action, fileprefix=fileprefix)
    end subroutine xh5for_contiguous_hyperslab_handler_Open


    subroutine xh5for_contiguous_hyperslab_handler_Parse(this)
    !-----------------------------------------------------------------
    !< Parse the lightdata
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contigous hyperslab handler
    !-----------------------------------------------------------------
        call this%LightData%ParseFile()
    end subroutine xh5for_contiguous_hyperslab_handler_Parse



    subroutine xh5for_contiguous_hyperslab_handler_Close(this, action)
    !-----------------------------------------------------------------
    !< Close the lightdata and the heavydata files
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XH5For contigous hyperslab handler
        integer(I4P),                                 intent(IN)    :: action !< XH5For Close action (Read or Write)
    !-----------------------------------------------------------------
        call this%HeavyData%CloseFile()
        if(action == XDMF_ACTION_WRITE) then
            !< XDMF deferred writing when hdf5 closes    
            call this%LightData%Serialize()
            call this%LightData%CloseFile()
        endif
    end subroutine xh5for_contiguous_hyperslab_handler_Close


    subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_R4P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Write an R4P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XH5For contiguous hyperslab handler
        real(R4P),                                    intent(IN)    :: Coordinates(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name           !< Geometry dataset name
    !-----------------------------------------------------------------
        call this%LightData%SetGeometry(Coordinates = Coordinates, Name = Name)
        call this%HeavyData%WriteGeometry(Coordinates = Coordinates, Name = Name)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_R4P


    subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_R8P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Write an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XH5For contiguous hyperslab handler
        real(R8P),                                    intent(IN)    :: Coordinates(:) !< Grid coordinates
        character(len=*),                             intent(IN)    :: Name           !< Geometry dataset name
    !-----------------------------------------------------------------
        call this%LightData%SetGeometry(Coordinates = Coordinates, Name = Name)
        call this%HeavyData%WriteGeometry(Coordinates = Coordinates, Name = Name)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_R8P


    subroutine xh5for_contiguous_hyperslab_handler_ReadGeometry_R4P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Read an R4P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XH5For contiguous hyperslab handler
        real(R4P), allocatable,                       intent(OUT)   :: Coordinates(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name           !< Geometry dataset name
    !-----------------------------------------------------------------
        call this%HeavyData%ReadGeometry(Coordinates = Coordinates, Name = Name)
        call this%LightData%SetGeometry(Coordinates = Coordinates, Name = Name)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadGeometry_R4P


    subroutine xh5for_contiguous_hyperslab_handler_ReadGeometry_R8P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Read an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XH5For contiguous hyperslab handler
        real(R8P), allocatable,                       intent(OUT)   :: Coordinates(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name           !< Geometry dataset name
    !-----------------------------------------------------------------
        call this%HeavyData%ReadGeometry(Coordinates = Coordinates, Name = Name)
        call this%LightData%SetGeometry(Coordinates = Coordinates, Name = Name)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadGeometry_R8P


    subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Write an I4P Topology for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XH5For contiguous hyperslab handler
        integer(R4P),                                 intent(IN)    :: Connectivities(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
        call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = Name)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_I4P


    subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Write an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XH5For contiguous hyperslab handler
        integer(I8P),                                 intent(IN)    :: Connectivities(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Topology dataset name
    !----------------------------------------------------------------- 
        call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
        call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = Name)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_I8P


    subroutine xh5for_contiguous_hyperslab_handler_ReadTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read an I4P Topology for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XH5For contiguous hyperslab handler
        integer(R4P), allocatable,                    intent(OUT)   :: Connectivities(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = Name)
        call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadTopology_I4P


    subroutine xh5for_contiguous_hyperslab_handler_ReadTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XH5For contiguous hyperslab handler
        integer(I8P), allocatable,                    intent(OUT)   :: Connectivities(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Topology dataset name
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = Name)
        call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadTopology_I8P


    subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_I4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Write an I4P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        integer(I4P),                                 intent(IN)    :: Values(:)  !< I4P Grid attribute
    !----------------------------------------------------------------- 
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_I4P


    subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_I8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Write an I8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        integer(I8P),                                 intent(IN)    :: Values(:)  !< I8P Grid attribute
    !----------------------------------------------------------------- 
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_I8P


    subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_R4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Write an R4P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        real(R4P),                                    intent(IN)    :: Values(:)  !< R4P Grid attribute
    !----------------------------------------------------------------- 
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_R4P


    subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_R8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Write an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        real(R8P),                                    intent(IN)    :: Values(:)  !< R8P Grid attribute
    !----------------------------------------------------------------- 
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_R8P


    subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_I4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Read an I4P attribute for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        integer(I4P), allocatable,                    intent(OUT)   :: Values(:)  !< I4P Grid attribute
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_I4P


    subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_I8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Read an I8P attribute for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        integer(I8P), allocatable,                    intent(OUT)   :: Values(:)  !< I8P Grid attribute
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_I8P


    subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_R4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Read an R4P attribute for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        real(R4P), allocatable,                       intent(OUT)   :: Values(:)  !< R4P Grid attribute
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_R4P


    subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_R8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Read an R8P attribute for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        real(R8P), allocatable,                       intent(OUT)   :: Values(:)  !< R4P Grid attribute
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_R8P

end module xh5for_contiguous_hyperslab_handler
