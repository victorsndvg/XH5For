module xh5for_unstructured_contiguous_hyperslab_handler

use xh5for_handler
use xh5for_parameters
use IR_Precision, only : I4P, I8P, R4P, R8P
use xh5for_contiguous_hyperslab_handler
use xdmf_unstructured_contiguous_hyperslab_handler
use hdf5_unstructured_contiguous_hyperslab_handler

implicit none

private

    type, extends(xh5for_contiguous_hyperslab_handler_t) :: xh5for_unstructured_contiguous_hyperslab_handler_t
    contains
        procedure         :: WriteGeometry_R4P  => xh5for_unstructured_contiguous_hyperslab_WriteGeometry_R4P
        procedure         :: WriteGeometry_R8P  => xh5for_unstructured_contiguous_hyperslab_WriteGeometry_R8P
        procedure         :: ReadGeometry_R4P   => xh5for_unstructured_contiguous_hyperslab_ReadGeometry_R4P
        procedure         :: ReadGeometry_R8P   => xh5for_unstructured_contiguous_hyperslab_ReadGeometry_R8P
        procedure         :: WriteTopology_I4P  => xh5for_unstructured_contiguous_hyperslab_WriteTopology_I4P
        procedure         :: WriteTopology_I8P  => xh5for_unstructured_contiguous_hyperslab_WriteTopology_I8P
        procedure         :: ReadTopology_I4P   => xh5for_unstructured_contiguous_hyperslab_ReadTopology_I4P
        procedure         :: ReadTopology_I8P   => xh5for_unstructured_contiguous_hyperslab_ReadTopology_I8P
        procedure, public :: Initialize         => xh5for_unstructured_contiguous_hyperslab_Initialize
        procedure, public :: Free               => xh5for_unstructured_contiguous_hyperslab_Free
        procedure, public :: Open               => xh5for_unstructured_contiguous_hyperslab_Open
        procedure, public :: Parse              => xh5for_unstructured_contiguous_hyperslab_Parse
        procedure, public :: Close              => xh5for_unstructured_contiguous_hyperslab_Close
    end type xh5for_unstructured_contiguous_hyperslab_handler_t

public :: xh5for_unstructured_contiguous_hyperslab_handler_t

contains

    subroutine xh5for_unstructured_contiguous_hyperslab_Initialize(this, MPIEnvironment, UniformGridDescriptor, SpatialGridDescriptor)
    !-----------------------------------------------------------------
    !< XH5FOR initialization procedure
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t),  intent(INOUT) :: this     !< XH5For contiguous hyperslab handler for Unstructured Grids
        type(mpi_env_t),                  target,      intent(IN)    :: MPIEnvironment        !< MPI environment 
        class(uniform_grid_descriptor_t), target,      intent(IN)    :: UniformGridDescriptor !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), target,      intent(IN)    :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        call this%Free()
        ! Light data initialization
        allocate(xdmf_unstructured_contiguous_hyperslab_handler_t :: this%LightData)
        call this%LightData%Initialize(                        &
                MPIEnvironment        = MPIEnvironment,        &
                UniformGridDescriptor = UniformGridDescriptor, &
                SpatialGridDescriptor = SpatialGridDescriptor)
        ! Heavy data initialization
        allocate(hdf5_unstructured_contiguous_hyperslab_handler_t :: this%HeavyData)
        call this%HeavyData%Initialize(                        &
                MPIEnvironment        = MPIEnvironment,        &
                UniformGridDescriptor = UniformGridDescriptor, &
                SpatialGridDescriptor = SpatialGridDescriptor)
    end subroutine xh5for_unstructured_contiguous_hyperslab_Initialize


    subroutine xh5for_unstructured_contiguous_hyperslab_Free(this)
    !-----------------------------------------------------------------
    !< XH5FOR Free procedure
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
    !----------------------------------------------------------------- 
        if(allocated(this%LightData)) then
            call this%LightData%Free()
            deallocate(this%LightData)
        endif
        if(allocated(this%HeavyData)) then
            call this%HeavyData%Free()
            deallocate(this%HeavyData)
        endif
    end subroutine xh5for_unstructured_contiguous_hyperslab_Free

    subroutine xh5for_unstructured_contiguous_hyperslab_Open(this, action, fileprefix)
    !-----------------------------------------------------------------
    !< Open the lightdata and the heavydata files
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
        integer(I4P),                                 intent(IN)    :: action            !< XH5For Open action (Read or Write)
        character(len=*),                             intent(IN)    :: fileprefix        !< Filename prefix
    !-----------------------------------------------------------------
        call this%HeavyData%OpenFile(action=action, fileprefix=fileprefix)
        call this%LightData%OpenFile(action=action, fileprefix=fileprefix)
    end subroutine xh5for_unstructured_contiguous_hyperslab_Open


    subroutine xh5for_unstructured_contiguous_hyperslab_Parse(this)
    !-----------------------------------------------------------------
    !< Parse the lightdata
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contigous hyperslab handler
    !-----------------------------------------------------------------
        call this%LightData%ParseFile()
    end subroutine xh5for_unstructured_contiguous_hyperslab_Parse



    subroutine xh5for_unstructured_contiguous_hyperslab_Close(this, action)
    !-----------------------------------------------------------------
    !< Close the lightdata and the heavydata files
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contigous hyperslab handler
        integer(I4P),                                 intent(IN)    :: action            !< XH5For Close action (Read or Write)
    !-----------------------------------------------------------------
        call this%HeavyData%CloseFile()
        if(action == XDMF_ACTION_WRITE) then
            !< XDMF deferred writing when hdf5 closes    
            call this%LightData%Serialize()
            call this%LightData%CloseFile()
        endif
    end subroutine xh5for_unstructured_contiguous_hyperslab_Close


    subroutine xh5for_unstructured_contiguous_hyperslab_WriteGeometry_R4P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Write an R4P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
        real(R4P),                                    intent(IN)    :: Coordinates(:)    !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Geometry dataset name
    !-----------------------------------------------------------------
        call this%LightData%SetGeometry(Coordinates = Coordinates, Name = Name)
        call this%HeavyData%WriteGeometry(Coordinates = Coordinates, Name = Name)
    end subroutine xh5for_unstructured_contiguous_hyperslab_WriteGeometry_R4P


    subroutine xh5for_unstructured_contiguous_hyperslab_WriteGeometry_R8P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Write an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
        real(R8P),                                    intent(IN)    :: Coordinates(:)    !< Grid coordinates
        character(len=*),                             intent(IN)    :: Name              !< Geometry dataset name
    !-----------------------------------------------------------------
        call this%LightData%SetGeometry(Coordinates = Coordinates, Name = Name)
        call this%HeavyData%WriteGeometry(Coordinates = Coordinates, Name = Name)
    end subroutine xh5for_unstructured_contiguous_hyperslab_WriteGeometry_R8P


    subroutine xh5for_unstructured_contiguous_hyperslab_ReadGeometry_R4P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Read an R4P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
        real(R4P), allocatable,                       intent(OUT)   :: Coordinates(:)    !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Geometry dataset name
    !-----------------------------------------------------------------
        call this%HeavyData%ReadGeometry(Coordinates = Coordinates, Name = Name)
        call this%LightData%SetGeometry(Coordinates = Coordinates, Name = Name)
    end subroutine xh5for_unstructured_contiguous_hyperslab_ReadGeometry_R4P


    subroutine xh5for_unstructured_contiguous_hyperslab_ReadGeometry_R8P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Read an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
        real(R8P), allocatable,                       intent(OUT)   :: Coordinates(:)    !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Geometry dataset name
    !-----------------------------------------------------------------
        call this%HeavyData%ReadGeometry(Coordinates = Coordinates, Name = Name)
        call this%LightData%SetGeometry(Coordinates = Coordinates, Name = Name)
    end subroutine xh5for_unstructured_contiguous_hyperslab_ReadGeometry_R8P


    subroutine xh5for_unstructured_contiguous_hyperslab_WriteTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Write an I4P Topology for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
        integer(R4P),                                 intent(IN)    :: Connectivities(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
        call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = Name)
    end subroutine xh5for_unstructured_contiguous_hyperslab_WriteTopology_I4P


    subroutine xh5for_unstructured_contiguous_hyperslab_WriteTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Write an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
        integer(I8P),                                 intent(IN)    :: Connectivities(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Topology dataset name
    !----------------------------------------------------------------- 
        call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
        call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = Name)
    end subroutine xh5for_unstructured_contiguous_hyperslab_WriteTopology_I8P


    subroutine xh5for_unstructured_contiguous_hyperslab_ReadTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read an I4P Topology for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
        integer(R4P), allocatable,                    intent(OUT)   :: Connectivities(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = Name)
        call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
    end subroutine xh5for_unstructured_contiguous_hyperslab_ReadTopology_I4P


    subroutine xh5for_unstructured_contiguous_hyperslab_ReadTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler for Unstructured Grids
        integer(I8P), allocatable,                    intent(OUT)   :: Connectivities(:) !< Grid connectivities
        character(len=*),                             intent(IN)    :: Name              !< Topology dataset name
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = Name)
        call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
    end subroutine xh5for_unstructured_contiguous_hyperslab_ReadTopology_I8P

end module xh5for_unstructured_contiguous_hyperslab_handler
