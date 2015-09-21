module xh5for_contiguous_hyperslab_handler

use xh5for_handler
use IR_Precision, only : I4P, I8P, R4P, R8P
use xdmf_contiguous_hyperslab_handler
use hdf5_contiguous_hyperslab_handler

implicit none

private

    type, extends(xh5for_handler_t) :: xh5for_contiguous_hyperslab_handler_t
        type(xdmf_contiguous_hyperslab_handler_t) :: LightData
        type(hdf5_contiguous_hyperslab_handler_t) :: HeavyData
    contains
        procedure         :: WriteGeometry_R4P => xh5for_contiguous_hyperslab_handler_WriteGeometry_R4P
        procedure         :: WriteGeometry_R8P => xh5for_contiguous_hyperslab_handler_WriteGeometry_R8P
        procedure         :: WriteTopology_I4P => xh5for_contiguous_hyperslab_handler_WriteTopology_I4P
        procedure         :: WriteTopology_I8P => xh5for_contiguous_hyperslab_handler_WriteTopology_I8P
        procedure, public :: Initialize        => xh5for_contiguous_hyperslab_handler_Initialize
        procedure, public :: Open              => xh5for_contiguous_hyperslab_handler_Open
        procedure, public :: Close             => xh5for_contiguous_hyperslab_handler_Close
!        procedure, public :: WriteAttribute => xh5for_contiguous_hyperslab_handler_WriteAttribute
    end type xh5for_contiguous_hyperslab_handler_t

public :: xh5for_contiguous_hyperslab_handler_t

contains

    subroutine xh5for_contiguous_hyperslab_handler_Initialize(this, MPIEnvironment, UniformGridDescriptor, SpatialGridDescriptor)
    !-----------------------------------------------------------------
    !< XH5FOR initialization procedure
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this                  !< XH5For contiguous hyperslab handler
        type(mpi_env_t),                              intent(IN)    :: MPIEnvironment        !< MPI environment 
        type(spatial_grid_descriptor_t),              intent(IN)    :: SpatialGridDescriptor !< Spatial grid descriptor
        type(uniform_grid_descriptor_t),              intent(IN)    :: UniformGridDescriptor !< Uniform grid descriptor
    !-----------------------------------------------------------------
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


    subroutine xh5for_contiguous_hyperslab_handler_Open(this, fileprefix)
    !-----------------------------------------------------------------
    !< Open the lightdata and the heavydata files
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: fileprefix   !< Filename prefix
    !-----------------------------------------------------------------
        call this%HeavyData%OpenFile(fileprefix)
        call this%LightData%OpenFile(fileprefix)
    end subroutine xh5for_contiguous_hyperslab_handler_Open


    subroutine xh5for_contiguous_hyperslab_handler_Close(this)
    !-----------------------------------------------------------------
    !< Close the lightdata and the heavydata files
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contigous hyperslab handler
    !-----------------------------------------------------------------
        call this%HeavyData%CloseFile()
        !< XDMF deferred writing when hdf5 closes    
        call this%LightData%DeferredWrite()
        call this%LightData%CloseFile()
    end subroutine xh5for_contiguous_hyperslab_handler_Close


    subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_R4P(this, Coordinates)
    !-----------------------------------------------------------------
    !< Write an R4P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XH5For contiguous hyperslab handler
        real(R4P),                                    intent(IN)    :: Coordinates(:) !< Grid connectivities
    !-----------------------------------------------------------------
        call this%LightData%AddGeometry(Coordinates = Coordinates)
        call this%HeavyData%WriteGeometry(Coordinates = Coordinates)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_R4P


    subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_R8P(this, Coordinates)
    !-----------------------------------------------------------------
    !< Write an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XH5For contiguous hyperslab handler
        real(R8P),                                    intent(IN)    :: Coordinates(:) !< Grid coordinates
    !-----------------------------------------------------------------
        call this%LightData%AddGeometry(Coordinates = Coordinates)
        call this%HeavyData%WriteGeometry(Coordinates =  Coordinates)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_R8P


    subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_I4P(this, Connectivities)
    !-----------------------------------------------------------------
    !< Write an I4P Topology for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this            !< XH5For contiguous hyperslab handler
        integer(R4P),                                 intent(IN)    :: Connectivities(:)  !< Grid connectivities
    !-----------------------------------------------------------------
        call this%LightData%AddTopology(Connectivities = Connectivities)
        call this%HeavyData%WriteTopology(Connectivities = Connectivities)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_I4P


    subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_I8P(this, Connectivities)
    !-----------------------------------------------------------------
    !< Write an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this            !< XH5For contiguous hyperslab handler
        integer(I8P),                                 intent(IN)    :: Connectivities(:)  !< Grid connectivities
    !----------------------------------------------------------------- 
        call this%LightData%AddTopology(Connectivities = Connectivities)
        call this%HeavyData%WriteTopology(Connectivities = Connectivities)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_I8P
    !----------------------------------------------------------------- 

!        subroutine xh5for_handler_Attribute(this)
!            class(xh5for_handler_t), intent(INOUT) :: this
!        end subroutine xh5for_handler_Attribute


end module xh5for_contiguous_hyperslab_handler
