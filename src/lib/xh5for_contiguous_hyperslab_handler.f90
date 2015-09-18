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
        procedure         :: WriteGeometry_I4P => xh5for_contiguous_hyperslab_handler_WriteGeometry_I4P
!        procedure         :: WriteGeometry_I8P => xh5for_contiguous_hyperslab_handler_WriteGeometry_I8P
        procedure         :: WriteTopology_R4P => xh5for_contiguous_hyperslab_handler_WriteTopology_R4P
        procedure         :: WriteTopology_R8P => xh5for_contiguous_hyperslab_handler_WriteTopology_R8P
        procedure, public :: Initialize        => xh5for_contiguous_hyperslab_handler_Initialize
        procedure, public :: Open              => xh5for_contiguous_hyperslab_handler_Open
        procedure, public :: Close             => xh5for_contiguous_hyperslab_handler_Close
!        procedure, public :: WriteAttribute => xh5for_contiguous_hyperslab_handler_WriteAttribute
    end type xh5for_contiguous_hyperslab_handler_t

public :: xh5for_contiguous_hyperslab_handler_t

contains

    subroutine xh5for_contiguous_hyperslab_handler_Initialize(this, MPIEnvironment, UniformGridDescriptor, SpatialGridDescriptor)
        class(xh5for_contiguous_hyperslab_handler_t),         intent(INOUT) :: this
        type(mpi_env_t),                 intent(IN)    :: MPIEnvironment        !< MPI environment 
        type(spatial_grid_descriptor_t), intent(IN)    :: SpatialGridDescriptor !< Spatial grid descriptor
        type(uniform_grid_descriptor_t), intent(IN)    :: UniformGridDescriptor !< Uniform grid descriptor
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
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this
        character(len=*),         intent(IN)    :: fileprefix
        call this%LightData%OpenFile(fileprefix)
        call this%HeavyData%OpenFile(fileprefix)
    end subroutine xh5for_contiguous_hyperslab_handler_Open


    subroutine xh5for_contiguous_hyperslab_handler_Close(this)
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this
        call this%LightData%CloseFile()
        call this%HeavyData%CloseFile()
    end subroutine xh5for_contiguous_hyperslab_handler_Close


    subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_I4P(this, Connectivities)
        class(xh5for_contiguous_hyperslab_handler_t), intent(IN) :: this
        integer(I4P),                                 intent(IN) :: Connectivities(:)      !< Grid connectivities
!        call this%LightData%AddGeometry(Connectivities)
        call this%HeavyData%WriteGeometry(Connectivities = Connectivities)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_I4P

    subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_I8P(this, Connectivities)
        class(xh5for_contiguous_hyperslab_handler_t), intent(IN) :: this
        real(I8P),                                    intent(IN) :: Connectivities(:)      !< Grid coordinates
!        call this%LightData%AddGeometry(Connectivities)
!        call this%HeavyData%WriteGeometry(Connectivities)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteGeometry_I8P

    subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_R4P(this, Coordinates)
        class(xh5for_contiguous_hyperslab_handler_t), intent(IN) :: this
        real(R4P),                                    intent(IN) :: Coordinates(:)      !< Grid connectivities
!        call this%LightData%AddTopology(Coordinates)
        call this%HeavyData%WriteTopology(Coordinates = Coordinates)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_R4P

    subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_R8P(this, Coordinates)
        class(xh5for_contiguous_hyperslab_handler_t), intent(IN) :: this
        real(R8P),                                    intent(IN) :: Coordinates(:)      !< Grid connectivities
!        call this%LightData%AddTopology(Coordinates)
        call this%HeavyData%WriteTopology(Coordinates = Coordinates)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteTopology_R8P


!        subroutine xh5for_handler_Attribute(this)
!            class(xh5for_handler_t), intent(INOUT) :: this
!        end subroutine xh5for_handler_Attribute


end module xh5for_contiguous_hyperslab_handler
