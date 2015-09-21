module xh5for_handler

use IR_Precision, only : I4P, I8P, R4P, R8P
use xdmf_handler
use hdf5_handler
use mpi_environment
use uniform_grid_descriptor
use spatial_grid_descriptor

implicit none

private

    type, abstract :: xh5for_handler_t
    contains
    private
        procedure(xh5for_handler_WriteGeometry_I4P),       deferred :: WriteGeometry_I4P
        procedure(xh5for_handler_WriteGeometry_I8P),       deferred :: WriteGeometry_I8P
        procedure(xh5for_handler_WriteTopology_R4P),       deferred :: WriteTopology_R4P
        procedure(xh5for_handler_WriteTopology_R8P),       deferred :: WriteTopology_R8P
        procedure(xh5for_handler_Initialize),      public, deferred :: Initialize
        procedure(xh5for_handler_Open),            public, deferred :: Open
        procedure(xh5for_handler_Close),           public, deferred :: Close
        generic,                                   public           :: WriteTopology => WriteTopology_R4P, &
                                                                                        WriteTopology_R8P
        generic,                                   public           :: WriteGeometry => WriteGeometry_I4P, &
                                                                                        WriteGeometry_I8P
!        procedure(xh5for_handler_WriteAttribute), public, deferred :: WriteAttribute

    end type xh5for_handler_t


    abstract interface
        subroutine xh5for_handler_Initialize(this, MPIEnvironment, UniformGridDescriptor, SpatialGridDescriptor)
            import xh5for_handler_t 
            import mpi_env_t 
            import uniform_grid_descriptor_t 
            import spatial_grid_descriptor_t 
            class(xh5for_handler_t), intent(INOUT) :: this
            type(mpi_env_t),                 intent(IN)    :: MPIEnvironment        !< MPI environment 
            type(uniform_grid_descriptor_t), intent(IN)    :: UniformGridDescriptor !< Uniform grid descriptor
            type(spatial_grid_descriptor_t), intent(IN)    :: SpatialGridDescriptor !< Spatial grid descriptor
        end subroutine xh5for_handler_Initialize
    end interface

    abstract interface
        subroutine xh5for_handler_Open(this, fileprefix)
            import xh5for_handler_t 
            class(xh5for_handler_t), intent(INOUT) :: this
            character(len=*),        intent(IN)    :: fileprefix
        end subroutine xh5for_handler_Open
    end interface

    abstract interface
        subroutine xh5for_handler_Close(this)
            import xh5for_handler_t
            class(xh5for_handler_t), intent(INOUT) :: this
        end subroutine xh5for_handler_Close
    end interface

    abstract interface
        subroutine xh5for_handler_WriteGeometry_I4P(this, Connectivities)
            import xh5for_handler_t
            import I4P
            class(xh5for_handler_t), intent(INOUT) :: this
            integer(I4P),            intent(IN)    :: Connectivities(:)
        end subroutine xh5for_handler_WriteGeometry_I4P
    end interface

    abstract interface
        subroutine xh5for_handler_WriteGeometry_I8P(this, Connectivities)
            import xh5for_handler_t
            import I8P
            class(xh5for_handler_t), intent(INOUT) :: this
            integer(I8P),            intent(IN)    :: Connectivities(:)
        end subroutine xh5for_handler_WriteGeometry_I8P
    end interface

    abstract interface
        subroutine xh5for_handler_WriteTopology_R4P(this, Coordinates)
            import xh5for_handler_t
            import R4P
            class(xh5for_handler_t), intent(INOUT) :: this
            real(R4P),               intent(IN)    :: Coordinates(:)
        end subroutine xh5for_handler_WriteTopology_R4P
    end interface

    abstract interface
        subroutine xh5for_handler_WriteTopology_R8P(this, Coordinates)
            import xh5for_handler_t
            import R8P
            class(xh5for_handler_t), intent(INOUT) :: this
            real(R8P),               intent(IN)    :: Coordinates(:)
        end subroutine xh5for_handler_WriteTopology_R8P
    end interface

    abstract interface
        subroutine xh5for_handler_Attribute(this)
            import xh5for_handler_t
            class(xh5for_handler_t), intent(INOUT) :: this
        end subroutine xh5for_handler_Attribute
    end interface

public :: xh5for_handler_t
public :: mpi_env_t
public :: uniform_grid_descriptor_t
public :: spatial_grid_descriptor_t

end module xh5for_handler
