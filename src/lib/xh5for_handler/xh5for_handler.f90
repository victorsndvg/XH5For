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
        procedure(xh5for_handler_WriteGeometry_R4P),       deferred :: WriteGeometry_R4P
        procedure(xh5for_handler_WriteGeometry_R8P),       deferred :: WriteGeometry_R8P
        procedure(xh5for_handler_ReadGeometry_R4P),        deferred :: ReadGeometry_R4P
        procedure(xh5for_handler_ReadGeometry_R8P),        deferred :: ReadGeometry_R8P
        procedure(xh5for_handler_WriteTopology_I4P),       deferred :: WriteTopology_I4P
        procedure(xh5for_handler_WriteTopology_I8P),       deferred :: WriteTopology_I8P
        procedure(xh5for_handler_ReadTopology_I4P),        deferred :: ReadTopology_I4P
        procedure(xh5for_handler_ReadTopology_I8P),        deferred :: ReadTopology_I8P
        procedure(xh5for_handler_WriteAttribute_I4P),      deferred :: WriteAttribute_I4P
        procedure(xh5for_handler_WriteAttribute_I8P),      deferred :: WriteAttribute_I8P
        procedure(xh5for_handler_WriteAttribute_R4P),      deferred :: WriteAttribute_R4P
        procedure(xh5for_handler_WriteAttribute_R8P),      deferred :: WriteAttribute_R8P
        procedure(xh5for_handler_Initialize),      public, deferred :: Initialize
        procedure(xh5for_handler_Free),            public, deferred :: Free
        procedure(xh5for_handler_Open),            public, deferred :: Open
        procedure(xh5for_handler_Parse),           public, deferred :: Parse
        procedure(xh5for_handler_Close),           public, deferred :: Close
        generic,                                   public           :: WriteTopology  => WriteTopology_I4P, &
                                                                                         WriteTopology_I8P
        generic,                                   public           :: WriteGeometry  => WriteGeometry_R4P, &
                                                                                         WriteGeometry_R8P
        generic,                                   public           :: ReadGeometry   => ReadGeometry_R4P, &
                                                                                         ReadGeometry_R8P
        generic,                                   public           :: ReadTopology   => ReadTopology_I4P, &
                                                                                         ReadTopology_I8P
        generic,                                   public           :: WriteAttribute => WriteAttribute_I4P, &
                                                                                         WriteAttribute_I8P, &
                                                                                         WriteAttribute_R4P, &
                                                                                         WriteAttribute_R8P

    end type xh5for_handler_t


    abstract interface
        subroutine xh5for_handler_Initialize(this, MPIEnvironment, UniformGridDescriptor, SpatialGridDescriptor)
            import xh5for_handler_t 
            import mpi_env_t 
            import uniform_grid_descriptor_t 
            import spatial_grid_descriptor_t 
            class(xh5for_handler_t),                 intent(INOUT) :: this
            type(mpi_env_t),                 target, intent(IN)    :: MPIEnvironment        !< MPI environment 
            type(uniform_grid_descriptor_t), target, intent(IN)    :: UniformGridDescriptor !< Uniform grid descriptor
            type(spatial_grid_descriptor_t), target, intent(IN)    :: SpatialGridDescriptor !< Spatial grid descriptor
        end subroutine xh5for_handler_Initialize

        subroutine xh5for_handler_Free(this)
            import xh5for_handler_t
            class(xh5for_handler_t), intent(INOUT) :: this
        end subroutine xh5for_handler_Free

        subroutine xh5for_handler_Open(this, action, fileprefix)
            import xh5for_handler_t 
            import I4P
            class(xh5for_handler_t), intent(INOUT) :: this
            integer(I4P),            intent(IN)    :: action
            character(len=*),        intent(IN)    :: fileprefix
        end subroutine xh5for_handler_Open

        subroutine xh5for_handler_Parse(this)
            import xh5for_handler_t
            class(xh5for_handler_t), intent(INOUT) :: this
        end subroutine xh5for_handler_Parse

        subroutine xh5for_handler_Close(this)
            import xh5for_handler_t
            class(xh5for_handler_t), intent(INOUT) :: this
        end subroutine xh5for_handler_Close

        subroutine xh5for_handler_WriteGeometry_R4P(this, Coordinates)
            import xh5for_handler_t
            import R4P
            class(xh5for_handler_t), intent(INOUT) :: this
            real(R4P),               intent(IN)    :: Coordinates(:)
        end subroutine xh5for_handler_WriteGeometry_R4P

        subroutine xh5for_handler_WriteGeometry_R8P(this, Coordinates)
            import xh5for_handler_t
            import R8P
            class(xh5for_handler_t), intent(INOUT) :: this
            real(R8P),               intent(IN)    :: Coordinates(:)
        end subroutine xh5for_handler_WriteGeometry_R8P

        subroutine xh5for_handler_ReadGeometry_R4P(this, Coordinates)
            import xh5for_handler_t
            import R4P
            class(xh5for_handler_t), intent(INOUT) :: this
            real(R4P), allocatable,  intent(OUT)   :: Coordinates(:)
        end subroutine xh5for_handler_ReadGeometry_R4P

        subroutine xh5for_handler_ReadGeometry_R8P(this, Coordinates)
            import xh5for_handler_t
            import R8P
            class(xh5for_handler_t), intent(INOUT) :: this
            real(R8P), allocatable,  intent(OUT)   :: Coordinates(:)
        end subroutine xh5for_handler_ReadGeometry_R8P

        subroutine xh5for_handler_WriteTopology_I4P(this, Connectivities)
            import xh5for_handler_t
            import I4P
            class(xh5for_handler_t), intent(INOUT) :: this
            integer(I4P),            intent(IN)    :: Connectivities(:)
        end subroutine xh5for_handler_WriteTopology_I4P

        subroutine xh5for_handler_WriteTopology_I8P(this, Connectivities)
            import xh5for_handler_t
            import I8P
            class(xh5for_handler_t), intent(INOUT) :: this
            integer(I8P),            intent(IN)    :: Connectivities(:)
        end subroutine xh5for_handler_WriteTopology_I8P

        subroutine xh5for_handler_ReadTopology_I4P(this, Connectivities)
            import xh5for_handler_t
            import I4P
            class(xh5for_handler_t), intent(INOUT) :: this
            integer(I4P), allocatable, intent(OUT) :: Connectivities(:)
        end subroutine xh5for_handler_ReadTopology_I4P

        subroutine xh5for_handler_ReadTopology_I8P(this, Connectivities)
            import xh5for_handler_t
            import I8P
            class(xh5for_handler_t),   intent(INOUT) :: this
            integer(I8P), allocatable, intent(OUT)   :: Connectivities(:)
        end subroutine xh5for_handler_ReadTopology_I8P

        subroutine xh5for_handler_WriteAttribute_I4P(this, Name, Type, Center, Values)
            import xh5for_handler_t
            import I4P
            class(xh5for_handler_t), intent(INOUT) :: this
            character(len=*),        intent(IN)    :: Name
            integer(I4P),            intent(IN)    :: Type
            integer(I4P),            intent(IN)    :: Center
            integer(I4P),            intent(IN)    :: Values(:)
        end subroutine xh5for_handler_WriteAttribute_I4P

        subroutine xh5for_handler_WriteAttribute_I8P(this, Name, Type, Center, Values)
            import xh5for_handler_t
            import I8P
            import I4P
            class(xh5for_handler_t), intent(INOUT) :: this
            character(len=*),        intent(IN)    :: Name
            integer(I4P),            intent(IN)    :: Type
            integer(I4P),            intent(IN)    :: Center
            integer(I8P),            intent(IN)    :: Values(:)
        end subroutine xh5for_handler_WriteAttribute_I8P

        subroutine xh5for_handler_WriteAttribute_R4P(this, Name, Type, Center, Values)
            import xh5for_handler_t
            import R4P
            import I4P
            class(xh5for_handler_t), intent(INOUT) :: this
            character(len=*),        intent(IN)    :: Name
            integer(I4P),            intent(IN)    :: Type
            integer(I4P),            intent(IN)    :: Center
            real(R4P),               intent(IN)    :: Values(:)
        end subroutine xh5for_handler_WriteAttribute_R4P

        subroutine xh5for_handler_WriteAttribute_R8P(this, Name, Type, Center, Values)
            import xh5for_handler_t
            import R8P
            import I4P
            class(xh5for_handler_t), intent(INOUT) :: this
            character(len=*),        intent(IN)    :: Name
            integer(I4P),            intent(IN)    :: Type
            integer(I4P),            intent(IN)    :: Center
            real(R8P),               intent(IN)    :: Values(:)
        end subroutine xh5for_handler_WriteAttribute_R8P
    end interface

public :: xh5for_handler_t
public :: mpi_env_t
public :: uniform_grid_descriptor_t
public :: spatial_grid_descriptor_t

end module xh5for_handler
