module xdmf_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF file handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use xh5for_parameters
use IR_Precision, only : I4P, I8P, R4P, R8P
use fox_xdmf
use mpi_environment
use spatial_grid_descriptor
use uniform_grid_descriptor

implicit none

private

    type, abstract :: xdmf_handler_t
    !-----------------------------------------------------------------
    !< XDMF handler abstract type
    !----------------------------------------------------------------- 
        character(len=:),            allocatable :: prefix                          !< Name prefix of the XDMF file
        character(len=4)                         :: ext = '.xmf'                    !< XDMF file extension
        type(xdmf_file_t)                        :: file                            !< XDMF file handler
        type(mpi_env_t),                 pointer :: MPIEnvironment        => null() !< MPI environment 
        type(spatial_grid_descriptor_t), pointer :: SpatialGridDescriptor => null() !< Global grid info
        type(uniform_grid_descriptor_t), pointer :: UniformGridDescriptor => null() !< Local grid info
        integer                                  :: NumberOfAttributes = 0          !< Number of attributes of the grid
        logical                                  :: warn = .true.                   !< Flag to show warnings on screen
    contains
    private
        procedure(xdmf_handler_SetGeometry_R4P),     deferred :: SetGeometry_R4P
        procedure(xdmf_handler_SetGeometry_R8P),     deferred :: SetGeometry_R8P
        procedure(xdmf_handler_SetTopology_I4P),     deferred :: SetTopology_I4P
        procedure(xdmf_handler_SetTopology_I8P),     deferred :: SetTopology_I8P
        procedure(xdmf_handler_AppendAttribute_I4P), deferred :: AppendAttribute_I4P
        procedure(xdmf_handler_AppendAttribute_I8P), deferred :: AppendAttribute_I8P
        procedure(xdmf_handler_AppendAttribute_R4P), deferred :: AppendAttribute_R4P
        procedure(xdmf_handler_AppendAttribute_R8P), deferred :: AppendAttribute_R8P
        procedure(xdmf_handler_Serialize), public,   deferred :: Serialize
        procedure,                                   public   :: Initialize      => xdmf_handler_Initialize
        procedure,                                   public   :: Free            => xdmf_handler_Free
        procedure,                                   public   :: OpenFile        => xdmf_handler_OpenFile
        procedure,                                   public   :: CloseFile       => xdmf_handler_CloseFile
        generic,                                     public   :: SetGeometry     => SetGeometry_R4P, &
                                                                                    SetGeometry_R8P
        generic,                                     public   :: SetTopology     => SetTopology_I4P, &
                                                                                    SetTopology_I8P
        generic,                                     public   :: AppendAttribute => AppendAttribute_I4P, &
                                                                                    AppendAttribute_I8P, &
                                                                                    AppendAttribute_R4P, &
                                                                                    AppendAttribute_R8P
    end type xdmf_handler_t

    abstract interface
        subroutine xdmf_handler_SetGeometry_R4P(this, Coordinates)
            import xdmf_handler_t
            import R4P
            class(xdmf_handler_t), intent(INOUT) :: this
            real(R4P),             intent(IN)    :: Coordinates(:)
        end subroutine xdmf_handler_SetGeometry_R4P
    end interface

    abstract interface
        subroutine xdmf_handler_SetGeometry_R8P(this, Coordinates)
            import xdmf_handler_t
            import R8P
            class(xdmf_handler_t), intent(INOUT) :: this
            real(R8P),             intent(IN)    :: Coordinates(:)
        end subroutine xdmf_handler_SetGeometry_R8P
    end interface

    abstract interface
        subroutine xdmf_handler_SetTopology_I4P(this, Connectivities)
            import xdmf_handler_t
            import I4P
            class(xdmf_handler_t), intent(INOUT) :: this
            integer(I4P),          intent(IN)    :: Connectivities(:)
        end subroutine xdmf_handler_SetTopology_I4P
    end interface

    abstract interface
        subroutine xdmf_handler_SetTopology_I8P(this, Connectivities)
            import xdmf_handler_t
            import I8P
            class(xdmf_handler_t), intent(INOUT) :: this
            integer(I8P),          intent(IN)    :: Connectivities(:)
        end subroutine xdmf_handler_SetTopology_I8P
    end interface

    abstract interface
        subroutine xdmf_handler_AppendAttribute_I4P(this, Name, Type, Center, Attribute)
            import xdmf_handler_t
            import I4P
            class(xdmf_handler_t), intent(INOUT) :: this         
            character(len=*),      intent(IN)    :: Name         
            integer(I4P),          intent(IN)    :: Type         
            integer(I4P),          intent(IN)    :: Center       
            integer(I4P),          intent(IN)    :: Attribute(:) 
        end subroutine xdmf_handler_AppendAttribute_I4P
    end interface

    abstract interface
        subroutine xdmf_handler_AppendAttribute_I8P(this, Name, Type, Center, Attribute)
            import xdmf_handler_t
            import I4P
            import I8P
            class(xdmf_handler_t), intent(INOUT) :: this         
            character(len=*),      intent(IN)    :: Name         
            integer(I4P),          intent(IN)    :: Type         
            integer(I4P),          intent(IN)    :: Center       
            integer(I8P),          intent(IN)    :: Attribute(:) 
        end subroutine xdmf_handler_AppendAttribute_I8P
    end interface

    abstract interface
        subroutine xdmf_handler_AppendAttribute_R4P(this, Name, Type, Center, Attribute)
            import xdmf_handler_t
            import I4P
            import R4P
            class(xdmf_handler_t), intent(INOUT) :: this         
            character(len=*),      intent(IN)    :: Name         
            integer(I4P),          intent(IN)    :: Type         
            integer(I4P),          intent(IN)    :: Center       
            real(R4P),             intent(IN)    :: Attribute(:) 
        end subroutine xdmf_handler_AppendAttribute_R4P
    end interface

    abstract interface
        subroutine xdmf_handler_AppendAttribute_R8P(this, Name, Type, Center, Attribute)
            import xdmf_handler_t
            import I4P
            import R8P
            class(xdmf_handler_t), intent(INOUT) :: this         
            character(len=*),      intent(IN)    :: Name         
            integer(I4P),          intent(IN)    :: Type         
            integer(I4P),          intent(IN)    :: Center       
            real(R8P),             intent(IN)    :: Attribute(:) 
        end subroutine xdmf_handler_AppendAttribute_R8P
    end interface

    abstract interface
        subroutine xdmf_handler_Serialize(this)
            import xdmf_handler_t
            class(xdmf_handler_t), intent(INOUT) :: this
        end subroutine xdmf_handler_Serialize
    end interface


public :: xdmf_handler_t

contains

    subroutine xdmf_handler_Initialize(this, MPIEnvironment, UniformGridDescriptor, SpatialGridDescriptor)
    !-----------------------------------------------------------------
    !< XDMF file handler initialization procedure
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),                   intent(INOUT) :: this               !< XMDF handler
        type(mpi_env_t),                 target, intent(IN) :: MPIEnvironment        !< MPI environment
        type(uniform_grid_descriptor_t), target, intent(IN) :: UniformGridDescriptor !< Local grid info
        type(spatial_grid_descriptor_t), target, intent(IN) :: SpatialGridDescriptor !< Global grid info
    !----------------------------------------------------------------- 
        call this%Free()
        this%MPIEnvironment        => MPIEnvironment
        this%SpatialGridDescriptor => SpatialGridDescriptor
        this%UniformGridDescriptor => UniformGridDescriptor
    end subroutine xdmf_handler_Initialize


    subroutine xdmf_handler_Free(this)
    !-----------------------------------------------------------------
    !< Free XDMF file handler
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),                   intent(INOUT) :: this               !< XMDF handler
    !----------------------------------------------------------------- 
        if(allocated(this%prefix)) deallocate(this%prefix)
        !call this%file%Free()
        nullify(this%MPIEnvironment)
        nullify(this%SpatialGridDescriptor)
        nullify(this%UniformGridDescriptor)
    end subroutine xdmf_handler_Free


    subroutine xdmf_handler_OpenFile(this, fileprefix)
    !-----------------------------------------------------------------
    !< Open a XDMF file
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        character(len=*),      intent(IN)    :: fileprefix            !< XDMF filename
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            this%prefix = trim(adjustl(fileprefix))
            call this%file%set_filename(trim(adjustl(fileprefix))//this%ext)
            call this%file%openfile()
        endif
    end subroutine xdmf_handler_OpenFile


    subroutine xdmf_handler_CloseFile(this)
    !-----------------------------------------------------------------
    !< Close a XDMF file
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            call this%file%closefile()
        endif
    end subroutine xdmf_handler_CloseFile

end module xdmf_handler
