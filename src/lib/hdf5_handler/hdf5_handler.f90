module hdf5_handler

#ifdef ENABLE_HDF5
use HDF5
use IR_Precision, only : I8P, R4P, R8P, str
#else
use IR_Precision, only : I4P, I8P, R4P, R8P, str
#endif
use mpi_environment
use xh5for_parameters
use steps_handler
use spatial_grid_descriptor
use uniform_grid_descriptor

implicit none

#include "assert.i90"

private

#ifndef ENABLE_HDF5
    integer, parameter :: HID_T = I4P
    integer, parameter :: HSIZE_T = I4P
#endif

    integer(I4P), parameter :: HDF5_HANDLER_STATE_START = 0
    integer(I4P), parameter :: HDF5_HANDLER_STATE_INIT  = 1
    integer(I4P), parameter :: HDF5_HANDLER_STATE_OPEN  = 2
    integer(I4P), parameter :: HDF5_HANDLER_STATE_CLOSE = 3

    !-----------------------------------------------------------------
    ! HDF5_HANDLER State Transition Diagram
    !-----------------------------------------------------------------
    ! - This diagram controls the basic life cycle of the HDF5 file.
    ! - Only a public procedure (FileIsOpen) is needed to check if the
    !   handler is in the right state to perform I/O operations.
    ! - Only the next hierarchy layer needs to ensure this status via
    !   ReadHyperSlabs/WriteHyperSlabs/ReadDataset/WriteData/WriteMetadata
    !   procedures
    !----------------------------------------------------------------- 
    !       INIT STATE      |     ACTION      |      FINAL STATE
    !----------------------------------------------------------------- 
    ! START                 | Free            | START
    ! START                 | Initialize      | INIT
    !----------------------------------------------------------------- 
    ! INIT                  | Free            | START
    ! INIT                  | Initialize      | INIT
    ! INIT                  | OpenFile        | OPEN
    !----------------------------------------------------------------- 
    ! OPEN                  | Free            | START
    ! OPEN                  | Initialize      | INIT
    ! OPEN                  | OpenFile        | OPEN
    ! OPEN                  | CloseFile       | CLOSE
    !----------------------------------------------------------------- 
    ! CLOSE                 | Free            | START
    ! CLOSE                 | Initialize      | INIT
    ! CLOSE                 | OpenFile        | OPEN
    ! CLOSE                 | CloseFile       | CLOSE
    !----------------------------------------------------------------- 

    type, abstract :: hdf5_handler_t
    private
    !-----------------------------------------------------------------
    !< HDF5 abstract handler
    !----------------------------------------------------------------- 
        character(len=:),             allocatable :: prefix                           !< Name prefix of the HDF5 file
        integer(HID_T)                            :: FileID  = XDMF_NO_VALUE          !< File identifier 
        integer(I4P)                              :: action  = XDMF_NO_VALUE          !< HDF5 action to be perfomed (Read or Write)
        integer(I4P)                              :: state = HDF5_HANDLER_STATE_START !< HDF5 state
        type(mpi_env_t),                  pointer :: MPIEnvironment        => null()  !< MPI environment 
        type(steps_handler_t),            pointer :: StepsHandler          => null()  !< Steps handler
        class(spatial_grid_descriptor_t), pointer :: SpatialGridDescriptor => null()  !< Spatial grid descriptor
        class(uniform_grid_descriptor_t), pointer :: UniformGridDescriptor => null()  !< Uniform grid descriptor
    contains
    private
        procedure(hdf5_handler_WriteGeometry_XYZ_R4P),   deferred :: WriteGeometry_XYZ_R4P
        procedure(hdf5_handler_WriteGeometry_XYZ_R8P),   deferred :: WriteGeometry_XYZ_R8P
        procedure(hdf5_handler_WriteGeometry_X_Y_Z_R4P), deferred :: WriteGeometry_X_Y_Z_R4P
        procedure(hdf5_handler_WriteGeometry_X_Y_Z_R8P), deferred :: WriteGeometry_X_Y_Z_R8P
        procedure(hdf5_handler_WriteGeometry_DXDYDZ_R4P),deferred :: WriteGeometry_DXDYDZ_R4P
        procedure(hdf5_handler_WriteGeometry_DXDYDZ_R8P),deferred :: WriteGeometry_DXDYDZ_R8P
        procedure(hdf5_handler_ReadGeometry_XYZ_R4P),    deferred :: ReadGeometry_XYZ_R4P
        procedure(hdf5_handler_ReadGeometry_XYZ_R8P),    deferred :: ReadGeometry_XYZ_R8P
        procedure(hdf5_handler_ReadGeometry_X_Y_Z_R4P),  deferred :: ReadGeometry_X_Y_Z_R4P
        procedure(hdf5_handler_ReadGeometry_X_Y_Z_R8P),  deferred :: ReadGeometry_X_Y_Z_R8P
        procedure(hdf5_handler_ReadGeometry_DXDYDZ_R4P), deferred :: ReadGeometry_DXDYDZ_R4P
        procedure(hdf5_handler_ReadGeometry_DXDYDZ_R8P), deferred :: ReadGeometry_DXDYDZ_R8P
        procedure(hdf5_handler_WriteTopology_I4P),       deferred :: WriteTopology_I4P
        procedure(hdf5_handler_WriteTopology_I8P),       deferred :: WriteTopology_I8P
        procedure(hdf5_handler_ReadTopology_I4P),        deferred :: ReadTopology_I4P
        procedure(hdf5_handler_ReadTopology_I8P),        deferred :: ReadTopology_I8P
        procedure(hdf5_handler_WriteAttribute_I4P),      deferred :: WriteAttribute_I4P
        procedure(hdf5_handler_WriteAttribute_I8P),      deferred :: WriteAttribute_I8P
        procedure(hdf5_handler_WriteAttribute_R4P),      deferred :: WriteAttribute_R4P
        procedure(hdf5_handler_WriteAttribute_R8P),      deferred :: WriteAttribute_R8P
        procedure(hdf5_handler_ReadAttribute_I4P),       deferred :: ReadAttribute_I4P
        procedure(hdf5_handler_ReadAttribute_I8P),       deferred :: ReadAttribute_I8P
        procedure(hdf5_handler_ReadAttribute_R4P),       deferred :: ReadAttribute_R4P
        procedure(hdf5_handler_ReadAttribute_R8P),       deferred :: ReadAttribute_R8P
        procedure, non_overridable, public   :: Initialize               => hdf5_handler_Initialize
        procedure, non_overridable, public   :: Free                     => hdf5_handler_Free
        procedure, non_overridable, public   :: OpenFile                 => hdf5_handler_OpenFile
        procedure, non_overridable, public   :: FileIsOpen               => hdf5_handler_FileIsOpen
        procedure, non_overridable, public   :: CloseFile                => hdf5_handler_CloseFile
        procedure, non_overridable, public   :: GetFileID                => hdf5_handler_GetFileID
        procedure, non_overridable, public   :: GetAction                => hdf5_handler_GetAction
        procedure, non_overridable, public   :: GetMPIEnvironment        => hdf5_handler_GetMPIEnvironment
        procedure, non_overridable, public   :: GetUniformGridDescriptor => hdf5_handler_GetUniformGridDescriptor
        procedure, non_overridable, public   :: GetSpatialGridDescriptor => hdf5_handler_GetSpatialGridDescriptor
        generic,                    public   :: WriteTopology  => WriteTopology_I4P, &
                                                                  WriteTopology_I8P
        generic,                    public   :: ReadTopology   => ReadTopology_I4P, &
                                                                  ReadTopology_I8P
        generic,                    public   :: WriteGeometry  => WriteGeometry_XYZ_R4P,   &
                                                                  WriteGeometry_XYZ_R8P,   &
                                                                  WriteGeometry_X_Y_Z_R4P, &
                                                                  WriteGeometry_X_Y_Z_R8P, &
                                                                  WriteGeometry_DXDYDZ_R4P,&
                                                                  WriteGeometry_DXDYDZ_R8P
        generic,                    public   :: ReadGeometry   => ReadGeometry_XYZ_R4P,   &
                                                                  ReadGeometry_XYZ_R8P,   &
                                                                  ReadGeometry_X_Y_Z_R4P, &
                                                                  ReadGeometry_X_Y_Z_R8P, &
                                                                  ReadGeometry_DXDYDZ_R4P, &
                                                                  ReadGeometry_DXDYDZ_R8P
        generic,                    public   :: WriteAttribute => WriteAttribute_I4P, &
                                                                  WriteAttribute_I8P, &
                                                                  WriteAttribute_R4P, &
                                                                  WriteAttribute_R8P
        generic,                    public   :: ReadAttribute  => ReadAttribute_I4P, &
                                                                  ReadAttribute_I8P, &
                                                                  ReadAttribute_R4P, &
                                                                  ReadAttribute_R8P

    end type hdf5_handler_t

    abstract interface
        subroutine hdf5_handler_WriteGeometry_XYZ_R4P(this, XYZ, Name)
            import hdf5_handler_t
            import R4P
            class(hdf5_handler_t), intent(IN) :: this
            real(R4P),             intent(IN) :: XYZ(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteGeometry_XYZ_R4P

        subroutine hdf5_handler_WriteGeometry_XYZ_R8P(this, XYZ, Name)
            import hdf5_handler_t
            import R8P
            class(hdf5_handler_t), intent(IN) :: this
            real(R8P),             intent(IN) :: XYZ(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteGeometry_XYZ_R8P

        subroutine hdf5_handler_WriteGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
            import hdf5_handler_t
            import R4P
            class(hdf5_handler_t), intent(IN) :: this
            real(R4P),             intent(IN) :: X(:)
            real(R4P),             intent(IN) :: Y(:)
            real(R4P),             intent(IN) :: Z(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteGeometry_X_Y_Z_R4P

        subroutine hdf5_handler_WriteGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
            import hdf5_handler_t
            import R8P
            class(hdf5_handler_t), intent(IN) :: this
            real(R8P),             intent(IN) :: X(:)
            real(R8P),             intent(IN) :: Y(:)
            real(R8P),             intent(IN) :: Z(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteGeometry_X_Y_Z_R8P

        subroutine hdf5_handler_WriteGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
            import hdf5_handler_t
            import R4P
            class(hdf5_handler_t), intent(IN) :: this
            real(R4P),             intent(IN) :: Origin(:)
            real(R4P),             intent(IN) :: DxDyDz(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteGeometry_DXDYDZ_R4P

        subroutine hdf5_handler_WriteGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
            import hdf5_handler_t
            import R8P
            class(hdf5_handler_t), intent(IN) :: this
            real(R8P),             intent(IN) :: Origin(:)
            real(R8P),             intent(IN) :: DxDyDz(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteGeometry_DXDYDZ_R8P

        subroutine hdf5_handler_ReadGeometry_XYZ_R4P(this, XYZ, Name)
            import hdf5_handler_t
            import R4P
            class(hdf5_handler_t),  intent(IN)  :: this
            real(R4P), allocatable, intent(OUT) :: XYZ(:)
            character(len=*),      intent(IN)   :: Name
        end subroutine hdf5_handler_ReadGeometry_XYZ_R4P

        subroutine hdf5_handler_ReadGeometry_XYZ_R8P(this, XYZ, Name)
            import hdf5_handler_t
            import R8P
            class(hdf5_handler_t),  intent(IN)  :: this
            real(R8P), allocatable, intent(OUT) :: XYZ(:)
            character(len=*),       intent(IN)  :: Name
        end subroutine hdf5_handler_ReadGeometry_XYZ_R8P

        subroutine hdf5_handler_ReadGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
            import hdf5_handler_t
            import R4P
            class(hdf5_handler_t),  intent(IN)  :: this
            real(R4P), allocatable, intent(OUT) :: X(:)
            real(R4P), allocatable, intent(OUT) :: Y(:)
            real(R4P), allocatable, intent(OUT) :: Z(:)
            character(len=*),      intent(IN)   :: Name
        end subroutine hdf5_handler_ReadGeometry_X_Y_Z_R4P

        subroutine hdf5_handler_ReadGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
            import hdf5_handler_t
            import R8P
            class(hdf5_handler_t),  intent(IN)  :: this
            real(R8P), allocatable, intent(OUT) :: X(:)
            real(R8P), allocatable, intent(OUT) :: Y(:)
            real(R8P), allocatable, intent(OUT) :: Z(:)
            character(len=*),       intent(IN)  :: Name
        end subroutine hdf5_handler_ReadGeometry_X_Y_Z_R8P

        subroutine hdf5_handler_ReadGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
            import hdf5_handler_t
            import R4P
            class(hdf5_handler_t),  intent(IN)  :: this
            real(R4P), allocatable, intent(OUT) :: Origin(:)
            real(R4P), allocatable, intent(OUT) :: DxDyDz(:)
            character(len=*),       intent(IN)  :: Name
        end subroutine hdf5_handler_ReadGeometry_DXDYDZ_R4P

        subroutine hdf5_handler_ReadGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
            import hdf5_handler_t
            import R8P
            class(hdf5_handler_t),  intent(IN)  :: this
            real(R8P), allocatable, intent(OUT) :: Origin(:)
            real(R8P), allocatable, intent(OUT) :: DxDyDz(:)
            character(len=*),       intent(IN)  :: Name
        end subroutine hdf5_handler_ReadGeometry_DXDYDZ_R8P

        subroutine hdf5_handler_WriteTopology_I4P(this, Connectivities, Name)
            import hdf5_handler_t
            import I4P
            class(hdf5_handler_t), intent(IN) :: this
            integer(I4P),          intent(IN) :: Connectivities(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteTopology_I4P

        subroutine hdf5_handler_WriteTopology_I8P(this, Connectivities, Name)
            import hdf5_handler_t
            import I8P
            class(hdf5_handler_t), intent(IN) :: this
            integer(I8P),          intent(IN) :: Connectivities(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteTopology_I8P

        subroutine hdf5_handler_ReadTopology_I4P(this, Connectivities, Name)
            import hdf5_handler_t
            import I4P
            class(hdf5_handler_t),     intent(IN)  :: this
            integer(I4P), allocatable, intent(OUT) :: Connectivities(:)
            character(len=*),          intent(IN)  :: Name
        end subroutine hdf5_handler_ReadTopology_I4P

        subroutine hdf5_handler_ReadTopology_I8P(this, Connectivities, Name)
            import hdf5_handler_t
            import I8P
            class(hdf5_handler_t),     intent(IN)  :: this
            integer(I8P), allocatable, intent(OUT) :: Connectivities(:)
            character(len=*),          intent(IN)  :: Name
        end subroutine hdf5_handler_ReadTopology_I8P

        subroutine hdf5_handler_WriteAttribute_I4P(this, Name, Type, Center, Values)
            import hdf5_handler_t
            import I4P
            class(hdf5_handler_t), intent(IN) :: this
            character(len=*),      intent(IN) :: Name
            integer(I4P),          intent(IN) :: Type
            integer(I4P),          intent(IN) :: Center
            integer(I4P),          intent(IN) :: values(:)
        end subroutine hdf5_handler_WriteAttribute_I4P

        subroutine hdf5_handler_WriteAttribute_I8P(this, Name, Type, Center, Values)
            import hdf5_handler_t
            import I8P
            import I4P
            class(hdf5_handler_t), intent(IN) :: this
            character(len=*),      intent(IN) :: Name
            integer(I4P),          intent(IN) :: Type
            integer(I4P),          intent(IN) :: Center
            integer(I8P),          intent(IN) :: values(:)
        end subroutine hdf5_handler_WriteAttribute_I8P

        subroutine hdf5_handler_WriteAttribute_R4P(this, Name, Type, Center, Values)
            import hdf5_handler_t
            import R4P
            import I4P
            class(hdf5_handler_t), intent(IN) :: this
            character(len=*),      intent(IN) :: Name
            integer(I4P),          intent(IN) :: Type
            integer(I4P),          intent(IN) :: Center
            real(R4P),             intent(IN) :: values(:)
        end subroutine hdf5_handler_WriteAttribute_R4P

        subroutine hdf5_handler_WriteAttribute_R8P(this, Name, Type, Center, Values)
            import hdf5_handler_t
            import R8P
            import I4P
            class(hdf5_handler_t), intent(IN) :: this
            character(len=*),      intent(IN) :: Name
            integer(I4P),          intent(IN) :: Type
            integer(I4P),          intent(IN) :: Center
            real(R8P),             intent(IN) :: values(:)
        end subroutine hdf5_handler_WriteAttribute_R8P

        subroutine hdf5_handler_ReadAttribute_I4P(this, Name, Type, Center, Values)
            import hdf5_handler_t
            import I4P
            class(hdf5_handler_t),    intent(IN) :: this
            character(len=*),         intent(IN) :: Name
            integer(I4P),             intent(IN) :: Type
            integer(I4P),             intent(IN) :: Center
            integer(I4P), allocatable,intent(OUT):: values(:)
        end subroutine hdf5_handler_ReadAttribute_I4P

        subroutine hdf5_handler_ReadAttribute_I8P(this, Name, Type, Center, Values)
            import hdf5_handler_t
            import I4P
            import I8P
            class(hdf5_handler_t),  intent(IN) :: this
            character(len=*),        intent(IN) :: Name
            integer(I4P),            intent(IN) :: Type
            integer(I4P),            intent(IN) :: Center
            integer(I8P),allocatable,intent(OUT):: values(:)
        end subroutine hdf5_handler_ReadAttribute_I8P

        subroutine hdf5_handler_ReadAttribute_R4P(this, Name, Type, Center, Values)
            import hdf5_handler_t
            import I4P
            import R4P
            class(hdf5_handler_t), intent(IN) :: this
            character(len=*),      intent(IN) :: Name
            integer(I4P),          intent(IN) :: Type
            integer(I4P),          intent(IN) :: Center
            real(R4P), allocatable,intent(OUT):: values(:)
        end subroutine hdf5_handler_ReadAttribute_R4P

        subroutine hdf5_handler_ReadAttribute_R8P(this, Name, Type, Center, Values)
            import hdf5_handler_t
            import I4P
            import R8P
            class(hdf5_handler_t), intent(IN) :: this
            character(len=*),      intent(IN) :: Name
            integer(I4P),          intent(IN) :: Type
            integer(I4P),          intent(IN) :: Center
            real(R8P), allocatable,intent(OUT):: values(:)
        end subroutine hdf5_handler_ReadAttribute_R8P

    end interface

public :: hdf5_handler_t

#ifndef ENABLE_HDF5
public :: HID_T
public :: HSIZE_T
#endif

contains


    subroutine hdf5_handler_Initialize(this, MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor)
    !-----------------------------------------------------------------
    !< Initialize the HDF5 handler
    !----------------------------------------------------------------- 
        class(hdf5_handler_t),                    intent(INOUT) :: this                  !< HDF5 handler type
        type(mpi_env_t),                  target, intent(IN)    :: MPIEnvironment        !< MPI environment
        type(steps_handler_t),            target, intent(IN)    :: StepsHandler          !< Steps handler
        class(uniform_grid_descriptor_t), target, intent(IN)    :: UniformGridDescriptor !< Uniform grid descriptor 
        class(spatial_grid_descriptor_t), target, intent(IN)    :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        call this%Free()
        this%MPIEnvironment        => MPIEnvironment
        this%StepsHandler          => StepsHandler
        this%SpatialGridDescriptor => SpatialGridDescriptor
        this%UniformGridDescriptor => UniformGridDescriptor
        this%State                 =  HDF5_HANDLER_STATE_INIT
    end subroutine hdf5_handler_Initialize


    subroutine hdf5_handler_Free(this)
    !-----------------------------------------------------------------
    !< HDF5 handler free
    !----------------------------------------------------------------- 
        class(hdf5_handler_t),  intent(INOUT) :: this                 !< HDF5 handler type
    !----------------------------------------------------------------- 
        if(this%State == HDF5_HANDLER_STATE_OPEN) call this%CloseFile()
        this%FileID = XDMF_NO_VALUE
        this%action  = XDMF_NO_VALUE
        if(allocated(this%Prefix)) deallocate(this%Prefix)
        nullify(this%MPIEnvironment)
        nullify(this%UniformGridDescriptor)
        nullify(this%SpatialGridDescriptor)
        this%State = HDF5_HANDLER_STATE_START
    end subroutine hdf5_handler_Free


    function hdf5_handler_FileIsOpen(this) result(FileIsOpen)
    !-----------------------------------------------------------------
    !< Check if the HDF5 is already open. Needed to Write/Read
    !----------------------------------------------------------------- 
        class(hdf5_handler_t), intent(IN) :: this                     !< HDF5 handler type
        logical                           :: FileIsOpen               !< Check if file state is OPEN
    !----------------------------------------------------------------- 
        FileIsOpen = (this%State == HDF5_HANDLER_STATE_OPEN)
    end function hdf5_handler_FileIsOpen

    function hdf5_handler_GetAction(this) result(Action)
    !-----------------------------------------------------------------
    !< Return the current Action
    !----------------------------------------------------------------- 
        class(hdf5_handler_t), intent(IN) :: this                     !< HDF5 handler type
        integer(I4P)                      :: Action                   !< File ID
    !----------------------------------------------------------------- 
        assert(this%State > HDF5_HANDLER_STATE_START) ! Was initialized
        Action = this%Action
    end function hdf5_handler_GetAction


    function hdf5_handler_GetFileID(this) result(FileID)
    !-----------------------------------------------------------------
    !< Return the File ID
    !----------------------------------------------------------------- 
        class(hdf5_handler_t), intent(IN) :: this                     !< HDF5 handler type
        integer(I4P)                      :: FileID                   !< File ID
    !----------------------------------------------------------------- 
        assert(this%State == HDF5_HANDLER_STATE_OPEN) ! Was initialized
        FileID = this%FileID
    end function hdf5_handler_GetFileID


    function hdf5_handler_GetUniformGridDescriptor(this) result(UniformGridDescriptor)
    !-----------------------------------------------------------------
    !< Return a pointer to the UniformGridDescriptor
    !----------------------------------------------------------------- 
        class(hdf5_handler_t),         intent(IN) :: this                  !< HDF5 handler type
        class(uniform_grid_descriptor_t), pointer :: UniformGridDescriptor !< Uniform grid descriptor
    !----------------------------------------------------------------- 
        assert(this%State > HDF5_HANDLER_STATE_START) ! Was initialized
        nullify(UniformGridDescriptor)
        UniformGridDescriptor => this%UniformGridDescriptor
    end function hdf5_handler_GetUniformGridDescriptor


    function hdf5_handler_GetSpatialGridDescriptor(this) result(SpatialGridDescriptor)
    !-----------------------------------------------------------------
    !< Return a pointer to the SpatialGridDescriptor
    !----------------------------------------------------------------- 
        class(hdf5_handler_t),         intent(IN) :: this                  !< HDF5 handler type
        class(spatial_grid_descriptor_t), pointer :: SpatialGridDescriptor !< Uniform grid descriptor
    !----------------------------------------------------------------- 
        nullify(SpatialGridDescriptor)
        SpatialGridDescriptor => this%SpatialGridDescriptor
    end function hdf5_handler_GetSpatialGridDescriptor


    function hdf5_handler_GetMPIEnvironment(this) result(MPIEnvironment)
    !-----------------------------------------------------------------
    !< Return a pointer to the MPIEnvironment
    !----------------------------------------------------------------- 
        class(hdf5_handler_t), intent(IN) :: this                     !< HDF5 handler type
        class(mpi_env_t), pointer         :: MPIEnvironment           !< MPI Environment
    !----------------------------------------------------------------- 
        nullify(MPIEnvironment)
        MPIEnvironment => this%MPIEnvironment
    end function hdf5_handler_GetMPIEnvironment


    subroutine hdf5_handler_OpenFile(this, action, fileprefix)
    !-----------------------------------------------------------------
    !< Open a HDF5 file
    !----------------------------------------------------------------- 
        class(hdf5_handler_t), intent(INOUT) :: this                  !< HDF5 handler type
        integer(I4P),          intent(IN)    :: action                !< Action to be perfomed (Read or Write)
        character(len=*),      intent(IN)    :: fileprefix            !< HDF5 file prefix
        integer                              :: hdferror              !< HDF5 error code
        integer(HID_T)                       :: plist_id              !< HDF5 property list identifier 
        character(len=:), allocatable        :: HDF5FileName          !< Name of the HDF5 file
    !-----------------------------------------------------------------
        assert(this%State > HDF5_HANDLER_STATE_START) ! Was initialized
#ifdef ENABLE_HDF5
        this%action = action
        HDF5Filename = trim(adjustl(fileprefix))//'_'//trim(adjustl(str(no_sign=.true., n=this%StepsHandler%GetCurrentStep())))//HDF5_EXT
        if(this%State == HDF5_HANDLER_STATE_OPEN) call this%CloseFile()
        call H5open_f(error=hdferror) 
        call H5pcreate_f(H5P_FILE_ACCESS_F, prp_id=plist_id, hdferr=hdferror)
#ifdef ENABLE_MPI
        call H5pset_fapl_mpio_f(prp_id = plist_id, &
                        comm   = this%MPIEnvironment%get_comm(), &
                        info   = this%MPIEnvironment%get_info(), &
                        hdferr = hdferror)
#endif

        select case(this%action)
            case(XDMF_ACTION_WRITE)
                ! If file already exists, file is opened with read-write 
                ! access and new data overwrites existing data, destroying 
                ! all prior content, i.e., file content is truncated upon
                ! opening. 
                ! If file does not exist, it is created and opened with 
                ! read-write access.
                call H5fcreate_f(name = HDF5FileName,                 &
                        access_flags  = H5F_ACC_TRUNC_F,              &
                        File_id       = this%FileID,                  &
                        hdferr        = hdferror,                     &
                        creation_prp  = H5P_DEFAULT_F,                &
                        access_prp    = plist_id)
            case(XDMF_ACTION_READ)
                ! Existing file is opened with read-only access. If file 
                ! does not exist, H5Fopen fails.
                call H5fopen_f(name  = HDF5FileName,                  &
                        access_flags = H5F_ACC_RDONLY_F,              &
                        File_id      = this%FileID,                   &
                        hdferr       = hdferror,                      &
                        access_prp   = plist_id)
        end select

        call h5pclose_f(prp_id = plist_id, hdferr = hdferror)

#endif
        this%State = HDF5_HANDLER_STATE_OPEN
    end subroutine hdf5_handler_OpenFile


    subroutine hdf5_handler_CloseFile(this)
    !-----------------------------------------------------------------
    !< Close a HDF5 file
    !----------------------------------------------------------------- 
        class(hdf5_handler_t), intent(INOUT) :: this                  !< HDF5 handler type
        integer                              :: hdferror              !< HDF5 error code
    !-----------------------------------------------------------------
        assert(this%State == HDF5_HANDLER_STATE_OPEN) ! Was initialized
#ifdef ENABLE_HDF5
        call H5Fclose_f(file_id = this%FileID, hdferr = hdferror)
        call H5close_f(error = hdferror) 
#endif
        this%State = HDF5_HANDLER_STATE_CLOSE
    end subroutine hdf5_handler_CloseFile

end module hdf5_handler
