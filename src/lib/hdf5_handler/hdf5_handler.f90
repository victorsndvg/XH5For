module hdf5_handler

#ifdef ENABLE_HDF5
use HDF5
#else
use IR_Precision, only : I4P
#endif
use mpi_environment
use xh5for_parameters
use spatial_grid_descriptor
use uniform_grid_descriptor

implicit none

private

#ifndef ENABLE_HDF5
    integer, parameter :: HID_T = I4P
    integer, parameter :: HSIZE_T = I4P
#endif

    type, abstract :: hdf5_handler_t
    !-----------------------------------------------------------------
    !< HDF5 abstract handler
    !----------------------------------------------------------------- 
        character(len=:),             allocatable :: prefix                          !< Name prefix of the HDF5 file
        character(len=3)                          :: ext = '.h5'                     !< HDF5 file extension
        integer(HID_T)                            :: file_id                         !< File identifier 
        integer(I4P)                              :: action                          !< HDF5 action to be perfomed (Read or Write)
        type(mpi_env_t),                  pointer :: MPIEnvironment        => null() !< MPI environment 
        class(spatial_grid_descriptor_t), pointer :: SpatialGridDescriptor => null() !< Spatial grid descriptor
        class(uniform_grid_descriptor_t), pointer :: UniformGridDescriptor => null() !< Uniform grid descriptor
    contains
        procedure(hdf5_handler_WriteGeometry_R4P),  deferred :: WriteGeometry_R4P
        procedure(hdf5_handler_WriteGeometry_R8P),  deferred :: WriteGeometry_R8P
        procedure(hdf5_handler_ReadGeometry_R4P),   deferred :: ReadGeometry_R4P
        procedure(hdf5_handler_ReadGeometry_R8P),   deferred :: ReadGeometry_R8P
        procedure(hdf5_handler_WriteTopology_I4P),  deferred :: WriteTopology_I4P
        procedure(hdf5_handler_WriteTopology_I8P),  deferred :: WriteTopology_I8P
        procedure(hdf5_handler_ReadTopology_I4P),   deferred :: ReadTopology_I4P
        procedure(hdf5_handler_ReadTopology_I8P),   deferred :: ReadTopology_I8P
        procedure(hdf5_handler_WriteAttribute_I4P), deferred :: WriteAttribute_I4P
        procedure(hdf5_handler_WriteAttribute_I8P), deferred :: WriteAttribute_I8P
        procedure(hdf5_handler_WriteAttribute_R4P), deferred :: WriteAttribute_R4P
        procedure(hdf5_handler_WriteAttribute_R8P), deferred :: WriteAttribute_R8P
        procedure(hdf5_handler_ReadAttribute_I4P),  deferred :: ReadAttribute_I4P
        procedure(hdf5_handler_ReadAttribute_I8P),  deferred :: ReadAttribute_I8P
        procedure(hdf5_handler_ReadAttribute_R4P),  deferred :: ReadAttribute_R4P
        procedure(hdf5_handler_ReadAttribute_R8P),  deferred :: ReadAttribute_R8P
        procedure                                            :: Initialize => hdf5_handler_Initialize
        procedure                                            :: Free       => hdf5_handler_Free
        procedure                                            :: OpenFile   => hdf5_handler_OpenFile
        procedure                                            :: CloseFile  => hdf5_handler_CloseFile
        generic,                                    public   :: WriteTopology  => WriteTopology_I4P, &
                                                                                  WriteTopology_I8P
        generic,                                    public   :: ReadTopology   => ReadTopology_I4P, &
                                                                                  ReadTopology_I8P
        generic,                                    public   :: WriteGeometry  => WriteGeometry_R4P, &
                                                                                  WriteGeometry_R8P
        generic,                                    public   :: ReadGeometry   => ReadGeometry_R4P, &
                                                                                  ReadGeometry_R8P
        generic,                                    public   :: WriteAttribute => WriteAttribute_I4P, &
                                                                                  WriteAttribute_I8P, &
                                                                                  WriteAttribute_R4P, &
                                                                                  WriteAttribute_R8P
        generic,                                    public   :: ReadAttribute  => ReadAttribute_I4P, &
                                                                                  ReadAttribute_I8P, &
                                                                                  ReadAttribute_R4P, &
                                                                                  ReadAttribute_R8P

    end type hdf5_handler_t

    abstract interface
        subroutine hdf5_handler_WriteGeometry_R4P(this, Coordinates, Name)
            import hdf5_handler_t
            import R4P
            class(hdf5_handler_t), intent(IN) :: this
            real(R4P),             intent(IN) :: Coordinates(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteGeometry_R4P

        subroutine hdf5_handler_WriteGeometry_R8P(this, Coordinates, Name)
            import hdf5_handler_t
            import R8P
            class(hdf5_handler_t), intent(IN) :: this
            real(R8P),             intent(IN) :: Coordinates(:)
            character(len=*),      intent(IN) :: Name
        end subroutine hdf5_handler_WriteGeometry_R8P

        subroutine hdf5_handler_ReadGeometry_R4P(this, Coordinates, Name)
            import hdf5_handler_t
            import R4P
            class(hdf5_handler_t),  intent(IN)  :: this
            real(R4P), allocatable, intent(OUT) :: Coordinates(:)
            character(len=*),      intent(IN)   :: Name
        end subroutine hdf5_handler_ReadGeometry_R4P

        subroutine hdf5_handler_ReadGeometry_R8P(this, Coordinates, Name)
            import hdf5_handler_t
            import R8P
            class(hdf5_handler_t),  intent(IN)  :: this
            real(R8P), allocatable, intent(OUT) :: Coordinates(:)
            character(len=*),       intent(IN)   :: Name
        end subroutine hdf5_handler_ReadGeometry_R8P

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


    subroutine hdf5_handler_Initialize(this, MPIEnvironment, UniformGridDescriptor, SpatialGridDescriptor)
    !-----------------------------------------------------------------
    !< Initialize the HDF5 handler
    !----------------------------------------------------------------- 
        class(hdf5_handler_t),                    intent(INOUT) :: this                  !< HDF5 handler type
        type(mpi_env_t),                  target, intent(IN)    :: MPIEnvironment        !< MPI environment
        class(uniform_grid_descriptor_t), target, intent(IN)    :: UniformGridDescriptor !< Uniform grid descriptor 
        class(spatial_grid_descriptor_t), target, intent(IN)    :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        this%MPIEnvironment        => MPIEnvironment
        this%SpatialGridDescriptor => SpatialGridDescriptor
        this%UniformGridDescriptor => UniformGridDescriptor
    end subroutine hdf5_handler_Initialize


    subroutine hdf5_handler_Free(this)
    !-----------------------------------------------------------------
    !< HDF5 handler free
    !----------------------------------------------------------------- 
        class(hdf5_handler_t),  intent(INOUT) :: this                 !< HDF5 handler type
    !----------------------------------------------------------------- 
        nullify(this%MPIEnvironment)
        nullify(this%UniformGridDescriptor)
        nullify(this%SpatialGridDescriptor)
    end subroutine hdf5_handler_Free


    subroutine hdf5_handler_OpenFile(this, action, fileprefix)
    !-----------------------------------------------------------------
    !< Open a HDF5 file
    !----------------------------------------------------------------- 
        class(hdf5_handler_t), intent(INOUT) :: this                  !< HDF5 handler type
        integer(I4P),          intent(IN)    :: action                !< Action to be perfomed (Read or Write)
        character(len=*),      intent(IN)    :: fileprefix            !< HDF5 file prefix
        integer                              :: hdferror              !< HDF5 error code
        integer(HID_T)                       :: plist_id              !< HDF5 property list identifier 
    !-----------------------------------------------------------------
#ifdef ENABLE_HDF5

        this%action = action

        call H5open_f(error=hdferror) 
        call H5pcreate_f(H5P_FILE_ACCESS_F, prp_id=plist_id, hdferr=hdferror)
        call H5pset_fapl_mpio_f(prp_id = plist_id, &
                        comm   = this%MPIEnvironment%get_comm(), &
                        info   = this%MPIEnvironment%get_info(), &
                        hdferr = hdferror)

        select case(this%action)
            case(XDMF_ACTION_WRITE)
                ! If file already exists, file is opened with read-write 
                ! access and new data overwrites existing data, destroying 
                ! all prior content, i.e., file content is truncated upon
                ! opening. 
                ! If file does not exist, it is created and opened with 
                ! read-write access.
                call H5fcreate_f(name = trim(adjustl(fileprefix))//this%ext, &
                        access_flags = H5F_ACC_TRUNC_F,              &
                        file_id      = this%file_id,                 &
                        hdferr       = hdferror,                     &
                        creation_prp = H5P_DEFAULT_F,                &
                        access_prp   = plist_id)
            case(XDMF_ACTION_READ)
                ! Existing file is opened with read-only access. If file 
                ! does not exist, H5Fopen fails.
                call H5fopen_f(name = trim(adjustl(fileprefix))//this%ext, &
                        access_flags = H5F_ACC_RDONLY_F,             &
                        file_id      = this%file_id,                 &
                        hdferr       = hdferror,                     &
                        access_prp   = plist_id)
        end select

        call h5pclose_f(prp_id = plist_id, hdferr = hdferror)

#endif
    end subroutine hdf5_handler_OpenFile


    subroutine hdf5_handler_CloseFile(this)
    !-----------------------------------------------------------------
    !< Close a HDF5 file
    !----------------------------------------------------------------- 
        class(hdf5_handler_t), intent(INOUT) :: this                  !< HDF5 handler type
        integer                              :: hdferror              !< HDF5 error code
    !-----------------------------------------------------------------
#ifdef ENABLE_HDF5
        call h5fclose_f(file_id = this%file_id, hdferr = hdferror)
        call h5close_f(error = hdferror) 
#endif
    end subroutine hdf5_handler_CloseFile

end module hdf5_handler
