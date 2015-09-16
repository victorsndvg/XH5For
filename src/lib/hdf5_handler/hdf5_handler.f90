module hdf5_handler

#ifdef ENABLE_HDF5
use HDF5
#else
use IR_Precision, only : I4P
#endif
use mpi_environment
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
        character(len=:),            allocatable :: prefix                !< Name prefix of the HDF5 file
        integer(HID_T)                           :: file_id               ! File identifier 
        type(mpi_env_t),                 pointer :: MPIEnvironment        !< MPI environment 
        type(spatial_grid_descriptor_t), pointer :: SpatialGridDescriptor !< Spatial grid descriptor
        type(uniform_grid_descriptor_t), pointer :: UniformGridDescriptor !< Uniform grid descriptor
    contains
        procedure :: Initialize => hdf5_handler_Initialize
        procedure :: OpenFile   => hdf5_handler_OpenFile
        procedure :: CloseFile  => hdf5_handler_CloseFile
    end type hdf5_handler_t

public :: hdf5_handler_t

#ifndef ENABLE_HDF5
public :: HID_T
public :: HSIZE_T
#endif

contains


    subroutine hdf5_handler_Initialize(this, MPIEnvironment, SpatialGridDescriptor, UniformGridDescriptor)
    !-----------------------------------------------------------------
    !< Initialize the HDF5 handler
    !----------------------------------------------------------------- 
        class(hdf5_handler_t),                   intent(INOUT) :: this                  !< HDF5 handler type
        type(mpi_env_t),                 target, intent(IN)    :: MPIEnvironment        !< MPI environment 
        type(spatial_grid_descriptor_t), target, intent(IN)    :: SpatialGridDescriptor !< Spatial grid descriptor
        type(uniform_grid_descriptor_t), target, intent(IN)    :: UniformGridDescriptor !< Uniform grid descriptor
    !-----------------------------------------------------------------
        this%MPIEnvironment        => MPIEnvironment
        this%SpatialGridDescriptor => SpatialGridDescriptor
        this%UniformGridDescriptor => UniformGridDescriptor
    end subroutine hdf5_handler_Initialize


    subroutine hdf5_handler_OpenFile(this)
    !-----------------------------------------------------------------
    !< Open a HDF5 file
    !----------------------------------------------------------------- 
        class(hdf5_handler_t), intent(INOUT) :: this                  !< HDF5 handler type
        integer                              :: hdferror              !< HDF5 error code
        integer(HID_T)                       :: plist_id              !< HDF5 property list identifier 
    !-----------------------------------------------------------------
#ifdef ENABLE_HDF5
        call H5open_f(error=hdferror) 
        call H5pcreate_f(H5P_FILE_ACCESS_F, prp_id=plist_id, hdferr=hdferror)
        call H5pset_fapl_mpio_f(prp_id = plist_id, &
                        comm   = this%MPIEnvironment%get_comm(), &
                        info   = this%MPIEnvironment%get_info(), &
                        hdferr = hdferror)
        call H5fcreate_f(name = this%prefix//'.h5', &
                        access_flags = H5F_ACC_TRUNC_F, &
                        file_id      = this%file_id, &
                        hdferr       = hdferror, &
                        creation_prp = H5P_DEFAULT_F, &
                        access_prp   = plist_id)
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
