module xdmf_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF file handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use xh5for_parameters
use IR_Precision, only : I4P, I8P, R4P, R8P, str
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
    !< @TODO: abstract procedures
        procedure, public :: Initialize     => xdmf_handler_Initialize
        procedure, public :: Free           => xdmf_handler_Free
        procedure, public :: OpenFile       => xdmf_handler_OpenFile
        procedure, public :: CloseFile      => xdmf_handler_CloseFile
        procedure, public :: OpenGrid       => xdmf_handler_OpenGrid
        procedure, public :: CloseGrid      => xdmf_handler_CloseGrid
    end type xdmf_handler_t

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

    subroutine xdmf_handler_OpenGrid(this, GridID)
    !-----------------------------------------------------------------
    !< Open a XDMF grid
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),           intent(INOUT) :: this        !< XDMF contiguous hyperslab handler
        integer(I4P),          optional, intent(IN)    :: GridID      !< Grid ID number
        type(xdmf_grid_t)                              :: grid        !< XDMF Grid type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            call grid%open(xml_handler=this%file%xml_handler, &
                Name='Grid'//trim(adjustl(str(no_sign=.true.,n=GridID))))
        endif
    end subroutine xdmf_handler_OpenGrid

    subroutine xdmf_handler_CloseGrid(this, GridID)
    !-----------------------------------------------------------------
    !< Close a XDMF grid
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),           intent(INOUT) :: this        !< XDMF contiguous hyperslab handler
        integer(I4P),          optional, intent(IN)    :: GridID      !< Grid ID number
        type(xdmf_grid_t)                              :: grid        !< XDMF Grid type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            call grid%Close(xml_handler=this%file%xml_handler)
        endif
    end subroutine xdmf_handler_CloseGrid

end module xdmf_handler
