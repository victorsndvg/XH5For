module xdmf_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use xh5for_parameters
use IR_Precision, only : I4P, I8P, R4P, R8P, str
use fox_xdmf
use spatial_grid_descriptor
use uniform_grid_descriptor

implicit none

private

    type, abstract :: xdmf_handler_t
    !-----------------------------------------------------------------
    !< XDMF handler abstract type
    !----------------------------------------------------------------- 
        character(len=:),            allocatable :: prefix         !< Name prefix of the XDMF file
        type(spatial_grid_descriptor_t), pointer :: SpatialGridDescriptor
        type(uniform_grid_descriptor_t), pointer :: UniformGridDescriptor    !< Local grid info
        type(xdmf_file_t)                        :: file           !< XDMF file handler
        logical                                  :: warn = .true.  !< Flag to show warnings on screen
    contains
    private
        procedure, public :: initialize            => xdmf_handler_initialize
        procedure, public :: OpenFile              => xdmf_handler_OpenFile
        procedure, public :: CloseFile             => xdmf_handler_CloseFile
        procedure, public :: is_root               => xdmf_handler_is_root
    end type xdmf_handler_t

public :: xdmf_handler_t

contains

    subroutine xdmf_handler_initialize(this, SpatialGridDescriptor, UniformGridDescriptor)!, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
        class(xdmf_handler_t), intent(INOUT) :: this
        type(spatial_grid_descriptor_t), target, intent(IN) :: SpatialGridDescriptor
        type(uniform_grid_descriptor_t), target, intent(IN) :: UniformGridDescriptor
!        integer(I8P),  intent(IN)    :: NumberOfNodes
!        integer(I8P),  intent(IN)    :: NumberOfElements
!        integer(I4P),  intent(IN)    :: TopologyType
!        integer(I4P),  intent(IN)    :: GeometryType

        this%SpatialGridDescriptor => SpatialGridDescriptor
        this%UniformGridDescriptor => UniformGridDescriptor
!        call this%SetNumberOfNodes(NumberOfNodes)
!        call this%SetNumberOfElements(NumberOfelements)
!        call this%SetTopologyType(TopologyType)
!        call this%SetGeometryType(GeometryType)
    end subroutine xdmf_handler_initialize

    subroutine xdmf_handler_OpenFile(this, filename)
        class(xdmf_handler_t), intent(INOUT) :: this
        character(len=*),      intent(IN)    :: filename
        if(this%is_root()) then
            call this%file%set_filename(filename)
            call this%file%openfile()
        endif
    end subroutine xdmf_handler_OpenFile

    subroutine xdmf_handler_CloseFile(this)
        class(xdmf_handler_t), intent(INOUT)    :: this
        if(this%is_root()) then
            call this%file%closefile()
        endif
    end subroutine xdmf_handler_CloseFile


    function xdmf_handler_is_root(this)
    !-----------------------------------------------------------------
    !< Is the current task the root processor?
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(IN)  :: this                !< Distributed data type
        logical                            :: xdmf_handler_is_root !< Boolean variable, True if is root task   
    !----------------------------------------------------------------- 
        xdmf_handler_is_root = this%SpatialGridDescriptor%is_root()
    end function xdmf_handler_is_root

end module xdmf_handler
