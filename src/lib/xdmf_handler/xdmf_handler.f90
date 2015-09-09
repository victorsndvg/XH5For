module xdmf_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use xh5for_parameters
use IR_Precision, only : I4P, I8P, R4P, R8P, str
use xdmf_utils,   only : warning_message
use fox_xdmf
use mpi_environment

implicit none

private

    type :: xdmf_local_grid_info_t
    !-----------------------------------------------------------------
    !< Return MPI root processor
    !----------------------------------------------------------------- 
        integer(I4P) :: GridType           = XDMF_GRID_TYPE_UNSTRUCTURED
        integer(I4P) :: GeometryType       = XDMF_GEOMETRY_TYPE_XYZ
        integer(I4P) :: TopologyType       = XDMF_NO_VALUE
        integer(I8P) :: NumberOfNodes      = XDMF_NO_VALUE
        integer(I8P) :: NumberOfElements   = XDMF_NO_VALUE
    end type xdmf_local_grid_info_t

    type, abstract :: xdmf_handler_t
    !-----------------------------------------------------------------
    !< XDMF handler abstract type
    !----------------------------------------------------------------- 
        character(len=:),            allocatable :: prefix         !< Name prefix of the XDMF file
        type(xdmf_local_grid_info_t)             :: grid_info      !< Local grid info
        type(mpi_env_t)                          :: mpi_env        !< MPI environment 
        type(xdmf_file_t)                        :: file           !< XDMF file handler
        logical                                  :: warn = .true.  !< Flag to show warnings on screen
    contains
    private
        procedure         :: is_valid_TopologyType => handler_is_valid_TopologyType
        procedure         :: is_valid_GeometryType => handler_is_valid_GeometryType
        procedure, public :: SetNumberOfNodes      => handler_SetNumberOfNodes
        procedure, public :: SetNumberOfElements   => handler_SetNumberOfElements
        procedure, public :: SetTopologyType       => handler_SetTopologyType
        procedure, public :: SetGeometryType       => handler_SetGeometryType
        procedure, public :: GetNumberOfNodes      => handler_GetNumberOfNodes
        procedure, public :: GetNumberOfElements   => handler_GetNumberOfElements
        procedure, public :: GetTopologyType       => handler_GetTopologyType
        procedure, public :: GetGeometryType       => handler_GetGeometryType
        procedure, public :: OpenFile              => handler_OpenFile
        procedure, public :: CloseFile             => handler_CloseFile
    end type xdmf_handler_t

public :: xdmf_handler_t

contains

    subroutine handler_SetNumberOfNodes(this, NumberOfNodes)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I8P),          intent(IN)    :: NumberOfNodes
        this%grid_info%NumberOfNodes = NumberOfNodes
    end subroutine handler_SetNumberOfNodes

    subroutine handler_SetNumberOfElements(this, NumberOfElements)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I8P),          intent(IN)    :: NumberOfElements
        this%grid_info%NumberOfElements = NumberOfElements
    end subroutine handler_SetNumberOfElements

    function handler_GetNumberOfNodes(this)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I8P)                         :: handler_getNumberOfNodes
        handler_GetNumberOfNodes = this%grid_info%NumberOfNodes
    end function handler_GetNumberOfNodes

    function handler_GetNumberOfElements(this)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I8P)                         :: handler_GetNumberOfElements
        handler_GetNumberOfElements = this%grid_info%NumberOfElements
    end function handler_GetNumberOfElements

    function handler_is_valid_TopologyType(this, TopologyType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem NumberType
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),  intent(IN) :: this                    !< XDMF DataItem type
        integer(I4P),           intent(IN) :: TopologyType            !< XDMF Topology Type
        logical                            :: is_valid                !< Valid Topology Type confirmation flag
        integer(I4P), allocatable          :: allowed_TopologyTypes(:)!< Dataitem Topology Type list 
    !----------------------------------------------------------------- 
        !< @Note: Mixed topologies or variable number of node elements not allowed
        !< @Note: mixed topologies can be implemented with and array of celltypes (offset?)
        !< @Note: variable number of nodes can be implemented with and array of number of nodes (offset?)
        allowed_TopologyTypes = (/ &
!                                XDMF_TOPOLOGY_TYPE_POLYVERTEX,      &
!                                XDMF_TOPOLOGY_TYPE_POLYLINE,        &
!                                XDMF_TOPOLOGY_TYPE_POLYGON,         &
                                XDMF_TOPOLOGY_TYPE_TRIANGLE,        &
                                XDMF_TOPOLOGY_TYPE_QUADRILATERAL,   &
                                XDMF_TOPOLOGY_TYPE_TETRAHEDRON,     &
                                XDMF_TOPOLOGY_TYPE_PYRAMID,         &
                                XDMF_TOPOLOGY_TYPE_WEDGE,           &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON,      &
                                XDMF_TOPOLOGY_TYPE_EDGE_3,          &
                                XDMF_TOPOLOGY_TYPE_TRIANGLE_6,      &
                                XDMF_TOPOLOGY_TYPE_QUADRILATERAL_8, &
                                XDMF_TOPOLOGY_TYPE_QUADRILATERAL_9, &
                                XDMF_TOPOLOGY_TYPE_TETRAHEDRON_10,  &
                                XDMF_TOPOLOGY_TYPE_PYRAMID_13,      &
                                XDMF_TOPOLOGY_TYPE_WEDGE_15,        &
                                XDMF_TOPOLOGY_TYPE_WEDGE_18,        &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_20,   &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_24,   &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_27,   &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_64,   &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_125,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_216,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_343,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_512,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_729,  &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1000, &
                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1331 &
!                                XDMF_TOPOLOGY_TYPE_MIXED            &
                                /)
        is_valid = MINVAL(ABS(allowed_TopologyTypes - TopologyType)) == 0_I4P
        if(.not. is_valid .and. this%warn) call warning_message('Wrong Topology Type: "'//trim(str(no_sign=.true., n=TopologyType))//'"')
    end function handler_is_valid_TopologyType

    subroutine handler_SetTopologyType(this, TopologyType)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I4P),          intent(IN)    :: TopologyType

        if(this%is_valid_TopologyType(TopologyType)) then
            this%grid_info%TopologyType = TopologyType
        else
            this%grid_info%TopologyType = XDMF_NO_VALUE
        endif
    end subroutine handler_SetTopologyType

    function handler_GetTopologyType(this)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I4P)                         :: handler_GetTopologyType
        handler_GetTopologyType = this%grid_info%TopologyType
    end function handler_GetTopologyType

    function handler_is_valid_GeometryType(this, GeometryType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem NumberType
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),  intent(IN) :: this                    !< XDMF DataItem type
        integer(I4P),           intent(IN) :: GeometryType            !< XDMF Geometry Type
        logical                            :: is_valid                !< Valid Geometry Type confirmation flag
        integer(I4P), allocatable          :: allowed_GeometryTypes(:)!< Dataitem Geometry Type list 
    !----------------------------------------------------------------- 
        !< @Note: Mixed topologies or variable number of node elements not allowed
        !< @Note: mixed topologies can be implemented with and array of celltypes (offset?)
        !< @Note: variable number of nodes can be implemented with and array of number of nodes (offset?)
        allowed_GeometryTypes = (/ &
                                XDMF_GEOMETRY_TYPE_XYZ, &
                                XDMF_GEOMETRY_TYPE_XY   &
                                /)
        is_valid = MINVAL(ABS(allowed_GeometryTypes - GeometryType)) == 0_I4P
        if(.not. is_valid .and. this%warn) call warning_message('Wrong Geometry Type: "'//trim(str(no_sign=.true., n=GeometryType))//'"')
    end function handler_is_valid_GeometryType

    subroutine handler_SetGeometryType(this, GeometryType)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I4P),          intent(IN)    :: GeometryType

        if(this%is_valid_GeometryType(GeometryType)) then
            this%grid_info%GeometryType = GeometryType
        else
            this%grid_info%GeometryType = XDMF_NO_VALUE
        endif
    end subroutine handler_SetGeometryType

    function handler_GetGeometryType(this)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I4P)                         :: handler_GetGeometryType
        handler_GetGeometryType = this%grid_info%GeometryType
    end function handler_GetGeometryType

    subroutine handler_OpenFile(this, filename)
        class(xdmf_handler_t), intent(INOUT) :: this
        character(len=*),      intent(IN)    :: filename
        if(this%mpi_env%is_root()) then
            call this%file%set_filename(filename)
            call this%file%openfile()
        endif
    end subroutine

    subroutine handler_CloseFile(this)
        class(xdmf_handler_t), intent(INOUT)    :: this
        if(this%mpi_env%is_root()) then
            call this%file%closefile()
        endif
    end subroutine


end module xdmf_handler
