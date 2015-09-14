module xdmf_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use xh5for_parameters
use IR_Precision, only : I4P, I8P, R4P, R8P, str
use xdmf_utils,   only : warning_message
use fox_xdmf
use distributed_data_handler

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
        type(distributed_data_handler_t), pointer:: DistributedDataHandler
        type(xdmf_local_grid_info_t)             :: grid_info      !< Local grid info
        type(xdmf_file_t)                        :: file           !< XDMF file handler
        logical                                  :: warn = .true.  !< Flag to show warnings on screen
    contains
    private
        procedure         :: is_valid_TopologyType => xdmf_handler_is_valid_TopologyType
        procedure         :: is_valid_GeometryType => xdmf_handler_is_valid_GeometryType
        procedure, public :: initialize            => xdmf_handler_initialize
        procedure, public :: SetNumberOfNodes      => xdmf_handler_SetNumberOfNodes
        procedure, public :: SetNumberOfElements   => xdmf_handler_SetNumberOfElements
        procedure, public :: SetTopologyType       => xdmf_handler_SetTopologyType
        procedure, public :: SetGeometryType       => xdmf_handler_SetGeometryType
        procedure, public :: GetNumberOfNodes      => xdmf_handler_GetNumberOfNodes
        procedure, public :: GetNumberOfElements   => xdmf_handler_GetNumberOfElements
        procedure, public :: GetTopologyType       => xdmf_handler_GetTopologyType
        procedure, public :: GetGeometryType       => xdmf_handler_GetGeometryType
        procedure, public :: OpenFile              => xdmf_handler_OpenFile
        procedure, public :: CloseFile             => xdmf_handler_CloseFile
        procedure, public :: is_root               => xdmf_handler_is_root
    end type xdmf_handler_t

public :: xdmf_handler_t

contains

    subroutine xdmf_handler_initialize(this, DistributedDataHandler, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
        class(xdmf_handler_t), intent(INOUT) :: this
        type(distributed_data_handler_t), target, intent(IN) :: DistributedDataHandler
        integer(I8P),  intent(IN)    :: NumberOfNodes
        integer(I8P),  intent(IN)    :: NumberOfElements
        integer(I4P),  intent(IN)    :: TopologyType
        integer(I4P),  intent(IN)    :: GeometryType

        this%DistributedDataHandler => DistributedDataHandler
        call this%SetNumberOfNodes(NumberOfNodes)
        call this%SetNumberOfElements(NumberOfelements)
        call this%SetTopologyType(TopologyType)
        call this%SetGeometryType(GeometryType)
    end subroutine xdmf_handler_initialize

    subroutine xdmf_handler_SetNumberOfNodes(this, NumberOfNodes)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I8P),          intent(IN)    :: NumberOfNodes
        this%grid_info%NumberOfNodes = NumberOfNodes
    end subroutine xdmf_handler_SetNumberOfNodes

    subroutine xdmf_handler_SetNumberOfElements(this, NumberOfElements)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I8P),          intent(IN)    :: NumberOfElements
        this%grid_info%NumberOfElements = NumberOfElements
    end subroutine xdmf_handler_SetNumberOfElements

    function xdmf_handler_GetNumberOfNodes(this)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I8P)                         :: xdmf_handler_getNumberOfNodes
        xdmf_handler_GetNumberOfNodes = this%grid_info%NumberOfNodes
    end function xdmf_handler_GetNumberOfNodes

    function xdmf_handler_GetNumberOfElements(this)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I8P)                         :: xdmf_handler_GetNumberOfElements
        xdmf_handler_GetNumberOfElements = this%grid_info%NumberOfElements
    end function xdmf_handler_GetNumberOfElements

    function xdmf_handler_is_valid_TopologyType(this, TopologyType) result(is_valid)
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
    end function xdmf_handler_is_valid_TopologyType

    subroutine xdmf_handler_SetTopologyType(this, TopologyType)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I4P),          intent(IN)    :: TopologyType

        if(this%is_valid_TopologyType(TopologyType)) then
            this%grid_info%TopologyType = TopologyType
        else
            this%grid_info%TopologyType = XDMF_NO_VALUE
        endif
    end subroutine xdmf_handler_SetTopologyType

    function xdmf_handler_GetTopologyType(this)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I4P)                         :: xdmf_handler_GetTopologyType
        xdmf_handler_GetTopologyType = this%grid_info%TopologyType
    end function xdmf_handler_GetTopologyType

    function xdmf_handler_is_valid_GeometryType(this, GeometryType) result(is_valid)
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
    end function xdmf_handler_is_valid_GeometryType

    subroutine xdmf_handler_SetGeometryType(this, GeometryType)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I4P),          intent(IN)    :: GeometryType

        if(this%is_valid_GeometryType(GeometryType)) then
            this%grid_info%GeometryType = GeometryType
        else
            this%grid_info%GeometryType = XDMF_NO_VALUE
        endif
    end subroutine xdmf_handler_SetGeometryType

    function xdmf_handler_GetGeometryType(this)
        class(xdmf_handler_t), intent(INOUT) :: this
        integer(I4P)                         :: xdmf_handler_GetGeometryType
        xdmf_handler_GetGeometryType = this%grid_info%GeometryType
    end function xdmf_handler_GetGeometryType

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
        xdmf_handler_is_root = this%DistributedDataHandler%is_root()
    end function xdmf_handler_is_root

end module xdmf_handler
