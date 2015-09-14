module distributed_data_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use IR_Precision, only : I4P, I8P, R4P, R8P
use mpi_environment

implicit none

private

    type :: distributed_data_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
    private
        integer(I8P)                             :: GlobalNumberOfNodes
        integer(I8P)                             :: GlobalNumberOfElements
        integer(I8P), allocatable                :: AllNumberOfNodes(:)
        integer(I8P), allocatable                :: AllNumberOfElements(:)
        integer(I4P), allocatable                :: AllTopologyTypes(:)
        integer(I4P), allocatable                :: AllGeometryTypes(:)
        type(mpi_env_t)                          :: mpi_env                 !< MPI environment 
    contains
    private
        procedure         :: SetGlobalNumberOfNodes       => distributed_data_SetGlobalNumberOfNodes
        procedure         :: SetGlobalNumberOfElements    => distributed_data_SetGlobalNumberOfElements
        procedure, public :: GetGlobalNumberOfNodes       => distributed_data_GetGlobalNumberOfNodes
        procedure, public :: GetGlobalNumberOfElements    => distributed_data_GetGlobalNumberOfElements
        procedure, public :: GetNumberOfNodesFromTask     => distributed_data_GetNumberOfNodesFromTask
        procedure, public :: GetNumberOfElementsFromTask => distributed_data_GetNumberOfElementsFromTask
        procedure, public :: GetTopologyTypeFromTask      => distributed_data_GetTopologyTypeFromTask
        procedure, public :: GetGeometryTypeFromTask      => distributed_data_GetGeometryTypeFromTask
        procedure, public :: initialize                   => distributed_data_initialize
        procedure, public :: is_root                      => distributed_data_is_root
        procedure, public :: get_comm_size                => distributed_data_get_comm_size
    end type distributed_data_handler_t

public :: distributed_data_handler_t

contains

    subroutine distributed_data_SetGlobalNumberOfNodes(this, GlobalNumberOfNodes)
        class(distributed_data_handler_t), intent(INOUT) :: this
        integer(I8P)                              , intent(IN)    :: GlobalNumberOfNodes

        this%GlobalNumberOfNodes = GlobalNumberOfNodes
    end subroutine distributed_data_SetGlobalNumberOfNodes

    function distributed_data_GetGlobalNumberOfNodes(this)
        class(distributed_data_handler_t), intent(INOUT) :: this
        integer(I8P)                         :: distributed_data_GetGlobalNumberOfNodes

        distributed_data_GetGlobalNumberOfNodes = this%GlobalNumberOfNodes
    end function distributed_data_GetGlobalNumberOfNodes

    subroutine distributed_data_SetGlobalNumberOfElements(this, GlobalNumberOfElements)
        class(distributed_data_handler_t), intent(INOUT) :: this
        integer(I8P)                              , intent(IN)    :: GlobalNumberOfElements

        this%GlobalNumberOfElements = GlobalNumberOfelements
    end subroutine distributed_data_SetGlobalNumberOfElements

    Function distributed_data_GetGlobalNumberOfElements(this)
        class(distributed_data_handler_t), intent(INOUT) :: this
        integer(I8P)                         :: distributed_data_GetGlobalNumberOfElements

        distributed_data_GetGlobalNumberOfelements = this%GlobalNumberOfElements
    end function distributed_data_GetGlobalNumberOfElements

    function distributed_data_GetNumberOfNodesFromTask(this, TaskID)
        class(distributed_data_handler_t), intent(INOUT) :: this
        integer(I4P),                      intent(IN)    :: TaskID
        integer(I8P)                         :: distributed_data_GetNumberOfNodesFromTask

        distributed_data_GetNumberOfNodesFromTask = this%AllNumberOfNodes(TaskID)
    end function distributed_data_GetNumberOfNodesFromTask

    function distributed_data_GetNumberOfElementsFromTask(this, TaskID)
        class(distributed_data_handler_t), intent(INOUT) :: this
        integer(I4P),                      intent(IN)    :: TaskID
        integer(I8P)                         :: distributed_data_GetNumberOfElementsFromTask

        distributed_data_GetNumberOfElementsFromTask = this%AllNumberOfElements(TaskID)
    end function distributed_data_GetNumberOfElementsFromTask

    function distributed_data_GetTopologyTypeFromTask(this, TaskID)
        class(distributed_data_handler_t), intent(INOUT) :: this
        integer(I4P),                      intent(IN)    :: TaskID
        integer(I4P)                         :: distributed_data_GetTopologyTypeFromTask

        distributed_data_GetTopologyTypeFromTask = this%AllTopologyTypes(TaskID)
    end function distributed_data_GetTopologyTypeFromTask

    function distributed_data_GetGeometryTypeFromTask(this, TaskID)
        class(distributed_data_handler_t), intent(INOUT) :: this
        integer(I4P),                      intent(IN)    :: TaskID
        integer(I4P)                         :: distributed_data_GetGeometrytypeFromTask

        distributed_data_GetGeometryTypeFromTask = this%AllGeometryTypes(TaskID)
    end function distributed_data_GetGeometryTypeFromTask

    subroutine distributed_data_initialize(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
        class(distributed_data_handler_t), intent(INOUT) :: this
        integer(I8P),  intent(IN)    :: NumberOfNodes
        integer(I8P),  intent(IN)    :: NumberOfElements
        integer(I4P),  intent(IN)    :: TopologyType
        integer(I4P),  intent(IN)    :: GeometryType

        call this%mpi_env%initialize()
        call this%mpi_env%mpi_allgather_single_int_value(NumberOfNodes, this%AllNumberOfNodes)
        call this%mpi_env%mpi_allgather_single_int_value(NumberOfElements, this%AllNumberOfElements)
        call this%mpi_env%mpi_allgather_single_int_value(TopologyType, this%AllTopologyTypes)
        call this%mpi_env%mpi_allgather_single_int_value(GeometryType, this%AllGeometryTypes)

        if(this%mpi_env%is_root()) then
            call this%SetGlobalNumberOfElements(sum(this%AllNumberOfElements))
            call this%SetGlobalNumberOfNodes(sum(this%AllNumberOfNodes))
        endif

    end subroutine distributed_data_initialize

    function distributed_data_is_root(this)
    !-----------------------------------------------------------------
    !< Is the current task the root processor?
    !----------------------------------------------------------------- 
        class(distributed_data_handler_t), intent(IN)  :: this                !< Distributed data type
        logical                                :: distributed_data_is_root !< Boolean variable, True if is root task   
    !----------------------------------------------------------------- 
        distributed_data_is_root = this%mpi_env%is_root()
    end function distributed_data_is_root

    function distributed_data_get_comm_size(this)
    !-----------------------------------------------------------------
    !< Is the current task the root processor?
    !----------------------------------------------------------------- 
        class(distributed_data_handler_t), intent(IN)  :: this                !< Distributed data type
        integer(I4P)                      :: distributed_data_get_comm_size
    !----------------------------------------------------------------- 
        distributed_data_get_comm_size = this%mpi_env%get_comm_size()
    end function distributed_data_get_comm_size



end module distributed_data_handler
