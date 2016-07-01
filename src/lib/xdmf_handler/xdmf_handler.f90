module xdmf_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF File handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use xh5for_utils
use xh5for_parameters
use IR_Precision, only : I4P, I8P, R4P, R8P, str
use fox_xdmf
use fox_dom,      only: Node, NodeList, GetDocumentElement, Item, GetLength, GetChildNodes, getAttribute, &
                        HasChildNodes, GetElementsByTagName, GetNodeType, GetTagName, Destroy, getTextContent, &
                        TEXT_NODE, DOCUMENT_NODE
use mpi_environment
use steps_handler
use spatial_grid_descriptor
use uniform_grid_descriptor

implicit none

#include "assert.i90"

private

    integer(I4P), parameter :: XDMF_HANDLER_STATE_START      = 0
    integer(I4P), parameter :: XDMF_HANDLER_STATE_INIT       = 1
    integer(I4P), parameter :: XDMF_HANDLER_STATE_SERIALIZED = 2
    integer(I4P), parameter :: XDMF_HANDLER_STATE_PARSED     = 3
    integer(I4P), parameter :: XDMF_HANDLER_STATE_CLOSED     = 4

    !-----------------------------------------------------------------
    ! XDMF_HANDLER State Transition Diagram
    !-----------------------------------------------------------------
    ! - This diagram controls the basic life cycle of the XDMF_HANDLER
    ! - After initialization, calls to almost all procedures are allowed
    ! - The state diagram is really simple. Most of the checks also
    !   realies in the allocation status of the file prefix and in the 
    !   Action status. Writing/Reading operations are mutually exclusive
    !----------------------------------------------------------------- 
    !       INIT STATE      |     ACTION      |      FINAL STATE
    !----------------------------------------------------------------- 
    ! START                 | Free            | START
    ! START                 | Initialize      | INIT
    !----------------------------------------------------------------- 
    ! INIT                  | Free            | START
    ! INIT                  | Initialize      | INIT


    type, abstract :: xdmf_handler_t
    !-----------------------------------------------------------------
    !< XDMF handler abstract type
    !----------------------------------------------------------------- 
        character(len=:),             allocatable :: prefix                          !< Name prefix of the XDMF file
        type(xdmf_file_t)                         :: TemporalFile                    !< XDMF file handler for temporal collections
        type(xdmf_file_t)                         :: SpatialFile                     !< XDMF file handler for spatial collections
        integer(I4P)                              :: Action= XDMF_NO_VALUE           !< XDMF purpose (Read or Write)
        integer(I4P)                              :: State = XDMF_HANDLER_STATE_START!< XDMF purpose (Read or Write)
        type(steps_handler_t),            pointer :: StepsHandler          => null() !< Steps handler
        type(mpi_env_t),                  pointer :: MPIEnvironment        => null() !< MPI environment 
        class(spatial_grid_descriptor_t), pointer :: SpatialGridDescriptor => null() !< Global grid info
        class(uniform_grid_descriptor_t), pointer :: UniformGridDescriptor => null() !< Local grid info
    contains
    private
        ! Deferred procedures
        procedure(xdmf_handler_SetTopology_I4P),         deferred :: SetTopology_I4P
        procedure(xdmf_handler_SetTopology_I8P),         deferred :: SetTopology_I8P
        procedure(xdmf_handler_WriteGeometry),           deferred :: WriteGeometry
        procedure(xdmf_handler_WriteTopology),           deferred :: WriteTopology
        procedure(xdmf_handler_WriteAttributes),         deferred :: WriteAttributes
        procedure(xdmf_handler_FillSpatialGridTopology), deferred :: FillSpatialGridTopology

        ! XDMF Handler initialization/finalization procedures
        procedure, public :: Initialize                   => xdmf_handler_Initialize
        procedure, public :: Open                         => xdmf_handler_Open
        procedure, public :: Free                         => xdmf_handler_Free

        ! File name related functions 
        procedure         :: HasPrefix                    => xdmf_handler_HasPrefix
        procedure, public :: GetHDF5Filename              => xdmf_handler_GetHDF5Filename
        procedure, public :: GetSpatialFilename           => xdmf_handler_GetSpatialFilename

        ! File IO procedures
        procedure, public :: OpenSpatialFile              => xdmf_handler_OpenSpatialFile
        procedure         :: OpenSpatialGrid              => xdmf_handler_OpenSpatialGrid
        procedure, public :: SerializeSpatialFile         => xdmf_handler_SerializeSpatialFile
        procedure, public :: SerializeTemporalFile        => xdmf_handler_SerializeTemporalFile
        procedure, public :: ParseTemporalFile            => xdmf_handler_ParseTemporalFile
        procedure, public :: ParseSpatialFile             => xdmf_handler_ParseSpatialFile
        procedure         :: CloseSpatialGrid             => xdmf_handler_CloseSpatialGrid
        procedure, public :: CloseSpatialFile             => xdmf_handler_CloseSpatialFile

        ! XML DOM aux procedures for parsing XDMF
        procedure, public :: GetUniqueNodeByTag           => xdmf_handler_GetUniqueNodeByTag
        procedure, public :: GetFirstChildByTag           => xdmf_handler_GetFirstChildByTag
        procedure         :: GetDataItemXPath             => xdmf_handler_GetDataItemXPath
        procedure         :: FillSpatialGridGeometry      => xdmf_handler_FillSpatialGridGeometry
        procedure         :: FillSpatialGridDescriptor    => xdmf_handler_FillSpatialGridDescriptor

        ! Metadata storing procedures
        procedure         :: SetGeometry_R4P              => xdmf_handler_SetGeometry_R4P
        procedure         :: SetGeometry_R8P              => xdmf_handler_SetGeometry_R8P
        procedure         :: AppendAttribute_I4P          => xdmf_handler_AppendAttribute_I4P
        procedure         :: AppendAttribute_I8P          => xdmf_handler_AppendAttribute_I8P
        procedure         :: AppendAttribute_R4P          => xdmf_handler_AppendAttribute_R4P
        procedure         :: AppendAttribute_R8P          => xdmf_handler_AppendAttribute_R8P
        procedure, public :: CalculateAttributeDimensions => xdmf_handler_CalculateAttributeDimensions

        generic,   public :: SetGeometry                  => SetGeometry_R4P, &
                                                             SetGeometry_R8P
        generic,   public :: SetTopology                  => SetTopology_I4P, &
                                                             SetTopology_I8P
        generic,   public :: AppendAttribute              => AppendAttribute_I4P, &
                                                             AppendAttribute_I8P, &
                                                             AppendAttribute_R4P, &
                                                             AppendAttribute_R8P
    end type xdmf_handler_t

    abstract interface
        subroutine xdmf_handler_SetTopology_I4P(this, Connectivities, Name)
            import xdmf_handler_t
            import I4P
            class(xdmf_handler_t), intent(INOUT) :: this
            integer(I4P),          intent(IN)    :: Connectivities(:)
            character(len=*),      intent(IN)    :: Name
        end subroutine xdmf_handler_SetTopology_I4P

        subroutine xdmf_handler_SetTopology_I8P(this, Connectivities, Name)
            import xdmf_handler_t
            import I8P
            class(xdmf_handler_t), intent(INOUT) :: this
            integer(I8P),          intent(IN)    :: Connectivities(:)
            character(len=*),      intent(IN)    :: Name
        end subroutine xdmf_handler_SetTopology_I8P

        subroutine xdmf_handler_WriteGeometry(this, GridID)
            import I4P
            import xdmf_handler_t
            class(xdmf_handler_t), intent(INOUT) :: this
            integer(I4P),          intent(IN)    :: GridID
        end subroutine xdmf_handler_WriteGeometry

        subroutine xdmf_handler_WriteTopology(this, GridID)
            import I4P
            import xdmf_handler_t
            class(xdmf_handler_t), intent(INOUT) :: this
            integer(I4P),          intent(IN)    :: GridID
        end subroutine xdmf_handler_WriteTopology

        subroutine xdmf_handler_WriteAttributes(this, GridID)
            import I4P
            import xdmf_handler_t
            class(xdmf_handler_t), intent(INOUT) :: this
            integer(I4P),          intent(IN)    :: GridID
        end subroutine xdmf_handler_WriteAttributes

        subroutine xdmf_handler_FillSpatialGridTopology(this, TopologyNode, ID)
            import I4P
            import Node
            import xdmf_handler_t
            class(xdmf_handler_t), intent(INOUT) :: this
            type(Node), pointer,   intent(IN)    :: TopologyNode
            integer(I4P),          intent(IN)    :: ID
        end subroutine xdmf_handler_FillSpatialGridTopology
    end interface


public :: xdmf_handler_t

contains

    subroutine xdmf_handler_Initialize(this, MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor, Fileprefix, Action)
    !-----------------------------------------------------------------
    !< XDMF file handler initialization procedure
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),                    intent(INOUT) :: this                  !< XMDF handler
        type(mpi_env_t),                  target, intent(IN)    :: MPIEnvironment        !< MPI environment
        type(steps_handler_t),            target, intent(IN)    :: StepsHandler          !< Steps handler
        class(uniform_grid_descriptor_t), target, intent(IN)    :: UniformGridDescriptor !< Local grid info
        class(spatial_grid_descriptor_t), target, intent(IN)    :: SpatialGridDescriptor !< Global grid info
        character(len=*), optional,               intent(IN)    :: FilePrefix            !< XDMF filename prefix
        integer(I4P),     optional,               intent(IN)    :: Action                !< XDMF action to be performed (Read or Write)
    !----------------------------------------------------------------- 
        call this%Free()
        this%MPIEnvironment        => MPIEnvironment
        this%StepsHandler          => StepsHandler
        this%SpatialGridDescriptor => SpatialGridDescriptor
        this%UniformGridDescriptor => UniformGridDescriptor
        if(present(FilePrefix) .and. Present(Action)) call this%Open(FilePrefix, Action)
        this%State = XDMF_HANDLER_STATE_INIT
    end subroutine xdmf_handler_Initialize


    subroutine xdmf_handler_Free(this)
    !-----------------------------------------------------------------
    !< Free XDMF file handler
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XMDF handler
    !----------------------------------------------------------------- 
        if(allocated(this%prefix)) deallocate(this%prefix)
        call this%TemporalFile%Free()
        call this%SpatialFile%Free()
        nullify(this%MPIEnvironment)
        nullify(this%StepsHandler)
        nullify(this%SpatialGridDescriptor)
        nullify(this%UniformGridDescriptor)
        this%State = XDMF_HANDLER_STATE_START
    end subroutine xdmf_handler_Free


    function xdmf_handler_HasPrefix(this) result (HasPrefix)
    !-----------------------------------------------------------------
    !< Check if the file prefix was already set
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XMDF handler
        logical                              :: HasPrefix             !< Flag to check if Prefix has been set
    !----------------------------------------------------------------- 
        HasPrefix = .false.
        if(allocated(this%Prefix)) then
            if(len_trim(this%Prefix) > 0) HasPrefix = .true.
        endif
    end function xdmf_handler_HasPrefix


    function xdmf_handler_GetHDF5Filename(this, Step) result (HDF5FileName)
    !-----------------------------------------------------------------
    !< Generate HDF5 Filename depending on time step
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),  intent(INOUT) :: this                 !< XMDF handler
        integer(I4P), optional, intent(IN)    :: Step                 !< Force step number
        character(len=:), allocatable         :: HDF5FileName         !< Name of the current HDF5 file
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%HasPrefix())
        if(present(Step)) then
            HDF5Filename = trim(adjustl(this%prefix))//'_'//trim(adjustl(str(no_sign=.true., n=Step)))//HDF5_EXT
        else
            HDF5Filename = trim(adjustl(this%prefix))//'_'//trim(adjustl(str(no_sign=.true., n=this%StepsHandler%GetCurrentStep())))//HDF5_EXT
        endif
    end function xdmf_handler_GetHDF5Filename


    function xdmf_handler_GetSpatialFilename(this, Step) result (SpatialFileName)
    !-----------------------------------------------------------------
    !< Generate HDF5 Filename depending on time step
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),  intent(INOUT) :: this                 !< XMDF handler
        integer(I4P), optional, intent(IN)    :: Step                 !< Force step number
        character(len=:), allocatable         :: SpatialFileName      !< Name of the current Spatial file
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%HasPrefix())
        if(present(Step)) then
            SpatialfileName = trim(adjustl(this%prefix))//'_'//trim(adjustl(str(no_sign=.true., n=Step)))//XI_EXT
        else
            SpatialfileName = trim(adjustl(this%prefix))//'_'//trim(adjustl(str(no_sign=.true., n=this%StepsHandler%GetCurrentStep())))//XI_EXT
        endif
    end function xdmf_handler_GetSpatialFilename


    subroutine xdmf_handler_Open(this, FilePrefix, Action)
    !-----------------------------------------------------------------
    !< Open a XDMF file 
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        character(len=*),      intent(IN)    :: FilePrefix            !< XDMF filename prefix
        integer(I4P),          intent(IN)    :: Action                !< XDMF action to be performed (Read or Write)
    !-----------------------------------------------------------------
        this%Action = action
        if(this%MPIEnvironment%is_root()) then
            this%Prefix = trim(adjustl(FilePrefix))
        endif
    end subroutine xdmf_handler_Open


    subroutine xdmf_handler_OpenSpatialFile(this)
    !-----------------------------------------------------------------
    !< Open a XI spatial file 
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        type(xdmf_grid_t)                    :: grid                  !< XDMF Grid 
        type(xdmf_time_t)                    :: time                  !< XDMF Time type
    !-----------------------------------------------------------------
        assert(this%State == XDMF_HANDLER_STATE_INIT)
        if(this%MPIEnvironment%is_root()) then
            assert(this%HasPrefix())
            call this%StepsHandler%SetCurrentFilename(this%GetSpatialFileName())
            call this%SpatialFile%set_filename(this%StepsHandler%GetCurrentFilename())
            select case(this%action)
                case(XDMF_ACTION_WRITE)
                    call this%SpatialFile%openfile(write_header=.false.)
                    call grid%open(xml_handler = this%SpatialFile%xml_handler, &
                            GridType='Collection', &
                            CollectionType='Spatial')
                    call time%open(xml_handler=this%SpatialFile%xml_handler, &
                        TimeType='Single',Value=this%StepsHandler%GetCurrentValue())
                    call time%close(xml_handler=this%SpatialFile%xml_handler)
            end select
        endif
    end subroutine xdmf_handler_OpenSpatialFile


    subroutine xdmf_handler_OpenSpatialGrid(this, GridID)
    !-----------------------------------------------------------------
    !< Open a XDMF grid
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        integer(I4P),          intent(IN)    :: GridID                !< Grid ID number
        type(xdmf_grid_t)                    :: grid                  !< XDMF Grid type
    !-----------------------------------------------------------------
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_WRITE)
        if(this%MPIEnvironment%is_root()) then
            assert(this%HasPrefix())
            call grid%open(xml_handler=this%SpatialFile%xml_handler, &
                Name='Grid'//trim(adjustl(str(no_sign=.true.,n=GridID))))
        endif
    end subroutine xdmf_handler_OpenSpatialGrid


    subroutine xdmf_handler_ParseTemporalFile(this)
    !-----------------------------------------------------------------
    !< Parse a readed file and distribute the information
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),  intent(INOUT) :: this                 !< XDMF handler
        type(Node),     pointer               :: DocumentRootNode     !< Fox DOM Document Root node
        type(Node),     pointer               :: DomainNode           !< Fox DOM Domain node
        type(Node),     pointer               :: TemporalGridNode     !< Fox DOM TemporalGrid node
        type(NodeList), pointer               :: XIncludeNodes        !< Fox DOM Xinclude node list
        type(Node),     pointer               :: XIncludeNode         !< Fox DOM Xinclude node 
        type(xdmf_xinclude_t)                 :: xinclude             !< XDMF Xinclude type
        integer                               :: i
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_READ)
        if(this%MPIEnvironment%is_root()) then
            assert(this%HasPrefix())
            call this%TemporalFile%set_filename(trim(adjustl(this%Prefix))//XDMF_EXT)
            call this%TemporalFile%ParseFile()
            if(getNodeType(this%TemporalFile%get_document_root())==DOCUMENT_NODE) then
                DocumentRootNode => getDocumentElement(this%TemporalFile%get_document_root())
                DomainNode => this%GetUniqueNodeByTag(FatherNode = DocumentRootNode, Tag = 'Domain')
                ! Get Domain Node
                if(.not. associated(DomainNode)) return
                ! Get Temporal Grid Node
                TemporalGridNode => this%GetFirstChildByTag(FatherNode = DomainNode, Tag = 'Grid')
                if(.not. associated(TemporalGridNode)) return
                XIncludeNodes => getElementsByTagname(TemporalGridNode, 'xi:include')

                call this%StepsHandler%Initialize(this%MPIEnvironment, getLength(XIncludeNodes))
                do i = 0, getLength(XIncludeNodes) - 1
                    XIncludeNode => item(XIncludeNodes, i)
                    call xinclude%Parse(DOMNode = XIncludeNode)
                    call this%StepsHandler%Append(Filename=xinclude%GetHRef())
                enddo
            endif
            call destroy(this%TemporalFile%get_document_root())
        endif
        call this%TemporalFile%SetParsed()
        call this%StepsHandler%BroadCastNumberOfSteps()
        call this%StepsHandler%Begin(Start=this%SpatialGridDescriptor%isStaticGrid())
        this%State = XDMF_HANDLER_STATE_PARSED
    end subroutine xdmf_handler_ParseTemporalFile


    subroutine xdmf_handler_ParseSpatialFile(this)
    !-----------------------------------------------------------------
    !< Parse a readed file and distribute the information
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),  intent(INOUT) :: this                 !< XDMF handler
        type(Node),     pointer               :: SpatialGridNode      !< Fox DOM SpatialGrid node
        type(NodeList), pointer               :: UniformGridNodes     !< Fox DOM UniformGrid node list
        integer                               :: i
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_READ)
        if(.not. this%TemporalFile%isParsed()) call this%ParseTemporalFile()
        if(.not. this%SpatialFile%isParsed()) then
            if(this%MPIEnvironment%is_root()) then
                call this%SpatialFile%set_filename(this%StepsHandler%GetCurrentFilename())
                call this%SpatialFile%ParseFile()
                ! Get Spatial Grid Node
                SpatialGridNode => getDocumentElement(this%SpatialFile%get_document_root())
                if(.not. associated(SpatialGridNode)) return
                UniformGridNodes => getElementsByTagname(SpatialGridNode, 'Grid')
                if(.not. associated(UniformGridNodes)) return
                ! Get Fill Spatial Grid metainfo
                call this%FillSpatialGridDescriptor(UniformGridNodes=UniformGridNodes)
                call destroy(this%SpatialFile%get_document_root())
            endif
            call this%SpatialFile%SetParsed()
            call this%SpatialGridDescriptor%BroadcastMetadata()
        endif
    end subroutine xdmf_handler_ParseSpatialFile


    subroutine xdmf_handler_SerializeSpatialFile(this)
    !-----------------------------------------------------------------
    !< Serialize the topology, geometry and attribute metadata to a XDMF file
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        integer(I4P)                         :: IDidx                 !< GridID idex
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_WRITE)
        if(this%MPIEnvironment%is_root()) then
            call this%OpenSpatialFile()
            do IDidx=0, this%MPIEnvironment%get_comm_size()-1
                call this%OpenSpatialGrid(GridID = IDidx)
                call this%WriteTopology(GridID = IDidx)
                call this%WriteGeometry(GridID = IDidx)
                call this%WriteAttributes(GridID = IDidx)
                call this%CloseSpatialGrid()
            enddo
            call this%CloseSpatialFile()
            call this%UniformGridDescriptor%FreeAttributesMetadata()
        endif
        this%State = XDMF_HANDLER_STATE_SERIALIZED
    end subroutine xdmf_handler_SerializeSpatialFile


    subroutine xdmf_handler_SerializeTemporalFile(this)
    !-----------------------------------------------------------------
    !< Serialize the topology, geometry and attribute metadata to a XDMF file
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        type(xdmf_grid_t)                    :: grid                  !< XDMF Grid type
        type(xdmf_domain_t)                  :: domain                !< XDMF Domain type
        type(xdmf_xinclude_t)                :: xinclude              !< XDMF XInclude type
        integer(I4P)                         :: i
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_WRITE)
        if(this%MPIEnvironment%is_root()) then
            assert(this%HasPrefix())
            select case(this%action)
                case(XDMF_ACTION_WRITE)
                    call this%TemporalFile%set_filename(trim(adjustl(this%Prefix))//XDMF_EXT)
                    call this%TemporalFile%OpenFile()
                    call this%StepsHandler%Begin()
                    call domain%open(xml_handler = this%TemporalFile%xml_handler)
                    call grid%open(xml_handler = this%TemporalFile%xml_handler, &
                            GridType='Collection', &
                            CollectionType='Temporal')
                    do i=1, this%StepsHandler%GetNumberOfSteps()
                        call xinclude%Open(xml_handler=this%TemporalFile%xml_handler, HRef=this%StepsHandler%GetCurrentFilename())
                        call xinclude%Close(xml_handler=this%TemporalFile%xml_handler)
                        call this%StepsHandler%Next()
                    enddo
                    call grid%close(xml_handler=this%TemporalFile%xml_handler)
                    call domain%close(xml_handler = this%TemporalFile%xml_handler)
                    call this%TemporalFile%CloseFile()
            end select
        endif
    end subroutine xdmf_handler_SerializeTemporalFile


    subroutine xdmf_handler_CloseSpatialGrid(this)
    !-----------------------------------------------------------------
    !< Close a XDMF grid
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        type(xdmf_grid_t)                    :: grid                  !< XDMF Grid type
    !-----------------------------------------------------------------
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_WRITE .and. this%SpatialFile%isOpen())
        if(this%MPIEnvironment%is_root()) then
            call grid%Close(xml_handler=this%SpatialFile%xml_handler)
        endif
    end subroutine xdmf_handler_CloseSpatialGrid


    subroutine xdmf_handler_CloseSpatialFile(this)
    !-----------------------------------------------------------------
    !< Close a XI spatial file 
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        type(xdmf_grid_t)                    :: grid                  !< XDMF Grid type
        type(xdmf_domain_t)                  :: domain                !< XDMF Domain type
        type(xdmf_xinclude_t)                :: xinclude              !< XDMF Xinclude type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            select case(this%action)
                case(XDMF_ACTION_WRITE)
                    call grid%close(xml_handler=this%SpatialFile%xml_handler)
                    call this%SpatialFile%closefile()
            end select
        endif
        call this%SpatialFile%Free()
        this%State = XDMF_HANDLER_STATE_CLOSED
    end subroutine xdmf_handler_CloseSpatialFile


    function xdmf_handler_NodeIsDocumentRoot(this, DOMNode) result(NodeIsDocumentRoot)
    !-----------------------------------------------------------------
    !< Check if a DOM node is a document root
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        type(Node), pointer,   intent(IN)    :: DOMNode               !< FoX DOM Node 
        logical                              :: NodeIsDocumentRoot    !< Return True if the passed node is the root of the document
    !----------------------------------------------------------------- 
        NodeIsDocumentRoot = .false.
        if(associated(DOMNode)) then
            NodeIsDocumentRoot = (getNodeType(this%TemporalFile%get_document_root())==DOCUMENT_NODE)
        endif
    end function xdmf_handler_NodeIsDocumentRoot


    function xdmf_handler_GetUniqueNodeByTag(this, FatherNode, Tag) result(ChildNode)
    !-----------------------------------------------------------------
    !< Return the first FoX DOM child node given a father node
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),      intent(INOUT) :: this            !< XDMF handler
        type(Node),        pointer, intent(IN)    :: FatherNode      !< Fox DOM Father node
        character(len=*),           intent(IN)    :: Tag             !< Fox DOM Child node
        type(Node),        pointer                :: ChildNode       !< Fox DOM result Child node
        type(NodeList),    pointer                :: Childrens       !< List of childrens of the document root node
        integer(I4P)                              :: i               !< Index for a loop in Childrens
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_READ)
        nullify(ChildNode)
        if(hasChildNodes(FatherNode)) then
            Childrens => getElementsByTagname(FatherNode, Tag)
            if(getLength(Childrens) == 1) ChildNode => item(Childrens, 0)
        endif
        nullify(Childrens)
    end function xdmf_handler_GetUniqueNodeByTag


    function xdmf_handler_GetFirstChildByTag(this, FatherNode, Tag) result(ChildNode)
    !-----------------------------------------------------------------
    !< Return the first FoX DOM child node given a father node
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),      intent(INOUT) :: this             !< XDMF handler
        type(Node),        pointer, intent(IN)    :: FatherNode       !< Fox DOM Father node
        character(len=*),           intent(IN)    :: Tag              !< Fox DOM Child node
        type(Node),        pointer                :: ChildNode        !< Fox DOM result Child node
        type(NodeList),    pointer                :: Childrens        !< List of childrens of the document root node
        type(xdmf_grid_t) :: grid
        integer(I4P)                              :: i                !< Index for a loop in Childrens
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_READ)
        nullify(ChildNode)
        if(hasChildNodes(FatherNode)) then
            Childrens => getChildNodes(FatherNode)
            do i = 0, getLength(Childrens) - 1
                ChildNode => item(Childrens, i)
                if(getNodeType(ChildNode) == TEXT_NODE) cycle
                if(getTagName(Childnode) == trim(adjustl(Tag))) exit
                nullify(ChildNode)
            enddo
        endif
        nullify(Childrens)
    end function xdmf_handler_GetFirstChildByTag


    function xdmf_handler_GetDataItemXPath(this, DataItemNode) result(XPath)
    !-----------------------------------------------------------------
    !< Returns the XPath from a Hyperslab DataItem FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),          intent(INOUT) :: this         !< XDMF handler
        type(Node),        pointer,     intent(IN)    :: DataItemNode !< Fox DOM DataItem node
        character(len=:),  allocatable                :: XPath        !< XPath of the dataitem
        type(NodeList),    pointer                    :: Childrens    !< Fox DOM node list
        type(Node),        pointer                    :: ChildNode    !< Fox DOM node
        type(xdmf_dataitem_t)                         :: dataitem     !< XDMF Topology derived type
        integer(I4P)                                  :: i            !< Index for a loop in Childrens
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_READ)
        if(.not. associated(DataItemNode)) return
        if(hasChildNodes(DataItemNode)) then
            Childrens => getChildNodes(DataItemNode)
            do i = 0, getLength(Childrens) - 1
                ChildNode => item(Childrens, i)
                if(getNodeType(ChildNode) == TEXT_NODE) cycle
                if(getTagName(Childnode) == 'DataItem' .and. (getAttribute(ChildNode, 'Format') == 'HDF')) then
                    XPath = getTextContent(ChildNode)
                endif
            enddo
        endif
        nullify(Childrens)
        nullify(ChildNode)
        call dataitem%Free()
    end function xdmf_handler_GetDataItemXPath


    subroutine xdmf_handler_FillSpatialGridGeometry(this, GeometryNode, ID)
    !----------------------------------------------------------------- 
    !< Fill the Spatial grid geometry metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        type(Node), pointer,   intent(IN)    :: GeometryNode          !< Fox DOM Geometry node
        integer(I4P),          intent(IN)    :: ID                    !< Grid IDentifier
        type(xdmf_geometry_t)                :: Geometry              !< XDMF Geometry derived type
        type(xdmf_dataitem_t)                :: DataItem              !< XDMF DataItem derived type
        type(Node), pointer                  :: DataItemNode          !< Fox DOM Dataitem node
        integer(I8P)                         :: auxDims(1)            !< Aux dimensions variable
        integer(I4P)                         :: spacedims             !< Space dimensions
        integer(I4P)                         :: GeometryType          !< GeometryType
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_READ)
        if(.not. associated(GeometryNode)) return
        call Geometry%Parse(DOMNode = GeometryNode)
        ! Set GeometryType
        GeometryType = GetXDMFGeometryTypeFromName(Geometry%get_GeometryType())
        call this%SpatialGridDescriptor%SetGeometryTypePerGridID(GeometryType,ID=ID)
        ! Set NumberOfNodes
        DataItemNode => this%GetFirstChildByTag(FatherNode = GeometryNode, Tag = 'DataItem')
        call DataItem%Parse(DomNode = DataItemNode)
        auxDims = DataItem%get_Dimensions()
        spacedims = GetSpaceDimension(GetXDMFGeometryTypeFromName(Geometry%get_GeometryType()))
        select case (GeometryType)
            case (XDMF_GEOMETRY_TYPE_XY, XDMF_GEOMETRY_TYPE_XYZ)
                call this%SpatialGridDescriptor%SetNumberOfNodesPerGridID(AuxDims(1)/spacedims,ID=ID)
            case (XDMF_GEOMETRY_TYPE_X_Y_Z)
                call this%SpatialGridDescriptor%SetNumberOfNodesPerGridID(AuxDims(1),ID=ID)
        end select
        ! Free
        nullify(DataItemNode)
        call Geometry%Free()
        call DataItem%Free()
    end subroutine xdmf_handler_FillSpatialGridGeometry


    subroutine xdmf_handler_FillSpatialGridDescriptor(this, UniformGridNodes)
    !-----------------------------------------------------------------
    !< Fill Spatial Grid Descriptor From a FoX DOM UniformGrid node list
    !< given the Spatial Grid Node
    !----------------------------------------------------------------- 
        class(xdmf_handler_t),   intent(INOUT) :: this                !< XDMF handler
        type(NodeList), pointer, intent(IN)    :: UniformGridNodes    !< Fox DOM Grid node list
        type(Node),     pointer                :: UniformGridNode     !< Fox DOM Grid node
        type(Node),     pointer                :: ChildNode           !< Fox DOM node
        type(NodeList), pointer                :: AttributeNodes      !< Fox DOM Attribute node list
        type(xdmf_geometry_t)                  :: Geometry            !< XDMF Topology derived type
        type(xdmf_attribute_t)                 :: Attribute           !< XDMF Attribute derived type
        integer(I4P)                           :: i                   !< Index for a loop in UniformGridNodes
    !----------------------------------------------------------------- 
        assert(this%State == XDMF_HANDLER_STATE_INIT .and. this%Action == XDMF_ACTION_READ)
        if(associated(UniformGridNodes)) then
            call this%SpatialGridDescriptor%Allocate(NumberOfGrids=getLength(UniformGridNodes))
            do i = 0, getLength(UniformGridNodes) - 1
                UniformGridNode => item(UniformGridNodes, i)
                ! Fill each Spatial Grid Topology
                ChildNode => this%GetUniqueNodeByTag(FatherNode = UniformGridNode, Tag = 'Topology')
                call this%FillSpatialGridTopology(TopologyNode = ChildNode, ID = i)
                ! Fill each Spatial Grid Geometry
                ChildNode => this%GetUniqueNodeByTag(FatherNode = UniformGridNode, Tag = 'Geometry')
                call this%FillSpatialGridGeometry(GeometryNode = Childnode, ID = i)
            enddo
            nullify(UniformGridNode)
            nullify(ChildNode)
            nullify(Attributenodes)
            call Geometry%Free()
            call Attribute%Free()
        endif
    end subroutine xdmf_handler_FillSpatialGridDescriptor


    subroutine xdmf_handler_SetGeometry_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Add R4P geometry info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        real(R4P),             intent(IN)    :: XYZ(:)                !< Grid coordinates
        character(len=*),      intent(IN)    :: Name                  !< Topology name
    !-----------------------------------------------------------------
        assert(this%State == XDMF_HANDLER_STATE_INIT)
        call this%UniformGridDescriptor%SetGeometryMetadata(Name            = Name, &
                                                            Precision       = 4,    &
                                                            ArrayDimensions = (/size(XYZ, dim=1)/))
    end subroutine xdmf_handler_SetGeometry_R4P


    subroutine xdmf_handler_SetGeometry_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Add R8P geometry info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        real(R8P),             intent(IN)    :: XYZ(:)                !< Grid coordinates
        character(len=*),      intent(IN)    :: Name                  !< Geometry name
    !-----------------------------------------------------------------
        assert(this%State == XDMF_HANDLER_STATE_INIT)
        call this%UniformGridDescriptor%SetGeometryMetadata(Name            = Name, &
                                                            Precision       = 8,    &
                                                            ArrayDimensions = (/size(XYZ, dim=1)/))
    end subroutine xdmf_handler_SetGeometry_R8P


    subroutine xdmf_handler_AppendAttribute_I4P(this, Name, Type, Center, Attribute)
    !-----------------------------------------------------------------
    !< Append I4P attribute info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        character(len=*),      intent(IN)    :: Name                  !< Attribute Name
        integer(I4P),          intent(IN)    :: Type                  !< Attribute Type (Scalar, Vector, etc.)
        integer(I4P),          intent(IN)    :: Center                !< Attribute Center (Node, Cell, etc.)
        integer(I4P),          intent(IN)    :: Attribute(:)          !< I4P Grid attribute
    !-----------------------------------------------------------------
        assert(this%State == XDMF_HANDLER_STATE_INIT)
        call this%UniformGridDescriptor%UpdateNumberOfAttributes()
        call this%UniformGridDescriptor%SetLastAttributeMetadata( &
                        Name=trim(adjustl(Name)),                 &
                        Type=Type, DataType='Int',                &
                        Center=Center,                            &
                        Precision=4,                              &
                        ArrayDimensions=(/size(Attribute, dim=1)/))
    end subroutine xdmf_handler_AppendAttribute_I4P


    subroutine xdmf_handler_AppendAttribute_I8P(this, Name, Type, Center, Attribute)
    !-----------------------------------------------------------------
    !< Add I8P attribute info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        character(len=*),      intent(IN)    :: Name                  !< Attribute Name
        integer(I4P),          intent(IN)    :: Type                  !< Attribute Type (Scalar, Vector, etc.)
        integer(I4P),          intent(IN)    :: Center                !< Attribute Center (Node, Cell, etc.)
        integer(I8P),          intent(IN)    :: Attribute(:)          !< I8P Grid attribute
    !-----------------------------------------------------------------
        assert(this%State == XDMF_HANDLER_STATE_INIT)
        call this%UniformGridDescriptor%UpdateNumberOfAttributes()
        call this%UniformGridDescriptor%SetLastAttributeMetadata( &
                        Name=trim(adjustl(Name)),                 &
                        Type=Type, DataType='Int',                &
                        Center=Center,                            &
                        Precision=8,                              &
                        ArrayDimensions=(/size(Attribute, dim=1)/))

    end subroutine xdmf_handler_AppendAttribute_I8P


    subroutine xdmf_handler_AppendAttribute_R4P(this, Name, Type, Center, Attribute)
    !-----------------------------------------------------------------
    !< Add R4P attribute info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        character(len=*),      intent(IN)    :: Name                  !< Attribute Name
        integer(I4P),          intent(IN)    :: Type                  !< Attribute Type (Scalar, Vector, etc.)
        integer(I4P),          intent(IN)    :: Center                !< Attribute Center (Node, Cell, etc.)
        real(R4P),             intent(IN)    :: Attribute(:)          !< R4P Grid attribute
    !-----------------------------------------------------------------
        assert(this%State == XDMF_HANDLER_STATE_INIT)
        call this%UniformGridDescriptor%UpdateNumberOfAttributes()
        call this%UniformGridDescriptor%SetLastAttributeMetadata( &
                        Name=trim(adjustl(Name)),                 &
                        Type=Type, DataType='Float',              &
                        Center=Center,                            &
                        Precision=4,                              &
                        ArrayDimensions=(/size(Attribute, dim=1)/))

    end subroutine xdmf_handler_AppendAttribute_R4P


    subroutine xdmf_handler_AppendAttribute_R8P(this, Name, Type, Center, Attribute)
    !-----------------------------------------------------------------
    !< Add R4P attribute info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(INOUT) :: this                  !< XDMF handler
        character(len=*),      intent(IN)    :: Name                  !< Attribute Name
        integer(I4P),          intent(IN)    :: Type                  !< Attribute Type (Scalar, Vector, etc.)
        integer(I4P),          intent(IN)    :: Center                !< Attribute Center (Node, Cell, etc.)
        real(R8P),             intent(IN)    :: Attribute(:)          !< R4P Grid attribute
    !-----------------------------------------------------------------
        assert(this%State == XDMF_HANDLER_STATE_INIT)
        call this%UniformGridDescriptor%UpdateNumberOfAttributes()
        call this%UniformGridDescriptor%SetLastAttributeMetadata( &
                        Name=trim(adjustl(Name)),                 &
                        Type=Type, DataType='Float',              &
                        Center=Center,                            &
                        Precision=8,                              &
                        ArrayDimensions=(/size(Attribute, dim=1)/))

    end subroutine xdmf_handler_AppendAttribute_R8P


    subroutine xdmf_handler_CalculateAttributeDimensions(this, GridID, Center, GlobalNumberOfData, LocalNumberOfData, DataOffset)
    !-----------------------------------------------------------------
    !< Calculate hyperslab dimensions
    !----------------------------------------------------------------- 
        class(xdmf_handler_t), intent(IN)  :: this                    !< xmdf handler
        integer(I4P),          intent(IN)  :: GridID                  !< Grid ID
        integer(I4P),          intent(IN)  :: Center                  !< Attribute center at (Node, Cell, etc.)
        integer(I8P),          intent(OUT) :: GlobalNumberOfData      !< Global number of data
        integer(I8P),          intent(OUT) :: LocalNumberOfData       !< Local number of data
        integer(I8P),          intent(OUT) :: DataOffset              !< Data offset for current grid
    !----------------------------------------------------------------- 
    !< @TODO: face and edge centered attributes
        select case(Center)
            case (XDMF_ATTRIBUTE_CENTER_NODE)
                GlobalNumberOfData = this%SpatialGridDescriptor%GetGlobalNumberOfNodes()
                LocalNumberOfData  = this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=GridID)
                DataOffset         = this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=GridID)
            case (XDMF_ATTRIBUTE_CENTER_CELL)
                GlobalNumberOfData = this%SpatialGridDescriptor%GetGlobalNumberOfElements()
                LocalNumberOfData  = this%SpatialGridDescriptor%GetNumberOfElementsPerGridID(ID=GridID)
                DataOffset         = this%SpatialGridDescriptor%GetElementOffsetPerGridID(ID=GridID)
            case (XDMF_ATTRIBUTE_CENTER_GRID)
                GlobalNumberOfData = this%MPIEnvironment%get_comm_size()
                LocalNumberOfData  = 1_I8P
                DataOffset         = GridID
            case Default
                GlobalNumberOfData = this%SpatialGridDescriptor%GetGlobalNumberOfNodes()
                LocalNumberOfData  = this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=GridID)
                DataOffset         = this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=GridID)
        end select
    end subroutine xdmf_handler_CalculateAttributeDimensions

end module xdmf_handler
