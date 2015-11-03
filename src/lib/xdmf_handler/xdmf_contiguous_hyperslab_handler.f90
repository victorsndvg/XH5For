module xdmf_contiguous_hyperslab_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use IR_Precision, only: I4P, I8P, R4P, R8P, str
use xh5for_utils
use fox_xdmf
use fox_dom,      only: Node, NodeList, ParseFile, GetDocumentElement, Item, GetLength, GetChildNodes, getAttribute, &
                        HasChildNodes, GetElementsByTagName, GetNodeType, GetTagName, Destroy, getTextContent, &
                        TEXT_NODE, DOCUMENT_NODE
use xdmf_handler

implicit none

private


    type, extends(xdmf_handler_t) :: xdmf_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
    contains
    private
        procedure         :: CalculateHyperSlabDimensions => xdmf_contiguous_hyperslab_handler_CalculateHyperSlabDimensions
        procedure         :: SetGeometry_R4P              => xdmf_contiguous_hyperslab_handler_SetGeometry_R4P
        procedure         :: SetGeometry_R8P              => xdmf_contiguous_hyperslab_handler_SetGeometry_R8P
        procedure         :: SetTopology_I4P              => xdmf_contiguous_hyperslab_handler_SetTopology_I4P
        procedure         :: SetTopology_I8P              => xdmf_contiguous_hyperslab_handler_SetTopology_I8P
        procedure         :: AppendAttribute_I4P          => xdmf_contiguous_hyperslab_handler_AppendAttribute_I4P
        procedure         :: AppendAttribute_I8P          => xdmf_contiguous_hyperslab_handler_AppendAttribute_I8P
        procedure         :: AppendAttribute_R4P          => xdmf_contiguous_hyperslab_handler_AppendAttribute_R4P
        procedure         :: AppendAttribute_R8P          => xdmf_contiguous_hyperslab_handler_AppendAttribute_R8P
        procedure         :: WriteGeometry                => xdmf_contiguous_hyperslab_handler_WriteGeometry
        procedure         :: WriteTopology                => xdmf_contiguous_hyperslab_handler_WriteTopology
        procedure         :: WriteAttributes              => xdmf_contiguous_hyperslab_handler_WriteAttributes
        procedure         :: OpenGrid                     => xdmf_contiguous_hyperslab_handler_OpenGrid
        procedure         :: CloseGrid                    => xdmf_contiguous_hyperslab_handler_CloseGrid
        procedure         :: GetUniqueNodeByTag           => xdmf_contiguous_hyperslab_handler_GetUniqueNodeByTag
        procedure         :: GetFirstChildByTag           => xdmf_contiguous_hyperslab_handler_GetFirstChildByTag
        procedure         :: GetDataItemXPath             => xdmf_contiguous_hyperslab_handler_GetDataItemXPath
        procedure         :: FillSpatialGridTopology      => xdmf_contiguous_hyperslab_handler_FillSpatialGridTopology
        procedure         :: FillSpatialGridGeometry      => xdmf_contiguous_hyperslab_handler_FillSpatialGridGeometry
        procedure         :: FillSpatialGridDescriptor    => xdmf_contiguous_hyperslab_handler_FillSpatialGridDescriptor
        procedure, public :: OpenFile                     => xdmf_contiguous_hyperslab_handler_OpenFile
        procedure, public :: Free                         => xdmf_contiguous_hyperslab_handler_Free
        procedure, public :: CloseFile                    => xdmf_contiguous_hyperslab_handler_CloseFile
        procedure, public :: Serialize                    => xdmf_contiguous_hyperslab_handler_Serialize
        procedure, public :: ParseFile                    => xdmf_contiguous_hyperslab_handler_ParseFile
    end type xdmf_contiguous_hyperslab_handler_t

public :: xdmf_contiguous_hyperslab_handler_t

contains

    subroutine xdmf_contiguous_hyperslab_handler_OpenGrid(this, GridID)
    !-----------------------------------------------------------------
    !< Open a XDMF grid
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this        !< XDMF contiguous hyperslab handler
        integer(I4P),                     optional, intent(IN)    :: GridID      !< Grid ID number
        type(xdmf_grid_t)                                         :: grid        !< XDMF Grid type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            call grid%open(xml_handler=this%file%xml_handler, &
                Name='Grid'//trim(adjustl(str(no_sign=.true.,n=GridID))))
        endif
    end subroutine xdmf_contiguous_hyperslab_handler_OpenGrid


    subroutine xdmf_contiguous_hyperslab_handler_CloseGrid(this, GridID)
    !-----------------------------------------------------------------
    !< Close a XDMF grid
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this        !< XDMF contiguous hyperslab handler
        integer(I4P),                     optional, intent(IN)    :: GridID      !< Grid ID number
        type(xdmf_grid_t)                                         :: grid        !< XDMF Grid type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            call grid%Close(xml_handler=this%file%xml_handler)
        endif
    end subroutine xdmf_contiguous_hyperslab_handler_CloseGrid


    subroutine xdmf_contiguous_hyperslab_handler_Free(this)
    !-----------------------------------------------------------------
    !< Free XDMF file handler
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XMDF handler
        integer(I4P)                                              :: indx
    !----------------------------------------------------------------- 
        if(allocated(this%prefix)) deallocate(this%prefix)
        !call this%file%Free()
        nullify(this%MPIEnvironment)
        nullify(this%SpatialGridDescriptor)
        nullify(this%UniformGridDescriptor)
    end subroutine xdmf_contiguous_hyperslab_handler_Free


    subroutine xdmf_contiguous_hyperslab_handler_SetGeometry_R4P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Add R4P geometry info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XDMF contiguous hyperslab handler
        real(R4P),                                  intent(IN)    :: Coordinates(:) !< Grid coordinates
        character(len=*),                           intent(IN)    :: Name           !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetGeometryMetadata(Name = Name, Precision=4, ArrayDimensions=1)
    end subroutine xdmf_contiguous_hyperslab_handler_SetGeometry_R4P


    subroutine xdmf_contiguous_hyperslab_handler_SetGeometry_R8P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Add R8P geometry info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XDMF contiguous hyperslab handler
        real(R8P),                                  intent(IN)    :: Coordinates(:) !< Grid coordinates
        character(len=*),                           intent(IN)    :: Name           !< Geometry name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetGeometryMetadata(Name=Name, Precision=8, ArrayDimensions=1)
    end subroutine xdmf_contiguous_hyperslab_handler_SetGeometry_R8P


    subroutine xdmf_contiguous_hyperslab_handler_SetTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I4P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler
        integer(I4P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                           intent(IN)    :: Name              !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetTopologyMetadata(Name=Name, Precision=4, ArrayDimensions=1)
    end subroutine xdmf_contiguous_hyperslab_handler_SetTopology_I4P


    subroutine xdmf_contiguous_hyperslab_handler_SetTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I8P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler
        integer(I8P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                           intent(IN)    :: Name              !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetTopologyMetadata(Name=Name, Precision=8, ArrayDimensions=1)
    end subroutine xdmf_contiguous_hyperslab_handler_SetTopology_I8P


    subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_I4P(this, Name, Type, Center, Attribute)
    !-----------------------------------------------------------------
    !< Append I4P attribute info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XDMF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: Name         !< Attribute Name
        integer(I4P),                               intent(IN)    :: Type         !< Attribute Type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN)    :: Center       !< Attribute Center (Node, Cell, etc.)
        integer(I4P),                               intent(IN)    :: Attribute(:) !< I4P Grid attribute
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%UpdateNumberOfAttributes()
        call this%UniformGridDescriptor%SetLastAttributeMetadata(Name=trim(adjustl(Name)), Type=Type, DataType='Int', Center=Center, Precision=4, ArrayDimensions=1)
    end subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_I4P


    subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_I8P(this, Name, Type, Center, Attribute)
    !-----------------------------------------------------------------
    !< Add I8P attribute info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XDMF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: Name         !< Attribute Name
        integer(I4P),                               intent(IN)    :: Type         !< Attribute Type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN)    :: Center       !< Attribute Center (Node, Cell, etc.)
        integer(I8P),                               intent(IN)    :: Attribute(:) !< I8P Grid attribute
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%UpdateNumberOfAttributes()
        call this%UniformGridDescriptor%SetLastAttributeMetadata(Name=trim(adjustl(Name)), Type=Type, DataType='Int', Center=Center, Precision=8, ArrayDimensions=1)
    end subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_I8P


    subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_R4P(this, Name, Type, Center, Attribute)
    !-----------------------------------------------------------------
    !< Add R4P attribute info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XDMF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: Name         !< Attribute Name
        integer(I4P),                               intent(IN)    :: Type         !< Attribute Type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN)    :: Center       !< Attribute Center (Node, Cell, etc.)
        real(R4P),                                  intent(IN)    :: Attribute(:) !< R4P Grid attribute
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%UpdateNumberOfAttributes()
        call this%UniformGridDescriptor%SetLastAttributeMetadata(Name=trim(adjustl(Name)), Type=Type, DataType='Float', Center=Center, Precision=8, ArrayDimensions=1)
    end subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_R4P


    subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_R8P(this, Name, Type, Center, Attribute)
    !-----------------------------------------------------------------
    !< Add R4P attribute info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XDMF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: Name         !< Attribute Name
        integer(I4P),                               intent(IN)    :: Type         !< Attribute Type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN)    :: Center       !< Attribute Center (Node, Cell, etc.)
        real(R8P),                                  intent(IN)    :: Attribute(:) !< R4P Grid attribute
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%UpdateNumberOfAttributes()
        call this%UniformGridDescriptor%SetLastAttributeMetadata(Name=trim(adjustl(Name)), Type=Type, DataType='Float', Center=Center, Precision=8, ArrayDimensions=1)
    end subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_R8P


    subroutine xdmf_contiguous_hyperslab_handler_CalculateHyperSlabDimensions(this, GridID, Center, GlobalNumberOfData, LocalNumberOfData, DataOffset)
    !-----------------------------------------------------------------
    !< Calculate hyperslab dimensions for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(IN)  :: this                !< xmdf contiguous hyperslab handler
        integer(I4P),                               intent(IN)  :: GridID              !< Grid ID
        integer(I4P),                               intent(IN)  :: Center              !< Attribute center at (Node, Cell, etc.)
        integer(I8P),                               intent(OUT) :: GlobalNumberOfData  !< Global number of data
        integer(I8P),                               intent(OUT) :: LocalNumberOfData   !< Local number of data
        integer(I8P),                               intent(OUT) :: DataOffset          !< Data offset for current grid
    !----------------------------------------------------------------- 
    !< @TODO: face and edge centered attributes
        select case(Center)
            case (XDMF_ATTRIBUTE_CENTER_NODE)
                GlobalNumberOfData = this%SpatialGridDescriptor%GetGlobalNumberOfNodes()
                LocalNumberOfData  = this%SpatialGridDescriptor%GetNumberOfNodesFromGridID(ID=GridID)
                DataOffset         = this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=GridID)
            case (XDMF_ATTRIBUTE_CENTER_CELL)
                GlobalNumberOfData = this%SpatialGridDescriptor%GetGlobalNumberOfElements()
                LocalNumberOfData  = this%SpatialGridDescriptor%GetNumberOfElementsFromGridID(ID=GridID)
                DataOffset         = this%SpatialGridDescriptor%GetElementOffsetFromGridID(ID=GridID)
            case (XDMF_ATTRIBUTE_CENTER_GRID)
                GlobalNumberOfData = this%MPIEnvironment%get_comm_size()
                LocalNumberOfData  = 1_I8P
                DataOffset         = GridID
            case Default
                GlobalNumberOfData = this%SpatialGridDescriptor%GetGlobalNumberOfNodes()
                LocalNumberOfData  = this%SpatialGridDescriptor%GetNumberOfNodesFromGridID(ID=GridID)
                DataOffset         = this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=GridID)
        end select
    end subroutine xdmf_contiguous_hyperslab_handler_CalculateHyperSlabDimensions


    subroutine xdmf_contiguous_hyperslab_handler_OpenFile(this, action, fileprefix)
    !-----------------------------------------------------------------
    !< Open a XDMF file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XDMF contiguous hyperslab handler
        integer(I4P),                               intent(IN)    :: action     !< XDMF action to be performed (Read or Write)
        character(len=*),                           intent(IN)    :: fileprefix !< XDMF filename prefix
        type(xdmf_grid_t)                                         :: grid       !< XDMF Grid type
        type(xdmf_domain_t)                                       :: domain     !< XDMF Domain type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            this%prefix = trim(adjustl(fileprefix))
            this%action = action
            call this%file%set_filename(trim(adjustl(fileprefix))//this%ext)
            select case(this%action)
                case(XDMF_ACTION_WRITE)
                    call this%file%openfile()
                    call domain%open(xml_handler = this%file%xml_handler)
                    call grid%open(xml_handler = this%file%xml_handler, &
                            GridType='Collection', &
                            CollectionType='Spatial')
            end select
        endif
    end subroutine xdmf_contiguous_hyperslab_handler_OpenFile


    function xdmf_contiguous_hyperslab_handler_GetUniqueNodeByTag(this, FatherNode, Tag) result(ChildNode)
    !-----------------------------------------------------------------
    !< Return the first FoX DOM child node given a father node
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XDMF contiguous hyperslab handler
        type(Node),     pointer,                    intent(IN)    :: FatherNode !< Fox DOM Father node
        character(len=*),                           intent(IN)    :: Tag        !< Fox DOM Child node
        type(Node),     pointer                                   :: ChildNode  !< Fox DOM result Child node
        type(NodeList), pointer                                   :: Childrens  !< List of childrens of the document root node
        integer(I4P)                                              :: i          !< Index for a loop in Childrens
    !----------------------------------------------------------------- 
        nullify(ChildNode)
        if(hasChildNodes(FatherNode)) then
            Childrens => getElementsByTagname(FatherNode, Tag)
            if(getLength(Childrens) == 1) ChildNode => item(Childrens, 0)
        endif
        nullify(Childrens)
    end function xdmf_contiguous_hyperslab_handler_GetUniqueNodeByTag


    function xdmf_contiguous_hyperslab_handler_GetFirstChildByTag(this, FatherNode, Tag) result(ChildNode)
    !-----------------------------------------------------------------
    !< Return the first FoX DOM child node given a father node
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XDMF contiguous hyperslab handler
        type(Node),     pointer,                    intent(IN)    :: FatherNode !< Fox DOM Father node
        character(len=*),                           intent(IN)    :: Tag        !< Fox DOM Child node
        type(Node),     pointer                                   :: ChildNode  !< Fox DOM result Child node
        type(NodeList), pointer                                   :: Childrens  !< List of childrens of the document root node
        integer(I4P)                                              :: i          !< Index for a loop in Childrens
    !----------------------------------------------------------------- 
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
    end function xdmf_contiguous_hyperslab_handler_GetFirstChildByTag


    function xdmf_contiguous_hyperslab_handler_GetDataItemXPath(this, DataItemNode) result(XPath)
    !-----------------------------------------------------------------
    !< Returns the XPath from a Hyperslab DataItem FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XDMF contiguous hyperslab handler
        type(Node),     pointer,                    intent(IN)    :: DataItemNode   !< Fox DOM DataItem node
        character(len=:),        allocatable                      :: XPath          !< XPath of the dataitem
        type(NodeList), pointer                                   :: Childrens      !< Fox DOM node list
        type(Node), pointer                                       :: ChildNode      !< Fox DOM node
        type(xdmf_dataitem_t)                                     :: dataitem       !< XDMF Topology derived type
        integer(I4P)                                              :: i          !< Index for a loop in Childrens
    !----------------------------------------------------------------- 
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
    end function xdmf_contiguous_hyperslab_handler_GetDataItemXPath


    subroutine xdmf_contiguous_hyperslab_handler_FillSpatialGridTopology(this, TopologyNode, ID)
    !-----------------------------------------------------------------
    !< Fill the Spatial grid topology metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XDMF contiguous hyperslab handler
        type(Node), pointer,                        intent(IN)    :: TopologyNode !< Fox DOM Topology node
        integer(I4P),                               intent(IN)    :: ID           !< Grid IDentifier
        type(xdmf_topology_t)                                     :: Topology     !< XDMF Topology derived type
        type(Node), pointer                                       :: DataItemNode !< Fox DOM Dataitem node
        integer(I8P),     allocatable                             :: auxDims(:)   !< Aux dimensions variable
        character(len=:), allocatable                             :: XPath        !< Topology XPath
    !----------------------------------------------------------------- 
        if(.not. associated(TopologyNode)) return
        call Topology%Parse(DOMNode = TopologyNode)
        ! Set TopologyType
        call this%SpatialGridDescriptor%SetTopologyTypeByGridID(&
                    TopologyType = GetXDMFTopologyTypeFromName(Topology%get_TopologyType()), ID=ID)
        ! Set NumberOfElements
        auxDims = Topology%get_Dimensions()
        call this%SpatialGridDescriptor%SetNumberOfElementsByGridID(AuxDims(1),ID=ID)
        call Topology%Free()
        nullify(DataItemNode)
    end subroutine xdmf_contiguous_hyperslab_handler_FillSpatialGridTopology


    subroutine xdmf_contiguous_hyperslab_handler_FillSpatialGridGeometry(this, GeometryNode, ID)
    !----------------------------------------------------------------- 
    !< Fill the Spatial grid geometry metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XDMF contiguous hyperslab handler
        type(Node), pointer,                        intent(IN)    :: GeometryNode !< Fox DOM Geometry node
        integer(I4P),                               intent(IN)    :: ID           !< Grid IDentifier
        type(xdmf_geometry_t)                                     :: Geometry     !< XDMF Geometry derived type
        type(xdmf_dataitem_t)                                     :: DataItem     !< XDMF DataItem derived type
        type(Node), pointer                                       :: DataItemNode !< Fox DOM Dataitem node
        integer(I8P),           allocatable                       :: auxDims(:)   !< Aux dimensions variable
        integer(I4P)                                              :: spacedims    !< Space dimensions
    !----------------------------------------------------------------- 
        if(.not. associated(GeometryNode)) return
        call Geometry%Parse(DOMNode = GeometryNode)
        ! Set GeometryType
        call this%SpatialGridDescriptor%SetGeometryTypeByGridID(&
                    GeometryType = GetXDMFGeometryTypeFromName(Geometry%get_GeometryType()),ID=ID)
        ! Set NumberOfNodes
        DataItemNode => this%GetFirstChildByTag(FatherNode = GeometryNode, Tag = 'DataItem')
        call DataItem%Parse(DomNode = DataItemNode)
        auxDims = DataItem%get_Dimensions()
        spacedims = GetSpaceDimension(GetXDMFGeometryTypeFromName(Geometry%get_GeometryType()))
        call this%SpatialGridDescriptor%SetNumberOfNodesByGridID(AuxDims(1)/spacedims,ID=ID)
        nullify(DataItemNode)
        call Geometry%Free()
        call DataItem%Free()
    end subroutine xdmf_contiguous_hyperslab_handler_FillSpatialGridGeometry


    subroutine xdmf_contiguous_hyperslab_handler_FillSpatialGridDescriptor(this, UniformGridNodes)
    !-----------------------------------------------------------------
    !< Fill Spatial Grid Descriptor From a FoX DOM UniformGrid node list
    !< given the Spatial Grid Node
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this             !< XDMF contiguous hyperslab handler
        type(NodeList), pointer,                    intent(IN)    :: UniformGridNodes !< Fox DOM Grid node list
        type(Node),     pointer                                   :: UniformGridNode  !< Fox DOM Grid node
        type(Node),     pointer                                   :: ChildNode        !< Fox DOM node
        type(NodeList), pointer                                   :: AttributeNodes   !< Fox DOM Attribute node list
        type(xdmf_grid_t)                                         :: Grid             !< XDMF Grid derived type
        type(xdmf_geometry_t)                                     :: Geometry         !< XDMF Topology derived type
        type(xdmf_attribute_t)                                    :: Attribute        !< XDMF Attribute derived type
        integer(I4P)                                              :: i                !< Index for a loop in UniformGridNodes
    !----------------------------------------------------------------- 
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
            call Grid%Free()
            call Geometry%Free()
            call Attribute%Free()
        endif
    end subroutine xdmf_contiguous_hyperslab_handler_FillSpatialGridDescriptor


    subroutine xdmf_contiguous_hyperslab_handler_ParseFile(this)
    !-----------------------------------------------------------------
    !< Parse a readed file and distribute the information
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this             !< XDMF contiguous hyperslab handler
        type(Node),     pointer                                   :: DocumentRootNode !< Fox DOM Document Root node
        type(Node),     pointer                                   :: DomainNode       !< Fox DOM Domain node
        type(Node),     pointer                                   :: SpatialGridNode  !< Fox DOM SpatialGrid node
        type(NodeList), pointer                                   :: UniformGridNodes !< Fox DOM UniformGrid node list
    !----------------------------------------------------------------- 
        if(this%MPIEnvironment%is_root()) then
            call this%file%parsefile()
            if(getNodeType(this%file%get_document_root())==DOCUMENT_NODE) then
                DocumentRootNode => getDocumentElement(this%file%get_document_root())
                DomainNode => this%GetUniqueNodeByTag(FatherNode = DocumentRootNode, Tag = 'Domain')
                ! Get Domain Node
                if(.not. associated(DomainNode)) return
                SpatialGridNode => this%GetFirstChildByTag(FatherNode = DomainNode, Tag = 'Grid')
                ! Get Spatial Grid Node
                if(.not. associated(SpatialGridNode)) return
                UniformGridNodes => getElementsByTagname(SpatialGridNode, 'Grid')
                ! Get Fill Spatial Grid metainfo
                if(.not. associated(UniformGridNodes)) return
                call this%FillSpatialGridDescriptor(UniformGridNodes=UniformGridNodes)
            endif
            call destroy(this%file%get_document_root())
        endif
        call this%SpatialGridDescriptor%DistributeData()
    end subroutine xdmf_contiguous_hyperslab_handler_ParseFile


    subroutine xdmf_contiguous_hyperslab_handler_CloseFile(this)
    !-----------------------------------------------------------------
    !< Close a XDMF file for the contiguous HyperSlab strategy
    !< @TODO: inherited procedure
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler
        type(xdmf_grid_t)                                         :: grid !< XDMF Grid type
        type(xdmf_domain_t)                                       :: domain   !< XDMF Domain type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            select case(this%action)
                case(XDMF_ACTION_WRITE)
                    call grid%close(xml_handler=this%file%xml_handler)
                    call domain%close(xml_handler = this%file%xml_handler)
                    call this%file%closefile()
            end select
        endif
    end subroutine xdmf_contiguous_hyperslab_handler_CloseFile


    subroutine xdmf_contiguous_hyperslab_handler_Serialize(this)
    !-----------------------------------------------------------------
    !< Serialize the topology, geometry and attribute metadata to a XDMF file
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this  !< XDMF contiguous hyperslab handler
        integer(I4P)                                              :: IDidx !< GridID idex
    !----------------------------------------------------------------- 
        do IDidx=0, this%MPIEnvironment%get_comm_size()-1
            call this%OpenGrid(GridID = IDidx)
            call this%WriteTopology(GridID = IDidx)
            call this%WriteGeometry(GridID = IDidx)
            call this%WriteAttributes(GridID = IDidx)
            call this%CloseGrid(GridID = IDidx)
        enddo
    end subroutine xdmf_contiguous_hyperslab_handler_Serialize

    subroutine xdmf_contiguous_hyperslab_handler_WriteTopology(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Topology into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this                    !< XDMF contiguous hyperslab handler
        integer(I4P),          optional,            intent(IN)    :: GridID                  !< Grid ID number
        type(xdmf_topology_t)                                     :: topology                !< XDMF Topology type
        type(xdmf_dataitem_t)                                     :: dataitem                !< XDMF Dataitem type
        type(xdmf_character_data_t)                               :: chardata                !< XDMF Character Data type
        integer(I8P)                                              :: LocalNumberOfElements   !< Local number of elements
        integer(I8P)                                              :: GlobalConnectivitySize  !< Global connectivity size
        integer(I8P)                                              :: Start                   !< Hyperslab start
        integer(I8P)                                              :: Count                   !< Hyperslab count
        character(len=:), allocatable                             :: XMDFTopologyTypeName    !< String topology type identifier
    !-----------------------------------------------------------------
    !< @Note: allow different Topology or Topology for each part of the spatial grid?
        if(this%MPIEnvironment%is_root()) then
            if(present(GridID)) then
                LocalNumberOfElements = this%SpatialGridDescriptor%GetNumberOfElementsFromGridID(ID=GridID)
                Start = this%SpatialGridDescriptor%GetConnectivitySizeOffsetFromGridID(ID=GridID)
            else
                LocalNumberOfElements = this%UniformGridDescriptor%GetNumberOfElements()
                Start = 0
            endif
            GlobalConnectivitySize = this%SpatialGridDescriptor%GetGlobalConnectivitySize()
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(this%UniformGridDescriptor%getTopologyType())
            Count = this%SpatialGridDescriptor%GetConnectivitySizeFromGridID(ID=GridID)

            call topology%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/LocalNumberOfelements/),         &
                    TopologyType=XMDFTopologyTypeName)
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/Count/),&
                    ItemType    = 'HyperSlab',&
                    Format      = 'HDF')
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions     = (/3_I4P,this%UniformGridDescriptor%GetTopologyArrayDimensions()/),&
                    NumberType     = 'Int',&
                    Format         = 'XML',&
                    Precision      = 4 ) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = (/Start,1_I8P,Count/) )
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/GlobalConnectivitySize/),&
                    NumberType  = 'Int',&
                    Format      = 'HDF',& 
                    Precision   = this%UniformGridDescriptor%GetTopologyPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//this%UniformGridDescriptor%GetTopologyName() )
            call dataitem%close(xml_handler=this%file%xml_handler)
            call dataitem%close(xml_handler=this%file%xml_handler)
            call topology%close(xml_handler=this%file%xml_handler)
        endif                    
    end subroutine xdmf_contiguous_hyperslab_handler_WriteTopology


    subroutine xdmf_contiguous_hyperslab_handler_WriteGeometry(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XDMF contiguous hyperslab handler
        integer(I4P), optional,          intent(IN)    :: GridID                !< Grid ID number
        type(xdmf_geometry_t)                          :: geometry              !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem              !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata              !< XDMF Character Data type
        integer(I8P)                                   :: LocalNumberOfNodes    !< Local number of nodes
        integer(I8P)                                   :: GlobalNumberOfNodes   !< Global number of nodes
        integer(I4P)                                   :: SpaceDimension        !< Space dimension
        integer(I8P)                                   :: Start                 !< Hyperslab start
        integer(I8P)                                   :: Count                 !< Hyperslab count
        character(len=:), allocatable                  :: XDMFGeometryTypeName  !< String geometry type identifier
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            SpaceDimension = GetSpaceDimension(this%UniformGridDescriptor%getGeometryType())
            if(present(GridID)) then
                LocalNumberOfNodes = this%SpatialGridDescriptor%GetNumberOfNodesFromGridID(ID=GridID)
                Start = this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=GridID)*SpaceDimension
            else
                localNumberOfNodes = this%UniformGridDescriptor%GetNumberOfNodes()
                Start = 0
            endif
            GlobalNumberOfNodes = this%SpatialGridDescriptor%GetGlobalNumberOfNodes()
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(this%UniformGridDescriptor%GetGeometryType())
            Count = LocalNumberOfNodes*SpaceDimension
            call geometry%open( xml_handler  = this%file%xml_handler, &
                    GeometryType = XDMFGeometryTypeName)
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/localNumberOfNodes*SpaceDimension/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I4P,this%UniformGridDescriptor%GetGeometryArrayDimensions()/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = (/Start,1_I8P,Count/) )
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/GlobalNumberOfNodes*SpaceDimension/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%close(xml_handler = this%file%xml_handler)
            call geometry%close(xml_handler = this%file%xml_handler)
        endif                    
    end subroutine xdmf_contiguous_hyperslab_handler_WriteGeometry


    subroutine xdmf_contiguous_hyperslab_handler_WriteAttributes(this, GridID)
    !-----------------------------------------------------------------
    !< Writes a XDMF Attribute into a opened file for the contiguous HyperSlab strategy
    !< @NOTE: only nodal attributes
    !< @TODO: add cell, face and grid centered attributes
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this                   !< XDMF contiguous hyperslab handler
        integer(I4P), optional,                     intent(IN)    :: GridID                 !< Grid ID number
        type(xdmf_attribute_t)                                    :: attribute              !< XDMF Attribute type
        type(xdmf_dataitem_t)                                     :: dataitem               !< XDMF Dataitem type
        type(xdmf_character_data_t)                               :: chardata               !< XDMF Character Data type
        integer(I8P)                                              :: LocalNumberOfData      !< Local number of data
        integer(I8P)                                              :: GlobalNumberOfData     !< Global number of nodes
        integer(I8P)                                              :: DataOffset             !< DataOffset
        integer(I4P)                                              :: NumberOfComponents     !< Number of components given attribute type
        character(len=:), allocatable                             :: XDMFAttributeTypeName  !< String Attibute type identifier
        character(len=:), allocatable                             :: XDMFCenterTypeName     !< String Attribute Center identifier
        integer(I4P)                                              :: indx           
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            do indx = 1, this%UniformGridDescriptor%GetNumberOfAttributes()
                if(present(GridID)) then
                    call this%CalculateHyperSlabDimensions(                                           & 
                        GridID = GridID,                                                              &
                        Center = this%UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx), &
                        GlobalNumberOfData = GlobalNumberOfData,                                      &
                        LocalNumberOfData = LocalNumberOfData,                                        &
                        DataOffset = DataOffset)
                else
                    call this%CalculateHyperSlabDimensions(                                           &
                        GridID = this%MPIEnvironment%get_rank(),                                      &
                        Center = this%UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx), &
                        GlobalNumberOfData = GlobalNumberOfData,                                      &
                        LocalNumberOfData = LocalNumberOfData,                                        &
                        DataOffset = DataOffset)
                endif
                NumberOfComponents = GetNumberOfComponentsFromAttributeType( &
                                        this%UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFAttributeTypeName = GetXDMFAttributeTypeName( &
                                        this%UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFCenterTypeName = GetXDMFCenterTypeName( &
                                        this%UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx))
                call attribute%open(xml_handler = this%file%xml_handler,                                   &
                        Name          = this%UniformGridDescriptor%GetAttributeName(AttributeNumber=indx), &
                        AttributeType = XDMFAttributeTypeName,                                             &
                        Center        = XDMFCenterTypeName)
                call dataitem%open(xml_handler = this%file%xml_handler,                           &
                        Dimensions = (/int(LocalNumberOfData*int(NumberOfComponents,I8P),I8P)/),  &
                        ItemType   = 'HyperSlab',                                                 &
                        Format     = 'HDF')
                call dataitem%open(xml_handler = this%file%xml_handler,              &
                        Dimensions = (/3_I4P,this%UniformGridDescriptor%GetAttributeArrayDimensions(AttributeNumber=indx)/), &
                        NumberType = 'Int', &
                        Format     = 'XML', &
                        Precision=4) 
                call chardata%write( xml_handler = this%file%xml_handler, &
                        Data = (/DataOffset*int(NumberOfComponents,I8P),1_I8P,LocalNumberOfData*int(NumberOfComponents,I8P)/))
                call dataitem%close(xml_handler = this%file%xml_handler)
                call dataitem%open(xml_handler = this%file%xml_handler,                                    &
                        Dimensions = (/int(GlobalNumberOfData,I8P)*int(NumberOfComponents,I8P)/),          &
                        NumberType = this%UniformGridDescriptor%GetAttributeDataType(AttributeNumber=indx), &
                        Format     = 'HDF',                                                                &
                        Precision  = this%UniformGridDescriptor%GetAttributePrecision(AttributeNumber=indx)) 
                call chardata%write( xml_handler = this%file%xml_handler, &
                        Data = trim(adjustl(this%prefix))//'.h5'//':'//&
                                        this%UniformGridDescriptor%GetAttributeName(AttributeNumber=indx))
                call dataitem%close(xml_handler = this%file%xml_handler)
                call dataitem%close(xml_handler = this%file%xml_handler)
                call attribute%close(xml_handler = this%file%xml_handler)
            enddo
        endif                    
    end subroutine xdmf_contiguous_hyperslab_handler_WriteAttributes


end module xdmf_contiguous_hyperslab_handler
