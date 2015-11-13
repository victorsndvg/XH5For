module xdmf_unstructured_contiguous_hyperslab_handler
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
use xdmf_contiguous_hyperslab_handler

implicit none

private


    type, extends(xdmf_contiguous_hyperslab_handler_t) :: xdmf_unstructured_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous hyperslab handler for Unstructured Grids
    !----------------------------------------------------------------- 
    contains
    private
        procedure         :: SetGeometry_R4P              => xdmf_unst_contiguous_hyperslab_SetGeometry_R4P
        procedure         :: SetGeometry_R8P              => xdmf_unst_contiguous_hyperslab_SetGeometry_R8P
        procedure         :: SetTopology_I4P              => xdmf_unst_contiguous_hyperslab_SetTopology_I4P
        procedure         :: SetTopology_I8P              => xdmf_unst_contiguous_hyperslab_SetTopology_I8P
        procedure         :: WriteGeometry                => xdmf_unst_contiguous_hyperslab_WriteGeometry
        procedure         :: WriteTopology                => xdmf_unst_contiguous_hyperslab_WriteTopology
        procedure         :: FillSpatialGridTopology      => xdmf_unst_contiguous_hyperslab_FillSpatialGridTopology
        procedure         :: FillSpatialGridGeometry      => xdmf_unst_contiguous_hyperslab_FillSpatialGridGeometry
        procedure         :: FillSpatialGridDescriptor    => xdmf_unst_contiguous_hyperslab_FillSpatialGridDescriptor
        procedure, public :: ParseFile                    => xdmf_unst_contiguous_hyperslab_ParseFile
    end type xdmf_unstructured_contiguous_hyperslab_handler_t

public :: xdmf_unstructured_contiguous_hyperslab_handler_t

contains

    subroutine xdmf_unst_contiguous_hyperslab_SetGeometry_R4P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Add R4P geometry info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
        real(R4P),                                  intent(IN)    :: Coordinates(:)    !< Grid coordinates
        character(len=*),                           intent(IN)    :: Name              !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetGeometryMetadata(Name = Name, Precision=4, ArrayDimensions=1)
    end subroutine xdmf_unst_contiguous_hyperslab_SetGeometry_R4P


    subroutine xdmf_unst_contiguous_hyperslab_SetGeometry_R8P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Add R8P geometry info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
        real(R8P),                                  intent(IN)    :: Coordinates(:)    !< Grid coordinates
        character(len=*),                           intent(IN)    :: Name              !< Geometry name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetGeometryMetadata(Name=Name, Precision=8, ArrayDimensions=1)
    end subroutine xdmf_unst_contiguous_hyperslab_SetGeometry_R8P


    subroutine xdmf_unst_contiguous_hyperslab_SetTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I4P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
        integer(I4P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                           intent(IN)    :: Name              !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetTopologyMetadata(Name=Name, Precision=4, ArrayDimensions=1)
    end subroutine xdmf_unst_contiguous_hyperslab_SetTopology_I4P


    subroutine xdmf_unst_contiguous_hyperslab_SetTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I8P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
        integer(I8P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                           intent(IN)    :: Name              !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetTopologyMetadata(Name=Name, Precision=8, ArrayDimensions=1)
    end subroutine xdmf_unst_contiguous_hyperslab_SetTopology_I8P


    subroutine xdmf_unst_contiguous_hyperslab_FillSpatialGridTopology(this, TopologyNode, ID)
    !-----------------------------------------------------------------
    !< Fill the Spatial grid topology metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
        type(Node), pointer,                        intent(IN)    :: TopologyNode      !< Fox DOM Topology node
        integer(I4P),                               intent(IN)    :: ID                !< Grid IDentifier
        type(xdmf_topology_t)                                     :: Topology          !< XDMF Topology derived type
        type(xdmf_dataitem_t)                                     :: DataItem          !< XDMF DataItem derived type
        type(Node), pointer                                       :: DataItemNode      !< Fox DOM Dataitem node
        integer(I8P)                                              :: auxDims(1)        !< Aux dimensions variable
        character(len=:), allocatable                             :: XPath             !< Topology XPath
    !----------------------------------------------------------------- 
        if(.not. associated(TopologyNode)) return
        call Topology%Parse(DOMNode = TopologyNode)
        ! Set TopologyType
        call this%SpatialGridDescriptor%SetTopologyTypePerGridID(&
                    TopologyType = GetXDMFTopologyTypeFromName(Topology%get_TopologyType()), ID=ID)
        ! Set NumberOfElements
        auxDims = Topology%get_Dimensions()
        call this%SpatialGridDescriptor%SetNumberOfElementsPerGridID(AuxDims(1),ID=ID)
        ! Set ConnectivitySize
        DataItemNode => this%GetFirstChildByTag(FatherNode = TopologyNode, Tag = 'DataItem')
        call DataItem%Parse(DomNode = DataItemNode)
        auxDims = DataItem%get_Dimensions()
        call this%SpatialGridDescriptor%SetConnectivitySizePerGridID(AuxDims(1),ID=ID)
        ! Free
        call Topology%Free()
        call DataItem%Free()
        nullify(DataItemNode)
    end subroutine xdmf_unst_contiguous_hyperslab_FillSpatialGridTopology


    subroutine xdmf_unst_contiguous_hyperslab_FillSpatialGridGeometry(this, GeometryNode, ID)
    !----------------------------------------------------------------- 
    !< Fill the Spatial grid geometry metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
        type(Node), pointer,                        intent(IN)    :: GeometryNode      !< Fox DOM Geometry node
        integer(I4P),                               intent(IN)    :: ID                !< Grid IDentifier
        type(xdmf_geometry_t)                                     :: Geometry          !< XDMF Geometry derived type
        type(xdmf_dataitem_t)                                     :: DataItem          !< XDMF DataItem derived type
        type(Node), pointer                                       :: DataItemNode      !< Fox DOM Dataitem node
        integer(I8P)                                              :: auxDims(1)        !< Aux dimensions variable
        integer(I4P)                                              :: spacedims         !< Space dimensions
    !----------------------------------------------------------------- 
        if(.not. associated(GeometryNode)) return
        call Geometry%Parse(DOMNode = GeometryNode)
        ! Set GeometryType
        call this%SpatialGridDescriptor%SetGeometryTypePerGridID(&
                    GeometryType = GetXDMFGeometryTypeFromName(Geometry%get_GeometryType()),ID=ID)
        ! Set NumberOfNodes
        DataItemNode => this%GetFirstChildByTag(FatherNode = GeometryNode, Tag = 'DataItem')
        call DataItem%Parse(DomNode = DataItemNode)
        auxDims = DataItem%get_Dimensions()
        spacedims = GetSpaceDimension(GetXDMFGeometryTypeFromName(Geometry%get_GeometryType()))
        call this%SpatialGridDescriptor%SetNumberOfNodesPerGridID(AuxDims(1)/spacedims,ID=ID)
        ! Free
        nullify(DataItemNode)
        call Geometry%Free()
        call DataItem%Free()
    end subroutine xdmf_unst_contiguous_hyperslab_FillSpatialGridGeometry


    subroutine xdmf_unst_contiguous_hyperslab_FillSpatialGridDescriptor(this, UniformGridNodes)
    !-----------------------------------------------------------------
    !< Fill Spatial Grid Descriptor From a FoX DOM UniformGrid node list
    !< given the Spatial Grid Node
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
        type(NodeList), pointer,                    intent(IN)    :: UniformGridNodes  !< Fox DOM Grid node list
        type(Node),     pointer                                   :: UniformGridNode   !< Fox DOM Grid node
        type(Node),     pointer                                   :: ChildNode         !< Fox DOM node
        type(NodeList), pointer                                   :: AttributeNodes    !< Fox DOM Attribute node list
        type(xdmf_grid_t)                                         :: Grid              !< XDMF Grid derived type
        type(xdmf_geometry_t)                                     :: Geometry          !< XDMF Topology derived type
        type(xdmf_attribute_t)                                    :: Attribute         !< XDMF Attribute derived type
        integer(I4P)                                              :: i                 !< Index for a loop in UniformGridNodes
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
    end subroutine xdmf_unst_contiguous_hyperslab_FillSpatialGridDescriptor


    subroutine xdmf_unst_contiguous_hyperslab_ParseFile(this)
    !-----------------------------------------------------------------
    !< Parse a readed file and distribute the information
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
        type(Node),     pointer                                   :: DocumentRootNode  !< Fox DOM Document Root node
        type(Node),     pointer                                   :: DomainNode        !< Fox DOM Domain node
        type(Node),     pointer                                   :: SpatialGridNode   !< Fox DOM SpatialGrid node
        type(NodeList), pointer                                   :: UniformGridNodes  !< Fox DOM UniformGrid node list
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
        call this%SpatialGridDescriptor%BroadcastMetadata()
    end subroutine xdmf_unst_contiguous_hyperslab_ParseFile


    subroutine xdmf_unst_contiguous_hyperslab_WriteTopology(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Topology into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XDMF contiguous hyperslab handler for Unstructured Grids
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
                LocalNumberOfElements = this%SpatialGridDescriptor%GetNumberOfElementsPerGridID(ID=GridID)
                Start = this%SpatialGridDescriptor%GetConnectivitySizeOffsetPerGridID(ID=GridID)
            else
                LocalNumberOfElements = this%UniformGridDescriptor%GetNumberOfElements()
                Start = 0
            endif
            GlobalConnectivitySize = this%SpatialGridDescriptor%GetGlobalConnectivitySize()
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(this%UniformGridDescriptor%getTopologyType())
            Count = this%SpatialGridDescriptor%GetConnectivitySizePerGridID(ID=GridID)

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
    end subroutine xdmf_unst_contiguous_hyperslab_WriteTopology


    subroutine xdmf_unst_contiguous_hyperslab_WriteGeometry(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
        integer(I4P), optional,          intent(IN)    :: GridID                       !< Grid ID number
        type(xdmf_geometry_t)                          :: geometry                     !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem                     !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata                     !< XDMF Character Data type
        integer(I8P)                                   :: LocalNumberOfNodes           !< Local number of nodes
        integer(I8P)                                   :: GlobalNumberOfNodes          !< Global number of nodes
        integer(I4P)                                   :: SpaceDimension               !< Space dimension
        integer(I8P)                                   :: Start                        !< Hyperslab start
        integer(I8P)                                   :: Count                        !< Hyperslab count
        character(len=:), allocatable                  :: XDMFGeometryTypeName         !< String geometry type identifier
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            SpaceDimension = GetSpaceDimension(this%UniformGridDescriptor%getGeometryType())
            if(present(GridID)) then
                LocalNumberOfNodes = this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=GridID)
                Start = this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=GridID)*SpaceDimension
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
    end subroutine xdmf_unst_contiguous_hyperslab_WriteGeometry

end module xdmf_unstructured_contiguous_hyperslab_handler
