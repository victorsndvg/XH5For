module xdmf_structured_contiguous_hyperslab_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use IR_Precision, only: I4P, I8P, R4P, R8P, str
use xh5for_parameters
use xh5for_utils
use fox_xdmf
use fox_dom,      only: Node, NodeList, ParseFile, GetDocumentElement, Item, GetLength, GetChildNodes, getAttribute, &
                        HasChildNodes, GetElementsByTagName, GetNodeType, GetTagName, Destroy, getTextContent, &
                        TEXT_NODE, DOCUMENT_NODE
use xdmf_contiguous_hyperslab_handler

implicit none

private


    type, extends(xdmf_contiguous_hyperslab_handler_t) :: xdmf_structured_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous hyperslab handler for structured Grids
    !----------------------------------------------------------------- 
    contains
    private
        procedure         :: SetGeometry_R4P              => xdmf_str_contiguous_hyperslab_SetGeometry_R4P
        procedure         :: SetGeometry_R8P              => xdmf_str_contiguous_hyperslab_SetGeometry_R8P
        procedure         :: SetTopology_I4P              => xdmf_str_contiguous_hyperslab_SetTopology_I4P
        procedure         :: SetTopology_I8P              => xdmf_str_contiguous_hyperslab_SetTopology_I8P
        procedure         :: WriteGeometry                => xdmf_str_contiguous_hyperslab_WriteGeometry
        procedure         :: WriteGeometry_ORIGIN_DXDYDZ  => xdmf_str_contiguous_hyperslab_WriteGeometry_ORIGIN_DXDYDZ
        procedure         :: WriteGeometry_VXVYVZ         => xdmf_str_contiguous_hyperslab_WriteGeometry_VXVYVZ
        procedure         :: WriteTopology                => xdmf_str_contiguous_hyperslab_WriteTopology
        procedure         :: FillSpatialGridTopology      => xdmf_str_contiguous_hyperslab_FillSpatialGridTopology
        procedure         :: FillSpatialGridGeometry      => xdmf_str_contiguous_hyperslab_FillSpatialGridGeometry
        procedure         :: FillSpatialGridDescriptor    => xdmf_str_contiguous_hyperslab_FillSpatialGridDescriptor
        procedure, public :: ParseFile                    => xdmf_str_contiguous_hyperslab_ParseFile
    end type xdmf_structured_contiguous_hyperslab_handler_t

public :: xdmf_structured_contiguous_hyperslab_handler_t

contains

    subroutine xdmf_str_contiguous_hyperslab_SetGeometry_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Add R4P geometry info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XDMF contiguous hyperslab handler for structured Grids
        real(R4P),                                             intent(IN)    :: XYZ(:) !< Grid coordinates
        character(len=*),                                      intent(IN)    :: Name   !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetGeometryMetadata(Name            = Name, &
                                                            Precision       = 4,    &
                                                            ArrayDimensions = (/size(XYZ, dim=1)/))
    end subroutine xdmf_str_contiguous_hyperslab_SetGeometry_R4P


    subroutine xdmf_str_contiguous_hyperslab_SetGeometry_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Add R8P geometry info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XDMF contiguous hyperslab handler for structured Grids
        real(R8P),                                             intent(IN)    :: XYZ(:) !< Grid coordinates
        character(len=*),                                      intent(IN)    :: Name   !< Geometry name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetGeometryMetadata(Name            = Name, &
                                                            Precision       = 8,    &
                                                            ArrayDimensions = (/size(XYZ, dim=1)/))
    end subroutine xdmf_str_contiguous_hyperslab_SetGeometry_R8P


    subroutine xdmf_str_contiguous_hyperslab_SetTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I4P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler for structured Grids
        integer(I4P),                                          intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                                      intent(IN)    :: Name              !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetTopologyMetadata(Name            = Name, &
                                                            Precision       = 4,    &
                                                            ArrayDimensions = (/size(Connectivities, dim=1)/))
    end subroutine xdmf_str_contiguous_hyperslab_SetTopology_I4P


    subroutine xdmf_str_contiguous_hyperslab_SetTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I8P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler for structured Grids
        integer(I8P),                                          intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                                      intent(IN)    :: Name              !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetTopologyMetadata(Name            = Name, &
                                                            Precision       = 8,    &
                                                            ArrayDimensions = (/size(Connectivities, dim=1)/))
    end subroutine xdmf_str_contiguous_hyperslab_SetTopology_I8P


    subroutine xdmf_str_contiguous_hyperslab_FillSpatialGridTopology(this, TopologyNode, ID)
    !-----------------------------------------------------------------
    !< Fill the Spatial grid topology metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XDMF contiguous hyperslab handler for structured Grids
        type(Node), pointer,                                   intent(IN)    :: TopologyNode !< Fox DOM Topology node
        integer(I4P),                                          intent(IN)    :: ID           !< Grid IDentifier
        type(xdmf_topology_t)                                                :: Topology     !< XDMF Topology derived type
        type(xdmf_dataitem_t)                                                :: DataItem     !< XDMF DataItem derived type
        type(Node), pointer                                                  :: DataItemNode !< Fox DOM Dataitem node
        integer(I8P)                                                         :: auxDims(1)   !< Aux dimensions variable
        character(len=:), allocatable                                        :: XPath        !< Topology XPath
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
        call this%SpatialGridDescriptor%SetTopologySizePerGridID(AuxDims(1),ID=ID)
        ! Free
        call Topology%Free()
        call DataItem%Free()
        nullify(DataItemNode)
    end subroutine xdmf_str_contiguous_hyperslab_FillSpatialGridTopology


    subroutine xdmf_str_contiguous_hyperslab_FillSpatialGridGeometry(this, GeometryNode, ID)
    !----------------------------------------------------------------- 
    !< Fill the Spatial grid geometry metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for structured Grids
        type(Node), pointer,                        intent(IN)    :: GeometryNode !< Fox DOM Geometry node
        integer(I4P),                               intent(IN)    :: ID           !< Grid IDentifier
        type(xdmf_geometry_t)                                     :: Geometry     !< XDMF Geometry derived type
        type(xdmf_dataitem_t)                                     :: DataItem     !< XDMF DataItem derived type
        type(Node), pointer                                       :: DataItemNode !< Fox DOM Dataitem node
        integer(I8P)                                              :: auxDims(1)   !< Aux dimensions variable
        integer(I4P)                                              :: spacedims    !< Space dimensions
        integer(I4P)                                              :: GeometryType !< GeometryType
    !----------------------------------------------------------------- 
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
    end subroutine xdmf_str_contiguous_hyperslab_FillSpatialGridGeometry


    subroutine xdmf_str_contiguous_hyperslab_FillSpatialGridDescriptor(this, UniformGridNodes)
    !-----------------------------------------------------------------
    !< Fill Spatial Grid Descriptor From a FoX DOM UniformGrid node list
    !< given the Spatial Grid Node
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XDMF contiguous hyperslab handler for structured Grids
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
    end subroutine xdmf_str_contiguous_hyperslab_FillSpatialGridDescriptor


    subroutine xdmf_str_contiguous_hyperslab_ParseFile(this)
    !-----------------------------------------------------------------
    !< Parse a readed file and distribute the information
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XDMF contiguous hyperslab handler for structured Grids
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
    end subroutine xdmf_str_contiguous_hyperslab_ParseFile


    subroutine xdmf_str_contiguous_hyperslab_WriteTopology(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Topology into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XDMF contiguous hyperslab handler for structured Grids
        integer(I4P),                               intent(IN)    :: GridID                  !< Grid ID number
        type(xdmf_topology_t)                                     :: topology                !< XDMF Topology type
        integer(I8P)                                              :: GridShape(3)            !< Local number of elements
        character(len=:), allocatable                             :: XMDFTopologyTypeName    !< String topology type identifier
        integer(I4P)                                              :: DimensionsSize          !< Size fo the topology dimensions
    !-----------------------------------------------------------------
    !< @Note: allow different Topology or Topology for each part of the spatial grid?
        if(this%MPIEnvironment%is_root()) then
            ! Topology Grid shape is expressed in ZYX order in structured grids
            GridShape(1) = this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID, Dimension=3)
            GridShape(2) = this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID, Dimension=2)
            GridShape(3) = this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID, Dimension=1)
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(this%SpatialGridDescriptor%getTopologyTypePerGridID(id=GridID))

            call topology%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = GridShape,                         &
                    TopologyType=XMDFTopologyTypeName)
            call topology%close(xml_handler=this%file%xml_handler)
        endif                    
    end subroutine xdmf_str_contiguous_hyperslab_WriteTopology


    subroutine xdmf_str_contiguous_hyperslab_WriteGeometry(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XDMF contiguous hyperslab handler for structured Grids
        integer(I4P),                                          intent(IN)    :: GridID !< Grid ID number
    !----------------------------------------------------------------- 
        select case(this%UniformGridDescriptor%GetGeometryType())

            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY, XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                call this%WriteGeometry_ORIGIN_DXDYDZ(GridID=GridID)
            case (XDMF_GEOMETRY_TYPE_VXVYVZ)
                call this%WriteGeometry_VXVYVZ(GridID=GridID)
        end select
    end subroutine xdmf_str_contiguous_hyperslab_WriteGeometry


    subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_ORIGIN_DXDYDZ(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF XY[Z] Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for structured Grids
        integer(I4P),                    intent(IN)    :: GridID                  !< Grid ID number
        type(xdmf_geometry_t)                          :: geometry                !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem                !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata                !< XDMF Character Data type
        integer(I4P)                                   :: GridNumber              !< NumberOfGrids
        integer(I4P)                                   :: NumberOfGrids           !< NumberOfGrids
        character(len=:), allocatable                  :: XDMFGeometryTypeName    !< String geometry type identifier
        integer(I4P)                                   :: DimensionsSize          !< Size of the GeometryDimensions
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=GridID))
            GridNumber = GridID
            NumberOfGrids = this%SpatialGridDescriptor%GetNumberOfGrids()
            call geometry%open( xml_handler  = this%file%xml_handler, &
                    GeometryType = XDMFGeometryTypeName)
            ! Origin
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/3/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I4P, 1/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = (/3_I8P*int(GridNumber,I8P),1_I8P, 3_I8P/) )
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I8P*int(NumberOfGrids,I8P)/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//'Origin_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%close(xml_handler = this%file%xml_handler)
            ! DXDYXDZ
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/3/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I4P, 1/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = (/3_I8P*int(GridNumber,I8P),1_I8P, 3_I8P/) )
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I8P*int(NumberOfGrids,I8P)/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//'DxDyDz_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%close(xml_handler = this%file%xml_handler)
            call geometry%close(xml_handler = this%file%xml_handler)
        endif                    
    end subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_ORIGIN_DXDYDZ


    subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_VXVYVZ(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for structured Grids
        integer(I4P),                    intent(IN)    :: GridID                  !< Grid ID number
        type(xdmf_geometry_t)                          :: geometry                !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem                !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata                !< XDMF Character Data type
        integer(I8P)                                   :: LocalGridShape(3)       !< Uniform Grid shape
        integer(I8P)                                   :: GlobalGridShape(3)      !< Spatial grid shpae
        integer(I8P)                                   :: GridShapeOffset(3)      !< Grid shape offset
        character(len=:), allocatable                  :: XDMFGeometryTypeName    !< String geometry type identifier
        Integer(I4P)                                   :: DimensionsSize          !< Size of the Geometry shape
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            LocalGridShape(1) = this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=1)
            LocalGridShape(2) = this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=2)
            LocalGridShape(3) = this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=3)
            GlobalGridShape(1) = this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=1)
            GlobalGridShape(2) = this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=2)
            GlobalGridShape(3) = this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=3)
            GridShapeOffset(1) = this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID, Dimension=1)
            GridShapeOffset(2) = this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID, Dimension=2)
            GridShapeOffset(3) = this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID, Dimension=3)
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=GridID))
            call geometry%open( xml_handler  = this%file%xml_handler, &
                    GeometryType = XDMFGeometryTypeName)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/LocalGridShape(1)/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I4P, 1/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = (/GridShapeOffset(1),1_I8P,LocalGridShape(1)/) )
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/GlobalGridShape(1)/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':X_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%close(xml_handler = this%file%xml_handler)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/LocalGridShape(2)/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I4P, 1/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = (/GridShapeOffset(2),1_I8P,LocalGridShape(2)/) )
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/GlobalGridShape(2)/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':Y_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%close(xml_handler = this%file%xml_handler)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/LocalGridShape(3)/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I4P, 1/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = (/GridShapeOffset(3),1_I8P,LocalGridShape(3)/) )
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/GlobalGridShape(3)/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':Z_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%close(xml_handler = this%file%xml_handler)

            call geometry%close(xml_handler = this%file%xml_handler)
        endif                    
    end subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_VXVYVZ


end module xdmf_structured_contiguous_hyperslab_handler
