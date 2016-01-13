module xdmf_unstructured_dataset_per_process_handler
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
use xdmf_dataset_per_process_handler

implicit none

private


    type, extends(xdmf_dataset_per_process_handler_t) :: xdmf_unstructured_dataset_per_process_handler_t
    !-----------------------------------------------------------------
    !< XDMF dataset per process handler for Unstructured Grids
    !----------------------------------------------------------------- 
    contains
    private
        procedure         :: SetGeometry_R4P              => xdmf_unst_dataset_per_process_SetGeometry_R4P
        procedure         :: SetGeometry_R8P              => xdmf_unst_dataset_per_process_SetGeometry_R8P
        procedure         :: SetTopology_I4P              => xdmf_unst_dataset_per_process_SetTopology_I4P
        procedure         :: SetTopology_I8P              => xdmf_unst_dataset_per_process_SetTopology_I8P
        procedure         :: WriteGeometry                => xdmf_unst_dataset_per_process_WriteGeometry
        procedure         :: WriteGeometry_XYZ            => xdmf_unst_dataset_per_process_WriteGeometry_XYZ
        procedure         :: WriteGeometry_X_Y_Z          => xdmf_unst_dataset_per_process_WriteGeometry_X_Y_Z
        procedure         :: WriteTopology                => xdmf_unst_dataset_per_process_WriteTopology
        procedure         :: FillSpatialGridTopology      => xdmf_unst_dataset_per_process_FillSpatialGridTopology
        procedure         :: FillSpatialGridGeometry      => xdmf_unst_dataset_per_process_FillSpatialGridGeometry
        procedure         :: FillSpatialGridDescriptor    => xdmf_unst_dataset_per_process_FillSpatialGridDescriptor
        procedure, public :: ParseFile                    => xdmf_unst_dataset_per_process_ParseFile
    end type xdmf_unstructured_dataset_per_process_handler_t

public :: xdmf_unstructured_dataset_per_process_handler_t

contains

    subroutine xdmf_unst_dataset_per_process_SetGeometry_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Add R4P geometry info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this   !< XDMF dataset per process handler for Unstructured Grids
        real(R4P),                                               intent(IN)    :: XYZ(:) !< Grid coordinates
        character(len=*),                                        intent(IN)    :: Name   !< Topology name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetGeometryMetadata(        &
                                            Name            = Name, &
                                            Precision       = 4,    &
                                            ArrayDimensions = (/size(XYZ, dim=1)/))
    end subroutine xdmf_unst_dataset_per_process_SetGeometry_R4P


    subroutine xdmf_unst_dataset_per_process_SetGeometry_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Add R8P geometry info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this   !< XDMF dataset per process handler for Unstructured Grids
        real(R8P),                                               intent(IN)    :: XYZ(:) !< Grid coordinates
        character(len=*),                                        intent(IN)    :: Name   !< Geometry name
    !-----------------------------------------------------------------
        call this%UniformGridDescriptor%SetGeometryMetadata(        &
                                            Name            = Name, &
                                            Precision       = 8,    &
                                            ArrayDimensions = (/size(XYZ, dim=1)/))
    end subroutine xdmf_unst_dataset_per_process_SetGeometry_R8P


    subroutine xdmf_unst_dataset_per_process_SetTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I4P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                           intent(IN)    :: Name              !< Topology name
    !-----------------------------------------------------------------
        call this%SpatialGridDescriptor%SetTopologySizePerGridID(                               &
                                            TopologySize = int(size(connectivities,dim=1),I8P), &
                                            ID           = this%MPIEnvironment%get_rank())
        call this%UniformGridDescriptor%SetTopologyMetadata(        &
                                            Name            = Name, &
                                            Precision       = 4,    &
                                            ArrayDimensions = (/size(Connectivities, dim=1)/))
    end subroutine xdmf_unst_dataset_per_process_SetTopology_I4P


    subroutine xdmf_unst_dataset_per_process_SetTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I8P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this !< XDMF dataset per process handler for Unstructured Grids
        integer(I8P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                           intent(IN)    :: Name              !< Topology name
    !-----------------------------------------------------------------
        call this%SpatialGridDescriptor%SetTopologySizePerGridID(                               &
                                            TopologySize = int(size(connectivities,dim=1),I8P), &
                                            ID           = this%MPIEnvironment%get_rank())
        call this%UniformGridDescriptor%SetTopologyMetadata(        &
                                            Name            = Name, &
                                            Precision       = 8,    &
                                            ArrayDimensions = (/size(Connectivities, dim=1)/))
    end subroutine xdmf_unst_dataset_per_process_SetTopology_I8P


    subroutine xdmf_unst_dataset_per_process_FillSpatialGridTopology(this, TopologyNode, ID)
    !-----------------------------------------------------------------
    !< Fill the Spatial grid topology metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this !< XDMF dataset per process handler for Unstructured Grids
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
        call this%SpatialGridDescriptor%SetTopologySizePerGridID(AuxDims(1),ID=ID)
        ! Free
        call Topology%Free()
        call DataItem%Free()
        nullify(DataItemNode)
    end subroutine xdmf_unst_dataset_per_process_FillSpatialGridTopology


    subroutine xdmf_unst_dataset_per_process_FillSpatialGridGeometry(this, GeometryNode, ID)
    !----------------------------------------------------------------- 
    !< Fill the Spatial grid geometry metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this !< XDMF dataset per process handler for Unstructured Grids
        type(Node), pointer,                        intent(IN)    :: GeometryNode      !< Fox DOM Geometry node
        integer(I4P),                               intent(IN)    :: ID                !< Grid IDentifier
        type(xdmf_geometry_t)                                     :: Geometry          !< XDMF Geometry derived type
        type(xdmf_dataitem_t)                                     :: DataItem          !< XDMF DataItem derived type
        type(Node), pointer                                       :: DataItemNode      !< Fox DOM Dataitem node
        integer(I8P)                                              :: auxDims(1)        !< Aux dimensions variable
        integer(I4P)                                              :: spacedims         !< Space dimensions
        integer(I4P)                                              :: GeometryType      !< GeometryType
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
    end subroutine xdmf_unst_dataset_per_process_FillSpatialGridGeometry


    subroutine xdmf_unst_dataset_per_process_FillSpatialGridDescriptor(this, UniformGridNodes)
    !-----------------------------------------------------------------
    !< Fill Spatial Grid Descriptor From a FoX DOM UniformGrid node list
    !< given the Spatial Grid Node
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this !< XDMF dataset per process handler for Unstructured Grids
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
    end subroutine xdmf_unst_dataset_per_process_FillSpatialGridDescriptor


    subroutine xdmf_unst_dataset_per_process_ParseFile(this)
    !-----------------------------------------------------------------
    !< Parse a readed file and distribute the information
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this !< XDMF dataset per process handler for Unstructured Grids
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
    end subroutine xdmf_unst_dataset_per_process_ParseFile


    subroutine xdmf_unst_dataset_per_process_WriteTopology(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Topology into a opened file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this       !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                               intent(IN)    :: GridID                  !< Grid ID number
        type(xdmf_topology_t)                                     :: topology                !< XDMF Topology type
        type(xdmf_dataitem_t)                                     :: dataitem                !< XDMF Dataitem type
        type(xdmf_character_data_t)                               :: chardata                !< XDMF Character Data type
        integer(I8P)                                              :: LocalNumberOfElements   !< Local number of elements
        integer(I8P)                                              :: GlobalConnectivitySize  !< Global connectivity size
        integer(I8P)                                              :: Start                   !< Hyperslab start
        integer(I8P)                                              :: TopologySize            !< Topology size
        character(len=:), allocatable                             :: XMDFTopologyTypeName    !< String topology type identifier
        integer(I4P)                                              :: DimensionsSize          !< Size of the topology shape
    !-----------------------------------------------------------------
    !< @Note: allow different Topology or Topology for each part of the spatial grid?
        if(this%MPIEnvironment%is_root()) then
            LocalNumberOfElements = this%SpatialGridDescriptor%GetNumberOfElementsPerGridID(ID=GridID)
            Start = this%SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=GridID)
            GlobalConnectivitySize = this%SpatialGridDescriptor%GetGlobalTopologySize()
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(this%UniformGridDescriptor%getTopologyType())
            TopologySize = this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID)
            DimensionsSize = size(this%UniformGridDescriptor%GetTopologyArrayDimensions(), dim=1)
            call topology%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/LocalNumberOfelements/),         &
                    TopologyType=XMDFTopologyTypeName)
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/TopologySize/),                  &
                    NumberType  = 'Int',                             &
                    Format      = 'HDF',                             & 
                    Precision   = this%UniformGridDescriptor%GetTopologyPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//this%UniformGridDescriptor%GetTopologyName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler=this%file%xml_handler)
            call topology%close(xml_handler=this%file%xml_handler)
        endif                    
    end subroutine xdmf_unst_dataset_per_process_WriteTopology


    subroutine xdmf_unst_dataset_per_process_WriteGeometry(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this   !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                                            intent(IN)    :: GridID !< Grid ID number
    !----------------------------------------------------------------- 
        select case(this%UniformGridDescriptor%GetGeometryType())
            case (XDMF_GEOMETRY_TYPE_XY, XDMF_GEOMETRY_TYPE_XYZ)
                call this%WriteGeometry_XYZ(GridID=GridID)
            case (XDMF_GEOMETRY_TYPE_X_Y_Z)
                call this%WriteGeometry_X_Y_Z(GridID=GridID)
        end select
    end subroutine xdmf_unst_dataset_per_process_WriteGeometry


    subroutine xdmf_unst_dataset_per_process_WriteGeometry_XYZ(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF XY[Z] Geometry into a opened file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                    intent(IN)    :: GridID                       !< Grid ID number
        type(xdmf_geometry_t)                          :: geometry                     !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem                     !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata                     !< XDMF Character Data type
        integer(I8P)                                   :: LocalGeometrySize            !< Local geometry size
        integer(I8P)                                   :: GlobalGeometrySize           !< Global geometrySize
        integer(I4P)                                   :: SpaceDimension               !< Space dimension
        integer(I8P)                                   :: Start                        !< Hyperslab start
        integer(I8P)                                   :: Count                        !< Hyperslab count
        character(len=:), allocatable                  :: XDMFGeometryTypeName         !< String geometry type identifier
        integer(I4P)                                   :: DimensionsSize               !< Size of the geometry shape
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            LocalGeometrySize = this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID)
            Start = this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID)
            GlobalGeometrySize = this%SpatialGridDescriptor%GetGlobalGeometrySize()
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(this%UniformGridDescriptor%GetGeometryType())
            Count = LocalGeometrySize
            DimensionsSize = size(this%UniformGridDescriptor%GetGeometryArrayDimensions(), dim=1)
            call geometry%open( xml_handler  = this%file%xml_handler, &
                    GeometryType = XDMFGeometryTypeName)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/LocalGeometrySize/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//this%UniformGridDescriptor%GetGeometryName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler = this%file%xml_handler)
            call geometry%close(xml_handler = this%file%xml_handler)
        endif                    
    end subroutine xdmf_unst_dataset_per_process_WriteGeometry_XYZ


    subroutine xdmf_unst_dataset_per_process_WriteGeometry_X_Y_Z(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                    intent(IN)    :: GridID                       !< Grid ID number
        type(xdmf_geometry_t)                          :: geometry                     !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem                     !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata                     !< XDMF Character Data type
        integer(I8P)                                   :: LocalNumberOfNodes           !< Number of nodes of the uniform grid
        integer(I8P)                                   :: GlobalNumberOfNodes          !< Number of nodes of the spatial grid
        integer(I4P)                                   :: SpaceDimension               !< Space dimension
        integer(I8P)                                   :: Start                        !< Hyperslab start
        integer(I8P)                                   :: Count                        !< Hyperslab count
        character(len=:), allocatable                  :: XDMFGeometryTypeName         !< String geometry type identifier
        integer(I4P)                                   :: DimensionsSize               !< Size of the geometry shape
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            LocalNumberOfNodes = this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=GridID)
            Start = this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=GridID)
            GlobalNumberOfNodes = this%SpatialGridDescriptor%GetGlobalNumberOfNodes()
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(this%UniformGridDescriptor%GetGeometryType())
            Count = LocalNumberOfNodes
            call geometry%open( xml_handler  = this%file%xml_handler, &
                    GeometryType = XDMFGeometryTypeName)
            DimensionsSize = size(this%UniformGridDescriptor%GetGeometryArrayDimensions(), dim=1)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/LocalNumberOfNodes/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':X_'//this%UniformGridDescriptor%GetGeometryName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler = this%file%xml_handler)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/LocalNumberOfNodes/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':Y_'//this%UniformGridDescriptor%GetGeometryName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler = this%file%xml_handler)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/LocalNumberOfNodes/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':Z_'//this%UniformGridDescriptor%GetGeometryName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler = this%file%xml_handler)

            call geometry%close(xml_handler = this%file%xml_handler)
        endif                    
    end subroutine xdmf_unst_dataset_per_process_WriteGeometry_X_Y_Z


end module xdmf_unstructured_dataset_per_process_handler
