module xdmf_unstructured_contiguous_hyperslab_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF File handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use fox_xdmf
use xh5for_utils
use xh5for_parameters
use xdmf_contiguous_hyperslab_handler
use fox_dom,      only: Node
use IR_Precision, only: I4P, I8P, R4P, R8P, str

implicit none

private

    type, extends(xdmf_contiguous_hyperslab_handler_t) :: xdmf_unstructured_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous hyperslab handler for Unstructured Grids
    !----------------------------------------------------------------- 
    contains
    private
        procedure         :: SetTopology_I4P              => xdmf_unst_contiguous_hyperslab_SetTopology_I4P
        procedure         :: SetTopology_I8P              => xdmf_unst_contiguous_hyperslab_SetTopology_I8P
        procedure         :: FillSpatialGridTopology      => xdmf_unst_contiguous_hyperslab_FillSpatialGridTopology
        procedure         :: WriteGeometry                => xdmf_unst_contiguous_hyperslab_WriteGeometry
        procedure         :: WriteGeometry_XYZ            => xdmf_unst_contiguous_hyperslab_WriteGeometry_XYZ
        procedure         :: WriteGeometry_X_Y_Z          => xdmf_unst_contiguous_hyperslab_WriteGeometry_X_Y_Z
        procedure         :: WriteTopology                => xdmf_unst_contiguous_hyperslab_WriteTopology
    end type xdmf_unstructured_contiguous_hyperslab_handler_t

public :: xdmf_unstructured_contiguous_hyperslab_handler_t

contains

    subroutine xdmf_unst_contiguous_hyperslab_SetTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I4P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
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
    end subroutine xdmf_unst_contiguous_hyperslab_SetTopology_I4P


    subroutine xdmf_unst_contiguous_hyperslab_SetTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I8P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
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
        call this%SpatialGridDescriptor%SetTopologySizePerGridID(AuxDims(1),ID=ID)
        ! Free
        call Topology%Free()
        call DataItem%Free()
        nullify(DataItemNode)
    end subroutine xdmf_unst_contiguous_hyperslab_FillSpatialGridTopology


    subroutine xdmf_unst_contiguous_hyperslab_WriteTopology(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Topology into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XDMF contiguous hyperslab handler for Unstructured Grids
        integer(I4P),                               intent(IN)    :: GridID                  !< Grid ID number
        type(xdmf_topology_t)                                     :: topology                !< XDMF Topology type
        type(xdmf_dataitem_t)                                     :: dataitem                !< XDMF Dataitem type
        type(xdmf_character_data_t)                               :: chardata                !< XDMF Character Data type
        integer(I8P)                                              :: LocalNumberOfElements   !< Local number of elements
        integer(I8P)                                              :: GlobalConnectivitySize  !< Global connectivity size
        integer(I8P)                                              :: Start                   !< Hyperslab start
        integer(I8P)                                              :: Count                   !< Hyperslab count
        character(len=:), allocatable                             :: XMDFTopologyTypeName    !< String topology type identifier
        integer(I4P)                                              :: DimensionsSize          !< Size of the topology shape
    !-----------------------------------------------------------------
    !< @Note: allow different Topology or Topology for each part of the spatial grid?
        if(this%MPIEnvironment%is_root()) then
            LocalNumberOfElements = this%SpatialGridDescriptor%GetNumberOfElementsPerGridID(ID=GridID)
            Start = this%SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=GridID)
            GlobalConnectivitySize = this%SpatialGridDescriptor%GetGlobalTopologySize()
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(this%UniformGridDescriptor%getTopologyType())
            Count = this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID)
            DimensionsSize = size(this%UniformGridDescriptor%GetTopologyArrayDimensions(), dim=1)
            call topology%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions  = (/LocalNumberOfelements/),         &
                    TopologyType=XMDFTopologyTypeName)
            call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions  = (/Count/),&
                    ItemType    = 'HyperSlab',&
                    Format      = 'HDF')
            call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions     = (/3_I4P, DimensionsSize/),&
                    NumberType     = 'Int',&
                    Format         = 'XML',&
                    Precision      = 4 ) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = (/Start,1_I8P,Count/) )
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions  = (/GlobalConnectivitySize/),&
                    NumberType  = 'Int',&
                    Format      = 'HDF',& 
                    Precision   = this%UniformGridDescriptor%GetTopologyPrecision()) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//this%UniformGridDescriptor%GetTopologyName() )
            call dataitem%close(xml_handler=this%SpatialFile%xml_handler)
            call dataitem%close(xml_handler=this%SpatialFile%xml_handler)
            call topology%close(xml_handler=this%SpatialFile%xml_handler)
        endif                    
    end subroutine xdmf_unst_contiguous_hyperslab_WriteTopology


    subroutine xdmf_unst_contiguous_hyperslab_WriteGeometry(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XDMF contiguous hyperslab handler for Unstructured Grids
        integer(I4P),                                            intent(IN)    :: GridID !< Grid ID number
    !----------------------------------------------------------------- 
        select case(this%UniformGridDescriptor%GetGeometryType())
            case (XDMF_GEOMETRY_TYPE_XY, XDMF_GEOMETRY_TYPE_XYZ)
                call this%WriteGeometry_XYZ(GridID=GridID)
            case (XDMF_GEOMETRY_TYPE_X_Y_Z)
                call this%WriteGeometry_X_Y_Z(GridID=GridID)
        end select
    end subroutine xdmf_unst_contiguous_hyperslab_WriteGeometry


    subroutine xdmf_unst_contiguous_hyperslab_WriteGeometry_XYZ(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF XY[Z] Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
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
            call geometry%open( xml_handler  = this%SpatialFile%xml_handler, &
                    GeometryType = XDMFGeometryTypeName)
            call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions  = (/LocalGeometrySize/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/3_I4P, DimensionsSize/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = (/Start,1_I8P,Count/) )
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/GlobalGeometrySize/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call geometry%close(xml_handler = this%SpatialFile%xml_handler)
        endif                    
    end subroutine xdmf_unst_contiguous_hyperslab_WriteGeometry_XYZ


    subroutine xdmf_unst_contiguous_hyperslab_WriteGeometry_X_Y_Z(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for Unstructured Grids
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
            call geometry%open( xml_handler  = this%SpatialFile%xml_handler, &
                    GeometryType = XDMFGeometryTypeName)
            DimensionsSize = size(this%UniformGridDescriptor%GetGeometryArrayDimensions(), dim=1)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
            call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions  = (/LocalNumberOfNodes/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/3_I4P, DimensionsSize/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = (/Start,1_I8P,Count/) )
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/GlobalNumberOfNodes/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':X_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
            call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions  = (/LocalNumberOfNodes/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/3_I4P, DimensionsSize/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = (/Start,1_I8P,Count/) )
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/GlobalNumberOfNodes/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':Y_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
            call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions  = (/LocalNumberOfNodes/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/3_I4P, DimensionsSize/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = (/Start,1_I8P,Count/) )
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/GlobalNumberOfNodes/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':Z_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)

            call geometry%close(xml_handler = this%SpatialFile%xml_handler)
        endif                    
    end subroutine xdmf_unst_contiguous_hyperslab_WriteGeometry_X_Y_Z


end module xdmf_unstructured_contiguous_hyperslab_handler
