module xdmf_structured_contiguous_hyperslab_handler
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

    type, extends(xdmf_contiguous_hyperslab_handler_t) :: xdmf_structured_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous hyperslab handler for structured Grids
    !----------------------------------------------------------------- 
    contains
    private
        procedure         :: SetTopology_I4P              => xdmf_str_contiguous_hyperslab_SetTopology_I4P
        procedure         :: SetTopology_I8P              => xdmf_str_contiguous_hyperslab_SetTopology_I8P
        procedure         :: FillSpatialGridTopology      => xdmf_str_contiguous_hyperslab_FillSpatialGridTopology
        procedure         :: WriteGeometry                => xdmf_str_contiguous_hyperslab_WriteGeometry
        procedure         :: WriteGeometry_VXVYVZ         => xdmf_str_contiguous_hyperslab_WriteGeometry_VXVYVZ
        procedure         :: WriteGeometry_DXDYDZ         => xdmf_str_contiguous_hyperslab_WriteGeometry_DXDYDZ
        procedure         :: WriteTopology                => xdmf_str_contiguous_hyperslab_WriteTopology
    end type xdmf_structured_contiguous_hyperslab_handler_t

public :: xdmf_structured_contiguous_hyperslab_handler_t

contains

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
        integer(I8P), allocatable                                            :: auxDims(:)   !< Aux dimensions variable
        character(len=:), allocatable                                        :: XPath        !< Topology XPath
    !----------------------------------------------------------------- 
        if(.not. associated(TopologyNode)) return
        call Topology%Parse(DOMNode = TopologyNode)
        ! Set TopologyType
        call this%SpatialGridDescriptor%SetTopologyTypePerGridID(&
                    TopologyType = GetXDMFTopologyTypeFromName(Topology%get_TopologyType()), ID=ID)
        ! Set NumberOfElements
        allocate(auxDims(size(Topology%get_Dimensions(),1)))
        auxDims = Topology%get_Dimensions()
        ! Dimensions are specified with the slowest varying dimension first (i.e. KJI order)
        auxDims(:) = auxDims(size(auxDims,dim=1):1:-1)
        call this%SpatialGridDescriptor%SetXSizePerGridID(AuxDims(1),ID=ID)
        call this%SpatialGridDescriptor%SetYSizePerGridID(AuxDims(2),ID=ID)
        if(size(AuxDims,1) == 3) then
            call this%SpatialGridDescriptor%SetZSizePerGridID(AuxDims(3),ID=ID)
            call this%SpatialGridDescriptor%SetNumberOfNodesPerGridID(max(1,AuxDims(1))*max(1,AuxDims(2))*max(1,AuxDims(3)),ID=ID)
            call this%SpatialGridDescriptor%SetNumberOfElementsPerGridID(max(1,AuxDims(1)-1)*max(1,AuxDims(2)-1)*max(1,AuxDims(3)-1),ID=ID)
        else
            call this%SpatialGridDescriptor%SetNumberOfNodesPerGridID(max(1,AuxDims(1))*max(1,AuxDims(2)),ID=ID)
            call this%SpatialGridDescriptor%SetNumberOfElementsPerGridID(max(1,AuxDims(1)-1)*max(1,AuxDims(2)-1),ID=ID)
        endif
        ! Free
        call Topology%Free()
        nullify(DataItemNode)
    end subroutine xdmf_str_contiguous_hyperslab_FillSpatialGridTopology


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
        integer(I4P)                                              :: SpaceDimension          !< Space dimensions
    !-----------------------------------------------------------------
    !< @Note: allow different Topology or Topology for each part of the spatial grid?
        if(this%MPIEnvironment%is_root()) then
            ! Topology Grid shape is expressed in ZYX order in structured grids
            GridShape(1) = this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID, Dimension=3)
            GridShape(2) = this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID, Dimension=2)
            GridShape(3) = this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID, Dimension=1)
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(this%SpatialGridDescriptor%GetTopologyTypePerGridID(id=GridID))
            SpaceDimension = GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(id=GridID))
            if (SpaceDimension == 2) then
                call topology%open( xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions  = GridShape(2:3),                    &
                        TopologyType=XMDFTopologyTypeName)
            else
                call topology%open( xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions  = GridShape,                         &
                        TopologyType=XMDFTopologyTypeName)
            endif
            call topology%close(xml_handler=this%SpatialFile%xml_handler)
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
                call this%WriteGeometry_DXDYDZ(GridID=GridID)
            case (XDMF_GEOMETRY_TYPE_VXVY, XDMF_GEOMETRY_TYPE_VXVYVZ)
                call this%WriteGeometry_VXVYVZ(GridID=GridID)
        end select
    end subroutine xdmf_str_contiguous_hyperslab_WriteGeometry


    subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_DXDYDZ(this, GridID)
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
        integer(I4P)                                   :: SpaceDimension          !< Space dimension
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=GridID))
            GridNumber = GridID
            NumberOfGrids = this%SpatialGridDescriptor%GetNumberOfGrids()
            SpaceDimension = GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(id=GridID))
            call geometry%open( xml_handler  = this%SpatialFile%xml_handler, &
                    GeometryType = XDMFGeometryTypeName)
            ! Origin
            call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions  = (/SpaceDimension/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/3_I4P, 1/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = (/int(SpaceDimension,I8P)*int(GridNumber,I8P),1_I8P, int(SpaceDimension,I8P)/) )
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/int(SpaceDimension,I8P)*int(NumberOfGrids,I8P)/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = this%GetHDF5FileName()//':'//'Origin_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            ! DXDYXDZ
            call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions  = (/SpaceDimension/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/3_I4P, 1/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = (/int(SpaceDimension,I8P)*int(GridNumber,I8P),1_I8P, int(SpaceDimension,I8P)/) )
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                    Dimensions = (/int(SpaceDimension,I8P)*int(NumberOfGrids,I8P)/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                    Data = this%GetHDF5FileName()//':'//'DxDyDz_'//this%UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            call geometry%close(xml_handler = this%SpatialFile%xml_handler)
        endif                    
    end subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_DXDYDZ


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
        Integer(I4P)                                   :: SpaceDimension          !< Space Dimension
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            LocalGridShape(1) = this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=1)
            LocalGridShape(2) = this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=2)
            GlobalGridShape(1) = this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=1)
            GlobalGridShape(2) = this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=2)
            GridShapeOffset(1) = this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID, Dimension=1)
            GridShapeOffset(2) = this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID, Dimension=2)
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=GridID))
            SpaceDimension = GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(id=GridID))
            if (SpaceDimension == 3) then
                LocalGridShape(3) = this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=3)
                GlobalGridShape(3) = this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=3)
                GridShapeOffset(3) = this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID, Dimension=3)
                call geometry%open( xml_handler  = this%SpatialFile%xml_handler, &
                        GeometryType = XDMFGeometryTypeName)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions  = (/LocalGridShape(1)/), &   
                        ItemType    = 'HyperSlab', &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/3_I4P, 1/), &
                        NumberType = 'Int', &
                        Format     = 'XML', &
                        Precision  = 4) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = (/GridShapeOffset(1),1_I8P,LocalGridShape(1)/) )
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/GlobalGridShape(1)/), &
                        NumberType = 'Float', &
                        Format     = 'HDF', &
                        Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = this%GetHDF5FileName()//':X_'//this%UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions  = (/LocalGridShape(2)/), &   
                        ItemType    = 'HyperSlab', &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/3_I4P, 1/), &
                        NumberType = 'Int', &
                        Format     = 'XML', &
                        Precision  = 4) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = (/GridShapeOffset(2),1_I8P,LocalGridShape(2)/) )
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/GlobalGridShape(2)/), &
                        NumberType = 'Float', &
                        Format     = 'HDF', &
                        Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = this%GetHDF5Filename()//':Y_'//this%UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions  = (/LocalGridShape(3)/), &   
                        ItemType    = 'HyperSlab', &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/3_I4P, 1/), &
                        NumberType = 'Int', &
                        Format     = 'XML', &
                        Precision  = 4) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = (/GridShapeOffset(3),1_I8P,LocalGridShape(3)/) )
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/GlobalGridShape(3)/), &
                        NumberType = 'Float', &
                        Format     = 'HDF', &
                        Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = this%GetHDF5FileName()//':Z_'//this%UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
            elseif (SpaceDimension == 2) then
            ! Why paraview need to put in inverse order?? YX
            ! 2D VXVY does not apper in the standard Model&Format
                call geometry%open( xml_handler  = this%SpatialFile%xml_handler, &
                        GeometryType = XDMFGeometryTypeName)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions  = (/LocalGridShape(2)/), &   
                        ItemType    = 'HyperSlab', &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/3_I4P, 1/), &
                        NumberType = 'Int', &
                        Format     = 'XML', &
                        Precision  = 4) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = (/GridShapeOffset(2),1_I8P,LocalGridShape(2)/) )
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/GlobalGridShape(2)/), &
                        NumberType = 'Float', &
                        Format     = 'HDF', &
                        Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = this%GetHDF5FileName()//':Y_'//this%UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions  = (/LocalGridShape(1)/), &   
                        ItemType    = 'HyperSlab', &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/3_I4P, 1/), &
                        NumberType = 'Int', &
                        Format     = 'XML', &
                        Precision  = 4) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = (/GridShapeOffset(1),1_I8P,LocalGridShape(1)/) )
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/GlobalGridShape(1)/), &
                        NumberType = 'Float', &
                        Format     = 'HDF', &
                        Precision  = this%UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = this%GetHDF5FileName()//':X_'//this%UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)

            endif
            call geometry%close(xml_handler = this%SpatialFile%xml_handler)
        endif                    
    end subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_VXVYVZ


end module xdmf_structured_contiguous_hyperslab_handler
