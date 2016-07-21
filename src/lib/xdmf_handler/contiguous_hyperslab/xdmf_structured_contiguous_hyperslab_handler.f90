module xdmf_structured_contiguous_hyperslab_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF File handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use fox_xdmf
use xh5for_utils
use xh5for_parameters
use mpi_environment
use uniform_grid_descriptor
use spatial_grid_descriptor
use xdmf_contiguous_hyperslab_handler
use fox_dom, only: Node
use PENF,    only: I4P, I8P, R4P, R8P, str

implicit none

#include "assert.i90"

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
        class(uniform_grid_descriptor_t), pointer                            :: UniformGridDescriptor  !< Uniform grid descriptor
    !-----------------------------------------------------------------
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        assert(associated(UniformGridDescriptor))
        call UniformGridDescriptor%SetTopologyMetadata(Name            = Name, &
                                                       Precision       = 4,    &
                                                       ArrayDimensions = (/size(Connectivities, dim=1, kind=I8P)/))
    end subroutine xdmf_str_contiguous_hyperslab_SetTopology_I4P


    subroutine xdmf_str_contiguous_hyperslab_SetTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I8P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler for structured Grids
        integer(I8P),                                          intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                                      intent(IN)    :: Name              !< Topology name
        class(uniform_grid_descriptor_t), pointer                            :: UniformGridDescriptor  !< Uniform grid descriptor
    !-----------------------------------------------------------------
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        assert(associated(UniformGridDescriptor))
        call UniformGridDescriptor%SetTopologyMetadata(Name            = Name, &
                                                       Precision       = 8,    &
                                                       ArrayDimensions = (/size(Connectivities, dim=1, kind=I8P)/))
    end subroutine xdmf_str_contiguous_hyperslab_SetTopology_I8P


    subroutine xdmf_str_contiguous_hyperslab_FillSpatialGridTopology(this, TopologyNode, ID)
    !-----------------------------------------------------------------
    !< Fill the Spatial grid topology metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this                  !< XDMF contiguous hyperslab handler for structured Grids
        type(Node), pointer,                                   intent(IN)    :: TopologyNode          !< Fox DOM Topology node
        integer(I4P),                                          intent(IN)    :: ID                    !< Grid IDentifier
        class(spatial_grid_descriptor_t), pointer                            :: SpatialGridDescriptor !< Spatial grid descriptor
        type(xdmf_topology_t)                                                :: Topology              !< XDMF Topology derived type
        type(xdmf_dataitem_t)                                                :: DataItem              !< XDMF DataItem derived type
        type(Node), pointer                                                  :: DataItemNode          !< Fox DOM Dataitem node
        integer(I8P), allocatable                                            :: auxDims(:)            !< Aux dimensions variable
        character(len=:), allocatable                                        :: XPath                 !< Topology XPath
    !----------------------------------------------------------------- 
        if(.not. associated(TopologyNode)) return
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor))
        call Topology%Parse(DOMNode = TopologyNode)
        ! Set TopologyType
        call SpatialGridDescriptor%SetTopologyTypePerGridID(&
                    TopologyType = GetXDMFTopologyTypeFromName(Topology%get_TopologyType()), ID=ID)
        ! Set NumberOfElements
        allocate(auxDims(size(Topology%get_Dimensions(), dim=1, kind=I4P)))
        auxDims = Topology%get_Dimensions()
        ! Dimensions are specified with the slowest varying dimension first (i.e. KJI order)
        auxDims(:) = auxDims(size(auxDims, dim=1, kind=I4P):1:-1)
        call SpatialGridDescriptor%SetXSizePerGridID(AuxDims(1),ID=ID)
        call SpatialGridDescriptor%SetYSizePerGridID(AuxDims(2),ID=ID)
        if(size(AuxDims, dim=1, kind=I4P) == 3) then
            call SpatialGridDescriptor%SetZSizePerGridID(AuxDims(3),ID=ID)
            call SpatialGridDescriptor%SetNumberOfNodesPerGridID(max(1,AuxDims(1))*max(1,AuxDims(2))*max(1,AuxDims(3)),ID=ID)
            call SpatialGridDescriptor%SetNumberOfElementsPerGridID(max(1,AuxDims(1)-1)*max(1,AuxDims(2)-1)*max(1,AuxDims(3)-1),ID=ID)
        else
            call SpatialGridDescriptor%SetNumberOfNodesPerGridID(max(1,AuxDims(1))*max(1,AuxDims(2)),ID=ID)
            call SpatialGridDescriptor%SetNumberOfElementsPerGridID(max(1,AuxDims(1)-1)*max(1,AuxDims(2)-1),ID=ID)
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
        type(mpi_env_t),                  pointer                 :: MPIEnvironment          !< MPI environment
        class(spatial_grid_descriptor_t), pointer                 :: SpatialGridDescriptor   !< Spatial grid descriptor
        type(xmlf_t),                     pointer                 :: XMLHandler              !< XDMF file handler
        type(xdmf_topology_t)                                     :: topology                !< XDMF Topology type
        integer(I8P)                                              :: GridShape(3)            !< Local number of elements
        character(len=:), allocatable                             :: XMDFTopologyTypeName    !< String topology type identifier
        integer(I4P)                                              :: SpaceDimension          !< Space dimensions
    !-----------------------------------------------------------------
    !< @Note: allow different Topology or Topology for each part of the spatial grid?
        MPIEnvironment => this%GetMPIEnvironment()
        assert(associated(MPIEnvironment))
        if(MPIEnvironment%is_root()) then
            SpatialGridDescriptor => this%GetSpatialGridDescriptor()
            XMLHandler            => this%GetSpatialFileXMLHandler()
            assert(associated(SpatialGridDescriptor) .and. associated(XMLHandler))
            ! Topology Grid shape is expressed in ZYX order in structured grids
            GridShape(1) = SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID, Dimension=3)
            GridShape(2) = SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID, Dimension=2)
            GridShape(3) = SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID, Dimension=1)
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(SpatialGridDescriptor%GetTopologyTypePerGridID(id=GridID))
            SpaceDimension = GetSpaceDimension(SpatialGridDescriptor%GetGeometryTypePerGridID(id=GridID))
            if (SpaceDimension == 2) then
                call topology%open( xml_handler = XMLHandler, &
                        Dimensions  = GridShape(2:3),         &
                        TopologyType=XMDFTopologyTypeName)
            else
                call topology%open( xml_handler = XMLHandler, &
                        Dimensions  = GridShape,              &
                        TopologyType=XMDFTopologyTypeName)
            endif
            call topology%close(xml_handler= XMLHandler)
        endif                    
    end subroutine xdmf_str_contiguous_hyperslab_WriteTopology


    subroutine xdmf_str_contiguous_hyperslab_WriteGeometry(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XDMF contiguous hyperslab handler for structured Grids
        integer(I4P),                                          intent(IN)    :: GridID !< Grid ID number
        class(uniform_grid_descriptor_t), pointer                            :: UniformGridDescriptor  !< Uniform grid descriptor
    !----------------------------------------------------------------- 
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        assert(associated(UniformGridDescriptor))
        select case(UniformGridDescriptor%GetGeometryType())
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
        type(mpi_env_t),                  pointer      :: MPIEnvironment          !< MPI environment
        class(uniform_grid_descriptor_t), pointer      :: UniformGridDescriptor   !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), pointer      :: SpatialGridDescriptor   !< Spatial grid descriptor
        type(xmlf_t),                     pointer      :: XMLHandler              !< XDMF file handler
        type(xdmf_geometry_t)                          :: geometry                !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem                !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata                !< XDMF Character Data type
        integer(I4P)                                   :: GridNumber              !< NumberOfGrids
        integer(I4P)                                   :: NumberOfGrids           !< NumberOfGrids
        character(len=:), allocatable                  :: HDF5FileName            !< Name of the HDF5 file
        character(len=:), allocatable                  :: XDMFGeometryTypeName    !< String geometry type identifier
        integer(I4P)                                   :: SpaceDimension          !< Space dimension
    !-----------------------------------------------------------------
        MPIEnvironment => this%GetMPIEnvironment()
        assert(associated(MPIEnvironment))
        if(MPIEnvironment%is_root()) then
            UniformGridDescriptor => this%GetUniformGridDescriptor()
            SpatialGridDescriptor => this%GetSpatialGridDescriptor()
            XMLHandler            => this%GetSpatialFileXMLHandler()
            assert(associated(UniformGridDescriptor) .and. associated(SpatialGridDescriptor) .and. associated(XMLHandler))
            if(SpatialGridDescriptor%IsStaticGrid()) then
                HDF5FileName = this%GetHDF5FileName(Step=XDMF_STATIC_STEP)
            else
                HDF5FileName = this%GetHDF5FileName()
            endif
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(SpatialGridDescriptor%GetGeometryTypePerGridID(ID=GridID))
            GridNumber = GridID
            NumberOfGrids = SpatialGridDescriptor%GetNumberOfGrids()
            SpaceDimension = GetSpaceDimension(SpatialGridDescriptor%GetGeometryTypePerGridID(id=GridID))
            call geometry%open( xml_handler  = XMLHandler, &
                    GeometryType = XDMFGeometryTypeName)
            ! Origin
            call dataitem%open( xml_handler = XMLHandler, &
                    Dimensions  = (/SpaceDimension/),     &   
                    ItemType    = 'HyperSlab',            &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = XMLHandler, &
                    Dimensions = (/3_I4P, 1_I4P/),       &
                    NumberType = 'Int',                  &
                    Format     = 'XML',                  &
                    Precision  = 4_I4P) 
            call chardata%write( xml_handler = XMLHandler, &
                    Data = (/int(SpaceDimension,I8P)*int(GridNumber,I8P),1_I8P, int(SpaceDimension,I8P)/) )
            call dataitem%close(xml_handler = XMLHandler)
            call dataitem%open(xml_handler = XMLHandler,                             &
                    Dimensions = (/int(SpaceDimension,I8P)*int(NumberOfGrids,I8P)/), &
                    NumberType = 'Float',                                            &
                    Format     = 'HDF',                                              &
                    Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = XMLHandler, &
                    Data = HDF5FileName//':'//'Origin_'//UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = XMLHandler)
            call dataitem%close(xml_handler = XMLHandler)
            ! DXDYXDZ
            call dataitem%open( xml_handler = XMLHandler, &
                    Dimensions  = (/SpaceDimension/),     &   
                    ItemType    = 'HyperSlab',            &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = XMLHandler, &
                    Dimensions = (/3_I4P, 1_I4P/),       &
                    NumberType = 'Int',                  &
                    Format     = 'XML',                  &
                    Precision  = 4_I4P) 
            call chardata%write( xml_handler = XMLHandler, &
                    Data = (/int(SpaceDimension,I8P)*int(GridNumber,I8P),1_I8P, int(SpaceDimension,I8P)/) )
            call dataitem%close(xml_handler = XMLHandler)
            call dataitem%open(xml_handler = XMLHandler,                             &
                    Dimensions = (/int(SpaceDimension,I8P)*int(NumberOfGrids,I8P)/), &
                    NumberType = 'Float',                                            &
                    Format     = 'HDF',                                              &
                    Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = XMLHandler, &
                    Data = HDF5FileName//':'//'DxDyDz_'//UniformGridDescriptor%GetGeometryName())
            call dataitem%close(xml_handler = XMLHandler)
            call dataitem%close(xml_handler = XMLHandler)
            call geometry%close(xml_handler = XMLHandler)
        endif                    
    end subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_DXDYDZ


    subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_VXVYVZ(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_structured_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler for structured Grids
        integer(I4P),                    intent(IN)    :: GridID                  !< Grid ID number
        type(mpi_env_t),                  pointer      :: MPIEnvironment          !< MPI environment
        class(uniform_grid_descriptor_t), pointer      :: UniformGridDescriptor   !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), pointer      :: SpatialGridDescriptor   !< Spatial grid descriptor
        type(xmlf_t),                     pointer      :: XMLHandler              !< XDMF file handler
        type(xdmf_geometry_t)                          :: geometry                !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem                !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata                !< XDMF Character Data type
        integer(I8P)                                   :: LocalGridShape(3)       !< Uniform Grid shape
        integer(I8P)                                   :: GlobalGridShape(3)      !< Spatial grid shpae
        integer(I8P)                                   :: GridShapeOffset(3)      !< Grid shape offset
        character(len=:), allocatable                  :: HDF5FileName            !< Name of the HDF5 file
        character(len=:), allocatable                  :: XDMFGeometryTypeName    !< String geometry type identifier
        Integer(I4P)                                   :: SpaceDimension          !< Space Dimension
    !-----------------------------------------------------------------
        MPIEnvironment => this%GetMPIEnvironment()
        assert(associated(MPIEnvironment))
        if(MPIEnvironment%is_root()) then
            UniformGridDescriptor => this%GetUniformGridDescriptor()
            SpatialGridDescriptor => this%GetSpatialGridDescriptor()
            XMLHandler            => this%GetSpatialFileXMLHandler()
            assert(associated(UniformGridDescriptor) .and. associated(SpatialGridDescriptor) .and. associated(XMLHandler))
            if(SpatialGridDescriptor%IsStaticGrid()) then
                HDF5FileName = this%GetHDF5FileName(Step=XDMF_STATIC_STEP)
            else
                HDF5FileName = this%GetHDF5FileName()
            endif
            LocalGridShape(1)  = SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=1)
            LocalGridShape(2)  = SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=2)
            GlobalGridShape(1) = SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=1)
            GlobalGridShape(2) = SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=2)
            GridShapeOffset(1) = SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID, Dimension=1)
            GridShapeOffset(2) = SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID, Dimension=2)
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(SpatialGridDescriptor%GetGeometryTypePerGridID(ID=GridID))
            SpaceDimension = GetSpaceDimension(SpatialGridDescriptor%GetGeometryTypePerGridID(id=GridID))
            if (SpaceDimension == 3) then
                LocalGridShape(3) = SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=3)
                GlobalGridShape(3) = SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=3)
                GridShapeOffset(3) = SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID, Dimension=3)
                call geometry%open( xml_handler  = XMLHandler, &
                        GeometryType = XDMFGeometryTypeName)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = XMLHandler, &
                        Dimensions  = (/LocalGridShape(1)/),  &   
                        ItemType    = 'HyperSlab',            &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/3_I4P, 1_I4P/),       &
                        NumberType = 'Int',                  &
                        Format     = 'XML',                  &
                        Precision  = 4_I4P) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = (/GridShapeOffset(1),1_I8P,LocalGridShape(1)/) )
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/GlobalGridShape(1)/), &
                        NumberType = 'Float',                &
                        Format     = 'HDF',                  &
                        Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = HDF5FileName//':X_'//UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%close(xml_handler = XMLHandler)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = XMLHandler, &
                        Dimensions  = (/LocalGridShape(2)/),  &   
                        ItemType    = 'HyperSlab',            &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/3_I4P, 1_I4P/),       &
                        NumberType = 'Int',                  &
                        Format     = 'XML',                  &
                        Precision  = 4_I4P) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = (/GridShapeOffset(2),1_I8P,LocalGridShape(2)/) )
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/GlobalGridShape(2)/), &
                        NumberType = 'Float',                &
                        Format     = 'HDF',                  &
                        Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = HDF5Filename//':Y_'//UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%close(xml_handler = XMLHandler)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = XMLHandler, &
                        Dimensions  = (/LocalGridShape(3)/),  &   
                        ItemType    = 'HyperSlab',            &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/3_I4P, 1_I4P/),       &
                        NumberType = 'Int',                  &
                        Format     = 'XML',                  &
                        Precision  = 4_I4P) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = (/GridShapeOffset(3),1_I8P,LocalGridShape(3)/) )
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/GlobalGridShape(3)/), &
                        NumberType = 'Float',                &
                        Format     = 'HDF',                  &
                        Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = HDF5FileName//':Z_'//UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%close(xml_handler = XMLHandler)
            elseif (SpaceDimension == 2) then
            ! Why paraview need to put in inverse order?? YX
            ! 2D VXVY does not apper in the standard Model&Format
                call geometry%open( xml_handler  = XMLHandler, &
                        GeometryType = XDMFGeometryTypeName)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = XMLHandler, &
                        Dimensions  = (/LocalGridShape(2)/),  &   
                        ItemType    = 'HyperSlab',            &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/3_I4P, 1_I4P/),       &
                        NumberType = 'Int',                  &
                        Format     = 'XML',                  &
                        Precision  = 4_I4P) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = (/GridShapeOffset(2),1_I8P,LocalGridShape(2)/) )
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/GlobalGridShape(2)/), &
                        NumberType = 'Float',                &
                        Format     = 'HDF',                  &
                        Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = HDF5FileName//':Y_'//UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%close(xml_handler = XMLHandler)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
                call dataitem%open( xml_handler = XMLHandler, &
                        Dimensions  = (/LocalGridShape(1)/),  &   
                        ItemType    = 'HyperSlab',            &
                        Format      = 'HDF')
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/3_I4P, 1_I4P/),       &
                        NumberType = 'Int',                  &
                        Format     = 'XML',                  &
                        Precision  = 4_I4P) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = (/GridShapeOffset(1),1_I8P,LocalGridShape(1)/) )
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%open(xml_handler = XMLHandler, &
                        Dimensions = (/GlobalGridShape(1)/), &
                        NumberType = 'Float',                &
                        Format     = 'HDF',                  &
                        Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = HDF5FileName//':X_'//UniformGridDescriptor%GetGeometryName())
                call dataitem%close(xml_handler = XMLHandler)
                call dataitem%close(xml_handler = XMLHandler)

            endif
            call geometry%close(xml_handler = XMLHandler)
        endif                    
    end subroutine xdmf_str_contiguous_hyperslab_WriteGeometry_VXVYVZ


end module xdmf_structured_contiguous_hyperslab_handler
