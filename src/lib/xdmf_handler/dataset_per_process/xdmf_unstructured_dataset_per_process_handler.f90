module xdmf_unstructured_dataset_per_process_handler
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
use xdmf_dataset_per_process_handler
use fox_dom, only: Node
use PENF,    only: I4P, I8P, R4P, R8P, str

implicit none

#include "assert.i90"

private

    type, extends(xdmf_dataset_per_process_handler_t) :: xdmf_unstructured_dataset_per_process_handler_t
    !-----------------------------------------------------------------
    !< XDMF dataset per process handler for Unstructured Grids
    !----------------------------------------------------------------- 
    contains
    private
        procedure         :: SetTopology_I4P              => xdmf_unst_dataset_per_process_SetTopology_I4P
        procedure         :: SetTopology_I8P              => xdmf_unst_dataset_per_process_SetTopology_I8P
        procedure         :: FillSpatialGridTopology      => xdmf_unst_dataset_per_process_FillSpatialGridTopology
        procedure         :: WriteGeometry                => xdmf_unst_dataset_per_process_WriteGeometry
        procedure         :: WriteGeometry_XYZ            => xdmf_unst_dataset_per_process_WriteGeometry_XYZ
        procedure         :: WriteGeometry_X_Y_Z          => xdmf_unst_dataset_per_process_WriteGeometry_X_Y_Z
        procedure         :: WriteTopology                => xdmf_unst_dataset_per_process_WriteTopology
    end type xdmf_unstructured_dataset_per_process_handler_t

public :: xdmf_unstructured_dataset_per_process_handler_t

contains

    subroutine xdmf_unst_dataset_per_process_SetTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I4P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this  !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                           intent(IN)    :: Name              !< Topology name
        type(mpi_env_t),                  pointer                 :: MPIEnvironment         !< MPI Environment
        class(uniform_grid_descriptor_t), pointer                 :: UniformGridDescriptor  !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), pointer                 :: SpatialGridDescriptor  !< Spatial grid descriptor
    !-----------------------------------------------------------------
        MPIEnvironment        => this%GetMPIEnvironment()
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(MPIEnvironment) .and. associated(UniformGridDescriptor) .and. associated(SpatialGridDescriptor))
        call SpatialGridDescriptor%SetTopologySizePerGridID(                               &
                                            TopologySize = size(connectivities,dim=1, kind=I8P), &
                                            ID           = MPIEnvironment%get_rank())
        call UniformGridDescriptor%SetTopologyMetadata(              &
                                            Name            = Name,  &
                                            Precision       = 4_I4P, &
                                            ArrayDimensions = (/size(Connectivities, dim=1, kind=I8P)/))
    end subroutine xdmf_unst_dataset_per_process_SetTopology_I4P


    subroutine xdmf_unst_dataset_per_process_SetTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Add I8P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this  !< XDMF dataset per process handler for Unstructured Grids
        integer(I8P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
        character(len=*),                           intent(IN)    :: Name              !< Topology name
        type(mpi_env_t),                  pointer                 :: MPIEnvironment         !< MPI Environment
        class(uniform_grid_descriptor_t), pointer                 :: UniformGridDescriptor  !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), pointer                 :: SpatialGridDescriptor  !< Spatial grid descriptor
    !-----------------------------------------------------------------
        MPIEnvironment        => this%GetMPIEnvironment()
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(MPIEnvironment) .and. associated(UniformGridDescriptor) .and. associated(SpatialGridDescriptor))
        call SpatialGridDescriptor%SetTopologySizePerGridID(                               &
                                            TopologySize = size(connectivities,dim=1, kind=I8P), &
                                            ID           = MPIEnvironment%get_rank())
        call UniformGridDescriptor%SetTopologyMetadata(              &
                                            Name            = Name,  &
                                            Precision       = 8_I4P, &
                                            ArrayDimensions = (/size(Connectivities, dim=1, kind=I8P)/))
    end subroutine xdmf_unst_dataset_per_process_SetTopology_I8P


    subroutine xdmf_unst_dataset_per_process_FillSpatialGridTopology(this, TopologyNode, ID)
    !-----------------------------------------------------------------
    !< Fill the Spatial grid topology metainfo from a Topology
    !< FoX DOM Node
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this  !< XDMF dataset per process handler for Unstructured Grids
        type(Node), pointer,                        intent(IN)    :: TopologyNode      !< Fox DOM Topology node
        integer(I4P),                               intent(IN)    :: ID                !< Grid IDentifier
        class(spatial_grid_descriptor_t), pointer                 :: SpatialGridDescriptor  !< Spatial grid descriptor
        type(xdmf_topology_t)                                     :: Topology          !< XDMF Topology derived type
        type(xdmf_dataitem_t)                                     :: DataItem          !< XDMF DataItem derived type
        type(Node), pointer                                       :: DataItemNode      !< Fox DOM Dataitem node
        integer(I8P)                                              :: auxDims(1)        !< Aux dimensions variable
        character(len=:), allocatable                             :: XPath             !< Topology XPath
    !----------------------------------------------------------------- 
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor))
        if(.not. associated(TopologyNode)) return
        call Topology%Parse(DOMNode = TopologyNode)
        ! Set TopologyType
        call SpatialGridDescriptor%SetTopologyTypePerGridID(&
                    TopologyType = GetXDMFTopologyTypeFromName(Topology%get_TopologyType()), ID=ID)
        ! Set NumberOfElements
        auxDims = Topology%get_Dimensions()
        call SpatialGridDescriptor%SetNumberOfElementsPerGridID(AuxDims(1),ID=ID)
        ! Set ConnectivitySize
        DataItemNode => this%GetFirstChildByTag(FatherNode = TopologyNode, Tag = 'DataItem')
        call DataItem%Parse(DomNode = DataItemNode)
        auxDims = DataItem%get_Dimensions()
        call SpatialGridDescriptor%SetTopologySizePerGridID(AuxDims(1),ID=ID)
        ! Free
        call Topology%Free()
        call DataItem%Free()
        nullify(DataItemNode)
    end subroutine xdmf_unst_dataset_per_process_FillSpatialGridTopology


    subroutine xdmf_unst_dataset_per_process_WriteTopology(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Topology into a opened file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this        !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                               intent(IN)    :: GridID                  !< Grid ID number
        type(mpi_env_t),                  pointer                 :: MPIEnvironment          !< MPI Environment
        class(uniform_grid_descriptor_t), pointer                 :: UniformGridDescriptor   !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), pointer                 :: SpatialGridDescriptor   !< Spatial grid descriptor
        type(xmlf_t),                     pointer                 :: XMLHandler              !< XDMF file handler
        type(xdmf_topology_t)                                     :: topology                !< XDMF Topology type
        type(xdmf_dataitem_t)                                     :: dataitem                !< XDMF Dataitem type
        type(xdmf_character_data_t)                               :: chardata                !< XDMF Character Data type
        integer(I8P)                                              :: LocalNumberOfElements   !< Local number of elements
        integer(I8P)                                              :: GlobalConnectivitySize  !< Global connectivity size
        integer(I8P)                                              :: Start                   !< Hyperslab start
        integer(I8P)                                              :: TopologySize            !< Topology size
        character(len=:), allocatable                             :: HDF5FileName            !< Name of the HDF5 file
        character(len=:), allocatable                             :: XMDFTopologyTypeName    !< String topology type identifier
        integer(I4P)                                              :: DimensionsSize          !< Size of the topology shape
    !-----------------------------------------------------------------
    !< @Note: allow different Topology or Topology for each part of the spatial grid?
        MPIEnvironment        => this%GetMPIEnvironment()
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        XMLHandler            => this%GetSpatialFileXMLHandler()
        assert(associated(MPIEnvironment) .and. associated(UniformGridDescriptor) .and. associated(SpatialGridDescriptor) .and. associated(XMLHandler))
        if(MPIEnvironment%is_root()) then
            if(SpatialGridDescriptor%IsStaticGrid()) then
                HDF5FileName = this%GetHDF5FileName(Step=XDMF_STATIC_STEP)
            else
                HDF5FileName = this%GetHDF5FileName()
            endif
            LocalNumberOfElements = SpatialGridDescriptor%GetNumberOfElementsPerGridID(ID=GridID)
            Start = SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=GridID)
            GlobalConnectivitySize = SpatialGridDescriptor%GetGlobalTopologySize()
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(UniformGridDescriptor%getTopologyType())
            TopologySize = SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID)
            DimensionsSize = size(UniformGridDescriptor%GetTopologyArrayDimensions(), dim=1, kind=I8P)
            call topology%open( xml_handler = XMLHandler,    &
                    Dimensions  = (/LocalNumberOfelements/), &
                    TopologyType=XMDFTopologyTypeName)
            call dataitem%open( xml_handler = XMLHandler, &
                    Dimensions  = (/TopologySize/),       &
                    NumberType  = 'Int',                  &
                    Format      = 'HDF',                  & 
                    Precision   = UniformGridDescriptor%GetTopologyPrecision()) 
            call chardata%write( xml_handler = XMLHandler,                             &
                    Data = HDF5Filename//':'//UniformGridDescriptor%GetTopologyName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler= XMLHandler)
            call topology%close(xml_handler= XMLHandler)
        endif                    
    end subroutine xdmf_unst_dataset_per_process_WriteTopology


    subroutine xdmf_unst_dataset_per_process_WriteGeometry(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this   !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                                           intent(IN)    :: GridID !< Grid ID number
        class(uniform_grid_descriptor_t), pointer                             :: UniformGridDescriptor        !< Uniform grid descriptor
    !----------------------------------------------------------------- 
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        assert(associated(UniformGridDescriptor))
        select case(UniformGridDescriptor%GetGeometryType())
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
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this  !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                    intent(IN)    :: GridID                       !< Grid ID number
        type(mpi_env_t),                  pointer      :: MPIEnvironment               !< MPI Environment
        class(uniform_grid_descriptor_t), pointer      :: UniformGridDescriptor        !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), pointer      :: SpatialGridDescriptor        !< Spatial grid descriptor
        type(xmlf_t),                     pointer      :: XMLHandler                   !< XDMF file handler
        type(xdmf_geometry_t)                          :: geometry                     !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem                     !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata                     !< XDMF Character Data type
        integer(I8P)                                   :: LocalGeometrySize            !< Local geometry size
        integer(I8P)                                   :: GlobalGeometrySize           !< Global geometrySize
        integer(I4P)                                   :: SpaceDimension               !< Space dimension
        integer(I8P)                                   :: Start                        !< Hyperslab start
        integer(I8P)                                   :: Count                        !< Hyperslab count
        character(len=:), allocatable                  :: HDF5FileName                 !< Name of the HDF5 file
        character(len=:), allocatable                  :: XDMFGeometryTypeName         !< String geometry type identifier
        integer(I4P)                                   :: DimensionsSize               !< Size of the geometry shape
    !-----------------------------------------------------------------
        MPIEnvironment        => this%GetMPIEnvironment()
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        XMLHandler            => this%GetSpatialFileXMLHandler()
        assert(associated(MPIEnvironment) .and. associated(UniformGridDescriptor) .and. associated(SpatialGridDescriptor) .and. associated(XMLHandler))
        if(MPIEnvironment%is_root()) then
            if(SpatialGridDescriptor%IsStaticGrid()) then
                HDF5FileName = this%GetHDF5FileName(Step=XDMF_STATIC_STEP)
            else
                HDF5FileName = this%GetHDF5FileName()
            endif
            LocalGeometrySize = SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID)
            Start = SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=GridID)
            GlobalGeometrySize = SpatialGridDescriptor%GetGlobalGeometrySize()
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(UniformGridDescriptor%GetGeometryType())
            Count = LocalGeometrySize
            DimensionsSize = size(UniformGridDescriptor%GetGeometryArrayDimensions(), dim=1, kind=I8P)
            call geometry%open( xml_handler  = XMLHandler, &
                    GeometryType = XDMFGeometryTypeName)
            call dataitem%open(xml_handler = XMLHandler, &
                    Dimensions = (/LocalGeometrySize/),  &
                    NumberType = 'Float',                &
                    Format     = 'HDF',                  &
                    Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = XMLHandler,                             &
                    Data = HDF5Filename//':'//UniformGridDescriptor%GetGeometryName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler = XMLHandler)
            call geometry%close(xml_handler = XMLHandler)
        endif                    
    end subroutine xdmf_unst_dataset_per_process_WriteGeometry_XYZ


    subroutine xdmf_unst_dataset_per_process_WriteGeometry_X_Y_Z(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(xdmf_unstructured_dataset_per_process_handler_t), intent(INOUT) :: this  !< XDMF dataset per process handler for Unstructured Grids
        integer(I4P),                    intent(IN)    :: GridID                       !< Grid ID number
        type(mpi_env_t),                  pointer      :: MPIEnvironment               !< MPI Environment
        class(uniform_grid_descriptor_t), pointer      :: UniformGridDescriptor        !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), pointer      :: SpatialGridDescriptor        !< Spatial grid descriptor
        type(xmlf_t),                     pointer      :: XMLHandler                   !< XDMF file handler
        type(xdmf_geometry_t)                          :: geometry                     !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem                     !< XDMF Dataitem ttype
        type(xdmf_character_data_t)                    :: chardata                     !< XDMF Character Data type
        integer(I8P)                                   :: LocalNumberOfNodes           !< Number of nodes of the uniform grid
        integer(I8P)                                   :: GlobalNumberOfNodes          !< Number of nodes of the spatial grid
        integer(I4P)                                   :: SpaceDimension               !< Space dimension
        integer(I8P)                                   :: Start                        !< Hyperslab start
        integer(I8P)                                   :: Count                        !< Hyperslab count
        character(len=:), allocatable                  :: HDF5FileName                 !< Name of the HDF5 file
        character(len=:), allocatable                  :: XDMFGeometryTypeName         !< String geometry type identifier
        integer(I4P)                                   :: DimensionsSize               !< Size of the geometry shape
    !-----------------------------------------------------------------
        MPIEnvironment        => this%GetMPIEnvironment()
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        XMLHandler            => this%GetSpatialFileXMLHandler()
        assert(associated(MPIEnvironment) .and. associated(UniformGridDescriptor) .and. associated(SpatialGridDescriptor) .and. associated(XMLHandler))
        if(MPIEnvironment%is_root()) then
            if(SpatialGridDescriptor%IsStaticGrid()) then
                HDF5FileName = this%GetHDF5FileName(Step=XDMF_STATIC_STEP)
            else
                HDF5FileName = this%GetHDF5FileName()
            endif
            LocalNumberOfNodes = SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=GridID)
            Start = SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=GridID)
            GlobalNumberOfNodes = SpatialGridDescriptor%GetGlobalNumberOfNodes()
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(UniformGridDescriptor%GetGeometryType())
            Count = LocalNumberOfNodes
            call geometry%open( xml_handler  = XMLHandler, &
                    GeometryType = XDMFGeometryTypeName)
            DimensionsSize = size(UniformGridDescriptor%GetGeometryArrayDimensions(), dim=1, kind=I8P)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
            call dataitem%open(xml_handler = XMLHandler, &
                    Dimensions = (/LocalNumberOfNodes/), &
                    NumberType = 'Float',                &
                    Format     = 'HDF',                  &
                    Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = XMLHandler,                               &
                    Data = HDF5Filename//':X_'//UniformGridDescriptor%GetGeometryName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler = XMLHandler)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
            call dataitem%open(xml_handler = XMLHandler, &
                    Dimensions = (/LocalNumberOfNodes/), &
                    NumberType = 'Float',                &
                    Format     = 'HDF',                  &
                    Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = XMLHandler,                               &
                    Data = HDF5Filename//':Y_'//UniformGridDescriptor%GetGeometryName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler = XMLHandler)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
            call dataitem%open(xml_handler = XMLHandler, &
                    Dimensions = (/LocalNumberOfNodes/), &
                    NumberType = 'Float',                &
                    Format     = 'HDF',                  &
                    Precision  = UniformGridDescriptor%GetGeometryPrecision()) 
            call chardata%write( xml_handler = XMLHandler,                               &
                    Data = HDF5Filename//':Z_'//UniformGridDescriptor%GetGeometryName()//&
                           '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
            call dataitem%close(xml_handler = XMLHandler)

            call geometry%close(xml_handler = XMLHandler)
        endif                    
    end subroutine xdmf_unst_dataset_per_process_WriteGeometry_X_Y_Z


end module xdmf_unstructured_dataset_per_process_handler
