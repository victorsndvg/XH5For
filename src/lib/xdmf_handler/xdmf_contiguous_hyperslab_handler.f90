module xdmf_contiguous_hyperslab_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF Time handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use IR_Precision, only : I4P, I8P, R4P, R8P
use xh5for_utils
use fox_xdmf
use xdmf_handler

implicit none

private

    type :: xdmf_geometry_info_t
        character(len=:), allocatable :: XPath
        integer(I4P)                  :: CoordinatesPrecision
        integer(I4P)                  :: CoordinatesDimension
    end type xdmf_geometry_info_t

    type :: xdmf_topology_info_t
        character(len=:), allocatable :: XPath
        integer(I4P)                  :: ConnectivitiesPrecision
        integer(I4P)                  :: ConnectivitiesDimension
    end type xdmf_topology_info_t

    type :: xdmf_attribute_info_t
        character(len=:), allocatable :: XPath
    end type xdmf_attribute_info_t

    type, extends(xdmf_handler_t) :: xdmf_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
        type(xdmf_geometry_info_t)               :: geometry_info                   !< XDMF contiguous hyperslab geometry info 
        type(xdmf_topology_info_t)               :: topology_info                   !< XDMF contiguous hyperslab topology info 
        type(xdmf_geometry_info_t),  allocatable :: attributes_info(:)              !< XDMF contiguous hyperslab attributes info 
    contains
    private
        procedure         :: SetGeometryInfo => xdmf_contiguous_hyperslab_handler_SetGeometryInfo
        procedure         :: SetTopologyInfo => xdmf_contiguous_hyperslab_handler_SetTopologyInfo
        procedure         :: AddGeometry_R4P => xdmf_contiguous_hyperslab_handler_AddGeometry_R4P
        procedure         :: AddGeometry_R8P => xdmf_contiguous_hyperslab_handler_AddGeometry_R8P
        procedure         :: AddTopology_I4P => xdmf_contiguous_hyperslab_handler_AddTopology_I4P
        procedure         :: AddTopology_I8P => xdmf_contiguous_hyperslab_handler_AddTopology_I8P
        procedure, public :: OpenFile        => xdmf_contiguous_hyperslab_handler_OpenFile
        procedure, public :: CloseFile       => xdmf_contiguous_hyperslab_handler_CloseFile
        generic,   public :: AddGeometry     => AddGeometry_R4P, &
                                                AddGeometry_R8P
        generic,   public :: AddTopology     => AddTopology_I4P, &
                                                AddTopology_I8P
        procedure, public :: DeferredWrite => xdmf_contiguous_hyperslab_handler_DeferredWrite
        procedure, public :: WriteGeometry   => xdmf_contiguous_hyperslab_handler_WriteGeometry
        procedure, public :: WriteTopology   => xdmf_contiguous_hyperslab_handler_WriteTopology
        procedure, public :: WriteAttribute  => xdmf_contiguous_hyperslab_handler_WriteAttribute
    end type xdmf_contiguous_hyperslab_handler_t

public :: xdmf_contiguous_hyperslab_handler_t

contains

    subroutine xdmf_contiguous_hyperslab_handler_SetGeometryInfo(this, XPath, CoordinatesPrecision, CoordinatesDimension)
    !-----------------------------------------------------------------
    !< XDMF geometry info
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this                  !< XMDF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: XPath                 !< XDMF XPath to the HDF5 connetivities
        integer(I4P),                               intent(IN)    :: CoordinatesPrecision  !< Precision of the Coordinates in the HDF5 file
        integer(I4P),                               intent(IN)    :: CoordinatesDimension  !< Dimensions of the Coordinates array in the HDF5 file
    !-----------------------------------------------------------------
        this%geometry_info%XPath                = XPath
        this%geometry_info%CoordinatesPrecision = CoordinatesPrecision
        this%geometry_info%CoordinatesDimension = CoordinatesDimension
    end subroutine xdmf_contiguous_hyperslab_handler_SetGeometryInfo


    subroutine xdmf_contiguous_hyperslab_handler_SetTopologyInfo(this, XPath, ConnectivitiesPrecision, ConnectivitiesDimension)
    !-----------------------------------------------------------------
    !< XDMF geometry info
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this                    !< XMDF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: XPath                   !< XDMF XPath to the HDF5 coordinates
        integer(I4P),                               intent(IN)    :: ConnectivitiesPrecision !< Precision of the coordinates in the HDF5 file
        integer(I4P),                               intent(IN)    :: ConnectivitiesDimension !< Dimensions of the coordinates array in the HDF5 file
    !-----------------------------------------------------------------
        this%topology_info%XPath                    = XPath
        this%topology_info%ConnectivitiesPrecision  = ConnectivitiesPrecision
        this%topology_info%ConnectivitiesDimension  = ConnectivitiesDimension
    end subroutine xdmf_contiguous_hyperslab_handler_SetTopologyInfo


    subroutine xdmf_contiguous_hyperslab_handler_AddGeometry_R4P(this, Coordinates)
    !-----------------------------------------------------------------
    !< Add R4P geometry info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XDMF contiguous hyperslab handler
        real(R4P),                                  intent(IN)    :: Coordinates(:) !< Grid coordinates
    !-----------------------------------------------------------------
        call this%SetGeometryInfo(XPath='Coordinates', CoordinatesPrecision=4, CoordinatesDimension=1)
    end subroutine xdmf_contiguous_hyperslab_handler_AddGeometry_R4P


    subroutine xdmf_contiguous_hyperslab_handler_AddGeometry_R8P(this, Coordinates)
    !-----------------------------------------------------------------
    !< Add R8P geometry info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XDMF contiguous hyperslab handler
        real(R8P),                                  intent(IN)    :: Coordinates(:) !< Grid coordinates
    !-----------------------------------------------------------------
        call this%SetGeometryInfo(XPath='Coordinates', CoordinatesPrecision=8, CoordinatesDimension=1)
    end subroutine xdmf_contiguous_hyperslab_handler_AddGeometry_R8P


    subroutine xdmf_contiguous_hyperslab_handler_AddTopology_I4P(this, Connectivities)
    !-----------------------------------------------------------------
    !< Add I4P topology info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler
        integer(I4P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
    !-----------------------------------------------------------------
        call this%SetTopologyInfo(XPath='Connectivities', ConnectivitiesPrecision=4, ConnectivitiesDimension=1)
    end subroutine xdmf_contiguous_hyperslab_handler_AddTopology_I4P


    subroutine xdmf_contiguous_hyperslab_handler_AddTopology_I8P(this, Connectivities)
    !-----------------------------------------------------------------
    !< Add I8P topology info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler
        integer(I8P),                               intent(IN)    :: Connectivities(:)    !< Grid Connectivities
    !-----------------------------------------------------------------
        call this%SetTopologyInfo(XPath='Connectivities', ConnectivitiesPrecision=8, ConnectivitiesDimension=1)
    end subroutine xdmf_contiguous_hyperslab_handler_AddTopology_I8P


    subroutine xdmf_contiguous_hyperslab_handler_OpenFile(this, fileprefix)
    !-----------------------------------------------------------------
    !< Open a XDMF file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XDMF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: fileprefix !< XDMF filename prefix
        type(xdmf_grid_t)                                         :: grid       !< XDMF Grid type
        type(xdmf_domain_t)                                       :: domain     !< XDMF Domain type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            this%prefix = trim(adjustl(fileprefix))
            call this%file%set_filename(trim(adjustl(fileprefix))//this%ext)
            call this%file%openfile()
            call domain%open(xml_handler = this%file%xml_handler)
            call grid%open(xml_handler = this%file%xml_handler, &
                    GridType='Collection', &
                    CollectionType='Spatial')
        endif
    end subroutine xdmf_contiguous_hyperslab_handler_OpenFile


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
            call grid%close(xml_handler=this%file%xml_handler)
            call domain%close(xml_handler = this%file%xml_handler)
            call this%file%closefile()
        endif
    end subroutine xdmf_contiguous_hyperslab_handler_CloseFile


    subroutine xdmf_contiguous_hyperslab_handler_DeferredWrite(this)
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this  !< XDMF contiguous hyperslab handler
        integer(I4P)                                              :: IDidx !< GridID idex

        do IDidx=0, this%MPIEnvironment%get_comm_size()-1
            call this%OpenGrid(GridID = IDidx)
            call this%WriteTopology(GridID = IDidx)
            call this%WriteGeometry(GridID = IDidx)
            call this%CloseGrid(GridID = IDidx)
        enddo
    end subroutine xdmf_contiguous_hyperslab_handler_DeferredWrite

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
        integer(I8P)                                              :: GlobalNumberOfElements  !< Global number of elements
        integer(I8P)                                              :: NodesPerElement         !< Number of nodes per element
        integer(I8P)                                              :: Start                   !< Hyperslab start
        integer(I8P)                                              :: Count                   !< Hyperslab count
        character(len=:), allocatable                             :: XMDFTopologyTypeName    !< String topology type identifier
    !-----------------------------------------------------------------
    !< @Note: allow different Topology or Topology for each part of the spatial grid?
        if(this%MPIEnvironment%is_root()) then
            if(present(GridID)) then
                LocalNumberOfElements = this%SpatialGridDescriptor%GetNumberOfElementsFromGridID(ID=GridID)
                NodesPerElement = GetNumberOfNodesPerElement(this%SpatialGridDescriptor%GetTopologyTypeFromGridID(ID=GridID))
                Start = this%SpatialGridDescriptor%GetElementOffsetFromGridID(ID=GridID)*NodesPerElement
            else
                LocalNumberOfElements = this%UniformGridDescriptor%GetNumberOfElements()
                NodesPerElement = GetNumberOfNodesPerElement(this%UniformGridDescriptor%GetTopologyType())
                Start = 0
            endif
            GlobalNumberOfElements = this%SpatialGridDescriptor%GetGlobalNumberOfElements()
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(this%UniformGridDescriptor%getTopologyType())
            Count = LocalNumberOfElements*NodesPerElement

            call topology%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/LocalNumberOfelements/),         &
                    TopologyType=XMDFTopologyTypeName)
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/Count/),&
                    ItemType    = 'HyperSlab',&
                    Format      = 'HDF')
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions     = (/3_I4P,this%topology_info%ConnectivitiesDimension/),&
                    NumberType     = 'Int',&
                    Format         = 'XML',&
                    Precision      = 4 ) 
            call chardata%open( xml_handler = this%file%xml_handler, &
                    Data = (/Start,1_I8P,Count/) )
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open( xml_handler = this%file%xml_handler,         &
                    Dimensions  = (/GlobalNumberOfElements*NodesPerElement/),&
                    NumberType  = 'Int',&
                    Format      = 'HDF',& 
                    Precision   = this%topology_info%ConnectivitiesPrecision) 
            call chardata%open( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//this%topology_info%XPath )
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
        integer(I8P)                                   :: GlobalNumberOfNodes   !< Local number of elements
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
                    Dimensions = (/3_I4P,this%geometry_info%CoordinatesDimension/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call chardata%open( xml_handler = this%file%xml_handler, &
                    Data = (/Start,1_I8P,Count/) )
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/GlobalNumberOfNodes*SpaceDimension/), &
                    NumberType = 'Float', &
                    Format     = 'HDF', &
                    Precision  = this%geometry_info%CoordinatesPrecision) 
            call chardata%open( xml_handler = this%file%xml_handler, &
                    Data = trim(adjustl(this%prefix))//'.h5'//':'//this%geometry_info%XPath)
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%close(xml_handler = this%file%xml_handler)
            call geometry%close(xml_handler = this%file%xml_handler)
        endif                    
    end subroutine xdmf_contiguous_hyperslab_handler_WriteGeometry


    subroutine xdmf_contiguous_hyperslab_handler_WriteAttribute(this, Name, Center, Type, GridID)
    !-----------------------------------------------------------------
    !< Writes a XDMF Attribute into a opened file for the contiguous HyperSlab strategy
    !< @NOTE: only nodal attributes
    !< @TODO: add cell, face and grid centered attributes
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this                   !< XDMF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: Name                   !< Attribute name
        integer(I4P),                               intent(IN)    :: Center                 !< XDMF Attribute center
        integer(I4P),                               intent(IN)    :: Type                   !< XDMF Attribute type
        integer(I4P), optional,                     intent(IN)    :: GridID                 !< Grid ID number
        type(xdmf_attribute_t)                                    :: attribute              !< XDMF Attribute type
        type(xdmf_dataitem_t)                                     :: dataitem               !< XDMF Dataitem type
        integer(I8P)                                              :: LocalNumberOfElements  !< Local number of elements
        integer(I8P)                                              :: LocalNumberOfNodes     !< Local number of nodes
        integer(I4P)                                              :: NodesPerElement        !< Number of nodes per element
        character(len=:), allocatable                             :: XDMFAttributeTypeName  !< String Attibute type identifier
        character(len=:), allocatable                             :: XDMFCenterTypeName     !< String Attribute Center identifier
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            if(present(GridID)) then
                localNumberOfElements = this%SpatialGridDescriptor%GetNumberOfElementsFromGridID(ID=GridID)
                localNumberOfNodes = this%SpatialGridDescriptor%GetNumberOfNodesFromGridID(ID=GridID)
                NodesPerElement = GetNumberOfNodesPerElement(this%SpatialGridDescriptor%GetTopologyTypeFromGridID(ID=GridID))
            else
                localNumberOfElements = this%UniformGridDescriptor%GetNumberOfElements()
                localNumberOfNodes = this%UniformGridDescriptor%GetNumberOfNodes()
                NodesPerElement = GetNumberOfNodesPerElement(this%UniformGridDescriptor%GetTopologyType())
            endif
            XDMFAttributeTypeName = GetXDMFAttributeTypeName(Type)
            XDMFCenterTypeName = GetXDMFCenterTypeName(Type)
            call attribute%open(xml_handler = this%file%xml_handler, &
                    Name          = name, &
                    AttributeType = XDMFAttributeTypeName, &
                    Center        = XDMFCenterTypeName)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/int(localNumberOfElements,I8P)*int(NodesPerElement,I8P)/), &
                    ItemType   = 'HyperSlab', &
                    Format     = 'HDF')
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I8P,1_I8P/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision=4) 
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/int(localNumberOfElements,I8P)*int(NodesPerElement,I8P)/), &
                    NumberType = 'Int', &
                    Format     = 'HDF', &
                    Precision  = 4) 
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%close(xml_handler = this%file%xml_handler)
            call attribute%close(xml_handler = this%file%xml_handler)
        endif                    
    end subroutine xdmf_contiguous_hyperslab_handler_WriteAttribute


end module xdmf_contiguous_hyperslab_handler
