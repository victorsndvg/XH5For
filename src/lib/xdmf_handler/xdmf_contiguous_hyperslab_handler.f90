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

    type :: xdmf_metainfo_t
        character(len=:), allocatable :: XPath
        integer(I4P)                  :: Type
        integer(I4P)                  :: Center
        character(len=:), allocatable :: DataType
        integer(I4P)                  :: Precision
        integer(I4P)                  :: Dimension
    end type xdmf_metainfo_t

    type, extends(xdmf_handler_t) :: xdmf_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
        type(xdmf_metainfo_t)               :: geometry_info                   !< XDMF contiguous hyperslab geometry info 
        type(xdmf_metainfo_t)               :: topology_info                   !< XDMF contiguous hyperslab topology info 
        type(xdmf_metainfo_t),  allocatable :: attributes_info(:)              !< XDMF contiguous hyperslab attributes info 
    contains
    private
        procedure         :: SetGeometryInfo          => xdmf_contiguous_hyperslab_handler_SetGeometryInfo
        procedure         :: SetTopologyInfo          => xdmf_contiguous_hyperslab_handler_SetTopologyInfo
        procedure         :: SetLastAttributeInfo     => xdmf_contiguous_hyperslab_handler_SetLastAttributeInfo
        procedure         :: SetGeometry_R4P          => xdmf_contiguous_hyperslab_handler_SetGeometry_R4P
        procedure         :: SetGeometry_R8P          => xdmf_contiguous_hyperslab_handler_SetGeometry_R8P
        procedure         :: SetTopology_I4P          => xdmf_contiguous_hyperslab_handler_SetTopology_I4P
        procedure         :: SetTopology_I8P          => xdmf_contiguous_hyperslab_handler_SetTopology_I8P
        procedure         :: UpdateNumberOfAttributes => xdmf_contiguous_hyperslab_handler_UpdateNumberOfAttributes
        procedure         :: AppendAttribute_I4P      => xdmf_contiguous_hyperslab_handler_AppendAttribute_I4P
        procedure         :: AppendAttribute_I8P      => xdmf_contiguous_hyperslab_handler_AppendAttribute_I8P
        procedure         :: AppendAttribute_R4P      => xdmf_contiguous_hyperslab_handler_AppendAttribute_R4P
        procedure         :: AppendAttribute_R8P      => xdmf_contiguous_hyperslab_handler_AppendAttribute_R8P
        procedure, public :: OpenFile                 => xdmf_contiguous_hyperslab_handler_OpenFile
        procedure, public :: CloseFile                => xdmf_contiguous_hyperslab_handler_CloseFile
        generic,   public :: SetGeometry              => SetGeometry_R4P, &
                                                         SetGeometry_R8P
        generic,   public :: SetTopology              => SetTopology_I4P, &
                                                         SetTopology_I8P
        generic,   public :: AppendAttribute          => AppendAttribute_I4P, &
                                                         AppendAttribute_I8P, &
                                                         AppendAttribute_R4P, &
                                                         AppendAttribute_R8P
        procedure, public :: DeferredWrite            => xdmf_contiguous_hyperslab_handler_DeferredWrite
        procedure, public :: WriteGeometry            => xdmf_contiguous_hyperslab_handler_WriteGeometry
        procedure, public :: WriteTopology            => xdmf_contiguous_hyperslab_handler_WriteTopology
        procedure, public :: WriteAttribute           => xdmf_contiguous_hyperslab_handler_WriteAttribute
        procedure         :: WriteAttributes          => xdmf_contiguous_hyperslab_handler_WriteAttributes
    end type xdmf_contiguous_hyperslab_handler_t

public :: xdmf_contiguous_hyperslab_handler_t

contains

    subroutine xdmf_contiguous_hyperslab_handler_SetGeometryInfo(this, XPath, Precision, Dimension)
    !-----------------------------------------------------------------
    !< Set XDMF geometry info
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this                  !< XMDF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: XPath                 !< XDMF XPath to the HDF5 connetivities
        integer(I4P),                               intent(IN)    :: Precision  !< Precision of the Coordinates in the HDF5 file
        integer(I4P),                               intent(IN)    :: Dimension  !< Dimensions of the Coordinates array in the HDF5 file
    !-----------------------------------------------------------------
        this%geometry_info%XPath     = XPath
        this%geometry_info%Precision = Precision
        this%geometry_info%Dimension = Dimension
    end subroutine xdmf_contiguous_hyperslab_handler_SetGeometryInfo


    subroutine xdmf_contiguous_hyperslab_handler_SetTopologyInfo(this, XPath, Precision, Dimension)
    !-----------------------------------------------------------------
    !< Set XDMF geometry info
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this      !< XMDF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: XPath     !< XDMF XPath to the HDF5 coordinates
        integer(I4P),                               intent(IN)    :: Precision !< Precision of the coordinates in the HDF5 file
        integer(I4P),                               intent(IN)    :: Dimension !< Dimensions of the coordinates array in the HDF5 file
    !-----------------------------------------------------------------
        this%topology_info%XPath      = XPath
        this%topology_info%Precision  = Precision
        this%topology_info%Dimension  = Dimension
    end subroutine xdmf_contiguous_hyperslab_handler_SetTopologyInfo


    subroutine xdmf_contiguous_hyperslab_handler_SetLastAttributeInfo(this, XPath, Type, DataType, Center, Precision, Dimension)
    !-----------------------------------------------------------------
    !< Set XDMF geometry info
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this      !< XMDF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: XPath     !< XDMF XPath to the HDF5 coordinates
        integer(I4P),                               intent(IN)    :: Type      !< XDMF attribute type (Scalar, Vector, Tensor, etc.)
        character(len=*),                           intent(IN)    :: DataType  !< XDMF attribute data type (Int or  Float)
        integer(I4P),                               intent(IN)    :: Center    !< Center property of the attribute (Node, Face, Edge, Cell or Grid)
        integer(I4P),                               intent(IN)    :: Precision !< Precision of the attribute in the HDF5 file
        integer(I4P),                               intent(IN)    :: Dimension !< Dimensions of the attribute array in the HDF5 file
    !-----------------------------------------------------------------
        this%attributes_info(this%NumberOfAttributes)%XPath      = XPath
        this%attributes_info(this%NumberOfAttributes)%DataType   = DataType
        this%attributes_info(this%NumberOfAttributes)%Center     = Center
        this%attributes_info(this%NumberOfAttributes)%Precision  = Precision
        this%attributes_info(this%NumberOfAttributes)%Dimension  = Dimension
    end subroutine xdmf_contiguous_hyperslab_handler_SetLastAttributeInfo


    subroutine xdmf_contiguous_hyperslab_handler_SetGeometry_R4P(this, Coordinates)
    !-----------------------------------------------------------------
    !< Add R4P geometry info to the handler. Used for deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XDMF contiguous hyperslab handler
        real(R4P),                                  intent(IN)    :: Coordinates(:) !< Grid coordinates
    !-----------------------------------------------------------------
        call this%SetGeometryInfo(XPath='Coordinates', Precision=4, Dimension=1)
    end subroutine xdmf_contiguous_hyperslab_handler_SetGeometry_R4P


    subroutine xdmf_contiguous_hyperslab_handler_SetGeometry_R8P(this, Coordinates)
    !-----------------------------------------------------------------
    !< Add R8P geometry info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this           !< XDMF contiguous hyperslab handler
        real(R8P),                                  intent(IN)    :: Coordinates(:) !< Grid coordinates
    !-----------------------------------------------------------------
        call this%SetGeometryInfo(XPath='Coordinates', Precision=8, Dimension=1)
    end subroutine xdmf_contiguous_hyperslab_handler_SetGeometry_R8P


    subroutine xdmf_contiguous_hyperslab_handler_SetTopology_I4P(this, Connectivities)
    !-----------------------------------------------------------------
    !< Add I4P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler
        integer(I4P),                               intent(IN)    :: Connectivities(:) !< Grid Connectivities
    !-----------------------------------------------------------------
        call this%SetTopologyInfo(XPath='Connectivities', Precision=4, Dimension=1)
    end subroutine xdmf_contiguous_hyperslab_handler_SetTopology_I4P


    subroutine xdmf_contiguous_hyperslab_handler_SetTopology_I8P(this, Connectivities)
    !-----------------------------------------------------------------
    !< Add I8P topology info to the handler. Used in deferred writing 
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler
        integer(I8P),                               intent(IN)    :: Connectivities(:)    !< Grid Connectivities
    !-----------------------------------------------------------------
        call this%SetTopologyInfo(XPath='Connectivities', Precision=8, Dimension=1)
    end subroutine xdmf_contiguous_hyperslab_handler_SetTopology_I8P


    subroutine xdmf_contiguous_hyperslab_handler_UpdateNumberOfAttributes(this)
    !-----------------------------------------------------------------
    !< Increase the number of attributes and allocate the attributes_info array to the right size
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this              !< XDMF contiguous hyperslab handler
        type(xdmf_metainfo_t),  allocatable                       :: aux_attrs_info(:) !< Aux XDMF contiguous hyperslab attributes info 
        integer(I4P)                                              :: indx
    !-----------------------------------------------------------------
        if(.not. allocated(this%attributes_info)) then
            this%NumberOfAttributes = 0
            allocate(this%attributes_info(1))
        elseif(size(this%attributes_info) < (this%NumberOfAttributes+1)) then
            allocate(aux_attrs_info(this%NumberOfAttributes))
            aux_attrs_info(:) = this%attributes_info(:)
            deallocate(this%attributes_info); allocate(this%attributes_info(this%NumberOfAttributes+1))
            this%attributes_info(1:this%NumberOfAttributes) = aux_attrs_info(1:this%NumberOfAttributes)
            deallocate(aux_attrs_info)
        endif
        this%NumberOfAttributes = this%NumberOfAttributes + 1
    end subroutine xdmf_contiguous_hyperslab_handler_UpdateNumberOfAttributes


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
        call this%UpdateNumberOfAttributes()
        call this%SetLastAttributeInfo(XPath=trim(adjustl(Name)), Type=Type, DataType='Int', Center=Center, Precision=4, Dimension=1)
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
        call this%UpdateNumberOfAttributes()
        call this%SetLastAttributeInfo(XPath=trim(adjustl(Name)), Type=Type, DataType='Int', Center=Center, Precision=8, Dimension=1)
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
        call this%UpdateNumberOfAttributes()
        call this%SetLastAttributeInfo(XPath=trim(adjustl(Name)), Type=Type, DataType='Float', Center=Center, Precision=8, Dimension=1)
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
        call this%UpdateNumberOfAttributes()
        call this%SetLastAttributeInfo(XPath=trim(adjustl(Name)), Type=Type, DataType='Float', Center=Center, Precision=8, Dimension=1)
    end subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_R8P


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
            call this%WriteAttributes(GridID = IDidx)
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
                    Dimensions     = (/3_I4P,this%topology_info%Dimension/),&
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
                    Precision   = this%topology_info%Precision) 
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
                    Dimensions = (/3_I4P,this%geometry_info%Dimension/), &
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
                    Precision  = this%geometry_info%Precision) 
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
            XDMFCenterTypeName = GetXDMFCenterTypeName(Center)
            call attribute%open(xml_handler = this%file%xml_handler, &
                    Name          = name, &
                    AttributeType = XDMFAttributeTypeName, &
                    Center        = XDMFCenterTypeName)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/int(localNumberOfElements,I8P)*int(NodesPerElement,I8P)/), &
                    ItemType   = 'HyperSlab', &
                    Format     = 'HDF')
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I4P,1_I4P/), &
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
        integer(I8P)                                              :: LocalNumberOfNodes     !< Local number of nodes
        integer(I8P)                                              :: Start                 !< Hyperslab start
        integer(I8P)                                              :: Count                 !< Hyperslab count
        character(len=:), allocatable                             :: XDMFAttributeTypeName  !< String Attibute type identifier
        character(len=:), allocatable                             :: XDMFCenterTypeName     !< String Attribute Center identifier
        integer(I4P)                                              :: indx           
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            if(present(GridID)) then
                localNumberOfNodes = this%SpatialGridDescriptor%GetNumberOfNodesFromGridID(ID=GridID)
                Start = this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=GridID)
            else
                localNumberOfNodes = this%UniformGridDescriptor%GetNumberOfNodes()
                Start = 0
            endif
            Count = localNumberOfNodes
            do indx = 1, this%NumberOfAttributes
                XDMFAttributeTypeName = GetXDMFAttributeTypeName(this%attributes_info(indx)%Type)
                XDMFCenterTypeName = GetXDMFCenterTypeName(this%attributes_info(indx)%Center)
                call attribute%open(xml_handler = this%file%xml_handler, &
                        Name          = this%attributes_info(indx)%XPath,&
                        AttributeType = XDMFAttributeTypeName,           &
                        Center        = XDMFCenterTypeName)
                call dataitem%open(xml_handler = this%file%xml_handler,                           &
                        Dimensions = (/int(localNumberOfNodes,I8P)/), &
                        ItemType   = 'HyperSlab',                                                 &
                        Format     = 'HDF')
                call dataitem%open(xml_handler = this%file%xml_handler,             &
                        Dimensions = (/3_I4P,this%attributes_info(indx)%Dimension/), &
                        NumberType = 'Int', &
                        Format     = 'XML', &
                        Precision=4) 
                call chardata%open( xml_handler = this%file%xml_handler, &
                        Data = (/Start,1_I8P,Count/) )
                call dataitem%close(xml_handler = this%file%xml_handler)
                call dataitem%open(xml_handler = this%file%xml_handler,                           &
                        Dimensions = (/int(localNumberOfNodes,I8P)/), &
                        NumberType = this%attributes_info(indx)%DataType,                         &
                        Format     = 'HDF',                                                       &
                        Precision  = this%attributes_info(indx)%Precision) 
                call chardata%open( xml_handler = this%file%xml_handler, &
                        Data = trim(adjustl(this%prefix))//'.h5'//':'//this%attributes_info(indx)%XPath)
                call dataitem%close(xml_handler = this%file%xml_handler)
                call dataitem%close(xml_handler = this%file%xml_handler)
                call attribute%close(xml_handler = this%file%xml_handler)
            enddo
        endif                    
    end subroutine xdmf_contiguous_hyperslab_handler_WriteAttributes


end module xdmf_contiguous_hyperslab_handler
