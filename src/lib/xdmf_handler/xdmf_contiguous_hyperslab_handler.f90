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


    type, abstract, extends(xdmf_handler_t) :: xdmf_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
    contains
    private
        procedure(xdmf_contiguous_hyperslab_handler_WriteGeometry), deferred :: WriteGeometry
        procedure(xdmf_contiguous_hyperslab_handler_WriteTopology), deferred :: WriteTopology
        procedure         :: CalculateAttributeDimensions => xdmf_contiguous_hyperslab_handler_CalculateAttributeDimensions
        procedure         :: AppendAttribute_I4P          => xdmf_contiguous_hyperslab_handler_AppendAttribute_I4P
        procedure         :: AppendAttribute_I8P          => xdmf_contiguous_hyperslab_handler_AppendAttribute_I8P
        procedure         :: AppendAttribute_R4P          => xdmf_contiguous_hyperslab_handler_AppendAttribute_R4P
        procedure         :: AppendAttribute_R8P          => xdmf_contiguous_hyperslab_handler_AppendAttribute_R8P
        procedure         :: WriteAttributes              => xdmf_contiguous_hyperslab_handler_WriteAttributes
        procedure, public :: GetUniqueNodeByTag           => xdmf_contiguous_hyperslab_handler_GetUniqueNodeByTag
        procedure, public :: GetFirstChildByTag           => xdmf_contiguous_hyperslab_handler_GetFirstChildByTag
        procedure, public :: GetDataItemXPath             => xdmf_contiguous_hyperslab_handler_GetDataItemXPath
        procedure         :: OpenGrid                     => xdmf_contiguous_hyperslab_handler_OpenGrid
        procedure         :: CloseGrid                    => xdmf_contiguous_hyperslab_handler_CloseGrid
        procedure, public :: OpenFile                     => xdmf_contiguous_hyperslab_handler_OpenFile
        procedure, public :: Serialize                    => xdmf_contiguous_hyperslab_handler_Serialize
        procedure, public :: CloseFile                    => xdmf_contiguous_hyperslab_handler_CloseFile
        procedure, public :: Free                         => xdmf_contiguous_hyperslab_handler_Free
    end type xdmf_contiguous_hyperslab_handler_t

    abstract interface
        subroutine xdmf_contiguous_hyperslab_handler_WriteGeometry(this, GridID)
            import I4P
            import xdmf_contiguous_hyperslab_handler_t
            class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this    !< XDMF contiguous hyperslab handler
            integer(I4P), optional,                     intent(IN) :: GridID     !< Grid ID number
        end subroutine xdmf_contiguous_hyperslab_handler_WriteGeometry

        subroutine xdmf_contiguous_hyperslab_handler_WriteTopology(this, GridID)
            import I4P
            import xdmf_contiguous_hyperslab_handler_t
            class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this    !< XDMF contiguous hyperslab handler
            integer(I4P), optional,                     intent(IN) :: GridID     !< Grid ID number
        end subroutine xdmf_contiguous_hyperslab_handler_WriteTopology
    end interface

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
        call this%UniformGridDescriptor%SetLastAttributeMetadata( &
                        Name=trim(adjustl(Name)),                 &
                        Type=Type, DataType='Int',                &
                        Center=Center,                            &
                        Precision=4,                              &
                        ArrayDimensions=(/size(Attribute, dim=1)/))
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
        call this%UniformGridDescriptor%SetLastAttributeMetadata( &
                        Name=trim(adjustl(Name)),                 &
                        Type=Type, DataType='Int',                &
                        Center=Center,                            &
                        Precision=8,                              &
                        ArrayDimensions=(/size(Attribute, dim=1)/))

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
        call this%UniformGridDescriptor%SetLastAttributeMetadata( &
                        Name=trim(adjustl(Name)),                 &
                        Type=Type, DataType='Float',              &
                        Center=Center,                            &
                        Precision=4,                              &
                        ArrayDimensions=(/size(Attribute, dim=1)/))

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
        call this%UniformGridDescriptor%SetLastAttributeMetadata( &
                        Name=trim(adjustl(Name)),                 &
                        Type=Type, DataType='Float',              &
                        Center=Center,                            &
                        Precision=8,                              &
                        ArrayDimensions=(/size(Attribute, dim=1)/))

    end subroutine xdmf_contiguous_hyperslab_handler_AppendAttribute_R8P


    subroutine xdmf_contiguous_hyperslab_handler_CalculateAttributeDimensions(this, GridID, Center, GlobalNumberOfData, LocalNumberOfData, DataOffset)
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
                LocalNumberOfData  = this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=GridID)
                DataOffset         = this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=GridID)
            case (XDMF_ATTRIBUTE_CENTER_CELL)
                GlobalNumberOfData = this%SpatialGridDescriptor%GetGlobalNumberOfElements()
                LocalNumberOfData  = this%SpatialGridDescriptor%GetNumberOfElementsPerGridID(ID=GridID)
                DataOffset         = this%SpatialGridDescriptor%GetElementOffsetPerGridID(ID=GridID)
            case (XDMF_ATTRIBUTE_CENTER_GRID)
                GlobalNumberOfData = this%MPIEnvironment%get_comm_size()
                LocalNumberOfData  = 1_I8P
                DataOffset         = GridID
            case Default
                GlobalNumberOfData = this%SpatialGridDescriptor%GetGlobalNumberOfNodes()
                LocalNumberOfData  = this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=GridID)
                DataOffset         = this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=GridID)
        end select
    end subroutine xdmf_contiguous_hyperslab_handler_CalculateAttributeDimensions


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
        integer(I4P)                                              :: i              !< Index for a loop in Childrens
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


    subroutine xdmf_contiguous_hyperslab_handler_CloseFile(this)
    !-----------------------------------------------------------------
    !< Close a XDMF file for the contiguous HyperSlab strategy
    !< @TODO: inherited procedure
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XDMF contiguous hyperslab handler
        type(xdmf_grid_t)                                         :: grid   !< XDMF Grid type
        type(xdmf_domain_t)                                       :: domain !< XDMF Domain type
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
        integer(I4P)                                              :: DimensionsSize         !< Size of the attribute shape
        integer(I4P)                                              :: indx           
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            do indx = 1, this%UniformGridDescriptor%GetNumberOfAttributes()
                if(present(GridID)) then
                    call this%CalculateAttributeDimensions(                                           & 
                        GridID = GridID,                                                              &
                        Center = this%UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx), &
                        GlobalNumberOfData = GlobalNumberOfData,                                      &
                        LocalNumberOfData = LocalNumberOfData,                                        &
                        DataOffset = DataOffset)
                else
                    call this%CalculateAttributeDimensions(                                           &
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
                DimensionsSize = size(this%UniformGridDescriptor%GetAttributeArrayDimensions(AttributeNumber=indx), dim=1)
                call attribute%open(xml_handler = this%file%xml_handler,                                   &
                        Name          = this%UniformGridDescriptor%GetAttributeName(AttributeNumber=indx), &
                        AttributeType = XDMFAttributeTypeName,                                             &
                        Center        = XDMFCenterTypeName)
                call dataitem%open(xml_handler = this%file%xml_handler,                           &
                        Dimensions = (/int(LocalNumberOfData*int(NumberOfComponents,I8P),I8P)/),  &
                        ItemType   = 'HyperSlab',                                                 &
                        Format     = 'HDF')
                call dataitem%open(xml_handler = this%file%xml_handler,              &
                        Dimensions = (/3_I4P, DimensionsSize/), &
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
