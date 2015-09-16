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

    type, extends(xdmf_handler_t) :: xdmf_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
    contains
    private
!        procedure, public :: WriteMesh => hyperslab_WriteMesh
        procedure, public :: OpenFile       => xdmf_contiguous_hyperslab_OpenFile
        procedure, public :: CloseFile      => xdmf_contiguous_hyperslab_CloseFile
        procedure, public :: WriteTopology  => xdmf_contiguous_hyperslab_WriteTopology
        procedure, public :: WriteGeometry  => xdmf_contiguous_hyperslab_WriteGeometry
        procedure, public :: WriteAttribute => xdmf_contiguous_hyperslab_WriteAttribute
    end type xdmf_contiguous_hyperslab_handler_t

public :: xdmf_contiguous_hyperslab_handler_t

contains

    subroutine hyperslab_WriteMesh(this)
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this

    end subroutine


    subroutine xdmf_contiguous_hyperslab_OpenFile(this, filename)
    !-----------------------------------------------------------------
    !< Open a XDMF file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this     !< XDMF contiguous hyperslab handler
        character(len=*),                           intent(IN)    :: filename !< XDMF filename
        type(xdmf_grid_t)                                         :: grid     !< XDMF Grid type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            call this%file%set_filename(filename)
            call this%file%openfile()
            call grid%open(xml_handler = this%file%xml_handler, &
                    GridType='Collection', &
                    CollectionType='Spatial')
        endif
    end subroutine


    subroutine xdmf_contiguous_hyperslab_CloseFile(this)
    !-----------------------------------------------------------------
    !< Close a XDMF file for the contiguous HyperSlab strategy
    !< @TODO: inherited procedure
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XDMF contiguous hyperslab handler
        type(xdmf_grid_t)                                         :: grid !< XDMF Grid type
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            call grid%close(xml_handler=this%file%xml_handler)
            call this%file%closefile()
        endif
    end subroutine


    subroutine xdmf_contiguous_hyperslab_WriteTopology(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Topology into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this                    !< XDMF contiguous hyperslab handler
        integer(I4P),          optional,            intent(IN)    :: GridID                  !< Grid ID number
        type(xdmf_topology_t)                                     :: topology                !< XDMF Topology type
        type(xdmf_dataitem_t)                                     :: dataitem                !< XDMF Dataitem type
        integer(I8P)                                              :: LocalNumberOfNodes      !< Local number of nodes
        integer(I8P)                                              :: LocalNumberOfElements   !< Local number of elements
        integer(I8P)                                              :: SpaceDimension          !< Space dimension
        character(len=:), allocatable                             :: XMDFTopologyTypeName    !< String topology type identifier
    !-----------------------------------------------------------------
    !< @Note: allow different Topology or Geometry for each part of the spatial grid?
        if(this%MPIEnvironment%is_root()) then
            if(present(GridID)) then
                localNumberOfElements = this%SpatialGridDescriptor%GetNumberOfElementsFromGridID(ID=GridID)
                localNumberOfNodes = this%SpatialGridDescriptor%GetNumberOfNodesFromGridID(ID=GridID)
            else
                localNumberOfElements = this%UniformGridDescriptor%GetNumberOfElements()
                localNumberOfNodes = this%UniformGridDescriptor%GetNumberOfNodes()
            endif
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(this%UniformGridDescriptor%getTopologyType())
            SpaceDimension = GetSpaceDimension(this%UniformGridDescriptor%getGeometryType())

            call topology%open( xml_handler = this%file%xml_handler,&
                    Dimensions  = (/This%SpatialGridDescriptor%GetGlobalNumberOfNodes()*int(SpaceDimension,I8P)/),&
                    TopologyType=XMDFTopologyTypeName)
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/int(localNumberOfNodes,I8P)*int(SpaceDimension,I8P)/),&
                    ItemType    = 'HyperSlab',&
                    Format      = 'HDF')
            call dataitem%open( xml_handler = this%file%xml_handler,&
                    Dimensions     = (/3_I8P,1_I8P/),&
                    NumberType     = 'Int',&
                    Format         = 'XML',&
                    Precision      = 4 ) 
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open( xml_handler = this%file%xml_handler,&
                    Dimensions  = (/localNumberOfNodes/),&
                    NumberType  = 'Float',&
                    Format      = 'HDF',& 
                    Precision   = 8) 
            call dataitem%close(xml_handler=this%file%xml_handler)
            call dataitem%close(xml_handler=this%file%xml_handler)
            call topology%close(xml_handler=this%file%xml_handler)
        endif                    
    end subroutine xdmf_contiguous_hyperslab_WriteTopology


    subroutine xdmf_contiguous_hyperslab_WriteGeometry(this, GridID)
    !-----------------------------------------------------------------
    !< Write a XDMF Geometry into a opened file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XDMF contiguous hyperslab handler
        integer(I4P), optional,          intent(IN)    :: GridID                !< Grid ID number
        type(xdmf_geometry_t)                          :: geometry              !< XDMF Geometry type
        type(xdmf_dataitem_t)                          :: dataitem              !< XDMF Dataitem ttype
        integer(I8P)                                   :: LocalNumberOfElements !< Local number of elements
        integer(I8P)                                   :: LocalNumberOfNodes    !< Local number of nodes
        integer(I4P)                                   :: NodesPerElement       !< Number of nodes per element
        character(len=:), allocatable                  :: XDMFGeometryTypeName  !< String geometry type identifier
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            if(present(GridID)) then
                LocalNumberOfElements = this%SpatialGridDescriptor%GetNumberOfElementsFromGridID(ID=GridID)
                LocalNumberOfNodes = this%SpatialGridDescriptor%GetNumberOfNodesFromGridID(ID=GridID)
                NodesPerElement = GetNumberOfNodesPerElement(this%SpatialGridDescriptor%GetTopologyTypeFromGridID(ID=GridID))
            else
                localNumberOfElements = this%UniformGridDescriptor%GetNumberOfElements()
                localNumberOfNodes = this%UniformGridDescriptor%GetNumberOfNodes()
                NodesPerElement = GetNumberOfNodesPerElement(this%UniformGridDescriptor%GetTopologyType())
            endif
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(this%UniformGridDescriptor%GetGeometryType())

            call geometry%open( xml_handler  = this%file%xml_handler, &
                    GeometryType = XDMFGeometryTypeName)
            call dataitem%open( xml_handler = this%file%xml_handler, &
                    Dimensions  = (/int(localNumberOfElements,I8P)*int(NodesPerElement,I8P)/), &   
                    ItemType    = 'HyperSlab', &
                    Format      = 'HDF')
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/3_I8P,1_I8P/), &
                    NumberType = 'Int', &
                    Format     = 'XML', &
                    Precision  = 4) 
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%open(xml_handler = this%file%xml_handler, &
                    Dimensions = (/int(localNumberOfElements,I8P)*int(NodesPerElement,I8P)/), &
                    NumberType = 'Int', &
                    Format     = 'HDF', &
                    Precision  = 4) 
            call dataitem%close(xml_handler = this%file%xml_handler)
            call dataitem%close(xml_handler = this%file%xml_handler)
            call geometry%close(xml_handler = this%file%xml_handler)
        endif                    
    end subroutine xdmf_contiguous_hyperslab_WriteGeometry


    subroutine xdmf_contiguous_hyperslab_WriteAttribute(this, Name, Center, Type, GridID)
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
    end subroutine xdmf_contiguous_hyperslab_WriteAttribute


end module xdmf_contiguous_hyperslab_handler
