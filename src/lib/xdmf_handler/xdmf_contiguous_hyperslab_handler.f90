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
        procedure, public :: OpenFile       => contiguous_hyperslab_OpenFile
        procedure, public :: CloseFile      => contiguous_hyperslab_CloseFile
        procedure, public :: WriteTopology  => contiguous_hyperslab_WriteTopology
        procedure, public :: WriteGeometry  => contiguous_hyperslab_WriteGeometry
        procedure, public :: WriteAttribute => contiguous_hyperslab_WriteAttribute
    end type xdmf_contiguous_hyperslab_handler_t

public :: xdmf_contiguous_hyperslab_handler_t

contains

    subroutine hyperslab_WriteMesh(this)
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this

    end subroutine

    subroutine contiguous_hyperslab_OpenFile(this, filename)
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this
        character(len=*),      intent(IN)    :: filename
        type(xdmf_grid_t)                    :: grid
        if(this%is_root()) then
            call this%file%set_filename(filename)
            call this%file%openfile()
            call grid%open(xml_handler = this%file%xml_handler, &
                    GridType='Collection', &
                    CollectionType='Spatial')
        endif
    end subroutine

    subroutine contiguous_hyperslab_CloseFile(this)
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT)    :: this
        type(xdmf_grid_t)                    :: grid
        if(this%is_root()) then
            call grid%close(xml_handler=this%file%xml_handler)
            call this%file%closefile()
        endif
    end subroutine

    subroutine contiguous_hyperslab_WriteTopology(this, GridNumber)
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this
        integer(I4P), optional,          intent(IN)    :: GridNumber
        type(xdmf_topology_t)                          :: topology
        type(xdmf_dataitem_t)                          :: dataitem
        integer(I8P)                                   :: LocalNumberOfNodes
        integer(I8P)                                   :: LocalNumberOfElements
        integer(I8P)                                   :: SpaceDimension
        character(len=:), allocatable                  :: XMDFTopologyTypeName

!< @Note: allow different Topology or Geometry for each part of the spatial grid?
        if(this%is_root()) then
            if(present(GridNumber)) then
                localNumberOfElements = this%DistributedDataHandler%GetNumberOfElementsFromTask(TaskID=GridNumber)
                localNumberOfNodes = this%DistributedDataHandler%GetNumberOfNodesFromTask(TaskID=GridNumber)
            else
                localNumberOfElements = this%GetNumberOfElements()
                localNumberOfNodes = this%GetNumberOfNodes()
            endif
            XMDFTopologyTypeName = GetXDMFTopologyTypeName(this%getTopologyType())
            SpaceDimension = GetSpaceDimension(this%getGeometryType())

            call topology%open( xml_handler = this%file%xml_handler,&
                    Dimensions  = (/This%DistributedDataHandler%GetGlobalNumberOfNodes()*int(SpaceDimension,I8P)/),&
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
    end subroutine contiguous_hyperslab_WriteTopology

    subroutine contiguous_hyperslab_WriteGeometry(this, GridNumber)
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this
        integer(I4P), optional,          intent(IN)    :: GridNumber
        type(xdmf_geometry_t)                          :: geometry
        type(xdmf_dataitem_t)                          :: dataitem
        integer(I8P)                                   :: LocalNumberOfElements
        integer(I8P)                                   :: LocalNumberOfNodes
        integer(I4P)                                   :: NodesPerElement
        character(len=:), allocatable                  :: XDMFGeometryTypeName

        if(this%is_root()) then
            if(present(GridNumber)) then
                LocalNumberOfElements = this%DistributedDataHandler%GetNumberOfElementsFromTask(TaskID=GridNumber)
                LocalNumberOfNodes = this%DistributedDataHandler%GetNumberOfNodesFromTask(TaskID=GridNumber)
                NodesPerElement = GetNumberOfNodesPerElement(this%DistributedDataHandler%GetTopologyTypeFromTask(taskID=GridNumber))
            else
                localNumberOfElements = this%GetNumberOfElements()
                localNumberOfNodes = this%GetNumberOfNodes()
                NodesPerElement = GetNumberOfNodesPerElement(this%GetTopologyType())
            endif
            XDMFGeometryTypeName = GetXDMFGeometryTypeName(this%GetGeometryType())

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
    end subroutine contiguous_hyperslab_WriteGeometry


    subroutine contiguous_hyperslab_WriteAttribute(this, Name, Center, Type, GridNumber)
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this
        character(len=*),                intent(IN)    :: Name
        integer(I4P),                    intent(IN)    :: Center
        integer(I4P),                    intent(IN)    :: Type
        integer(I4P), optional,          intent(IN)    :: GridNumber
        type(xdmf_attribute_t)                         :: attribute
        type(xdmf_dataitem_t)                          :: dataitem
        integer(I8P)                                   :: LocalNumberOfElements
        integer(I8P)                                   :: LocalNumberOfNodes
        integer(I4P)                                   :: NodesPerElement
        character(len=:), allocatable                  :: XDMFAttributeTypeName
        character(len=:), allocatable                  :: XDMFCenterTypeName

        if(this%is_root()) then
            if(present(GridNumber)) then
                localNumberOfElements = this%DistributedDataHandler%GetNumberOfElementsFromTask(TaskID=GridNumber)
                localNumberOfNodes = this%DistributedDataHandler%GetNumberOfNodesFromTask(TaskID=GridNumber)
                NodesPerElement = GetNumberOfNodesPerElement(this%DistributedDataHandler%GetTopologyTypeFromTask(TaskID=GridNumber))
            else
                localNumberOfElements = this%GetNumberOfElements()
                localNumberOfNodes = this%GetNumberOfNodes()
                NodesPerElement = GetNumberOfNodesPerElement(this%GetTopologyType())
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
    end subroutine contiguous_hyperslab_WriteAttribute




end module xdmf_contiguous_hyperslab_handler
