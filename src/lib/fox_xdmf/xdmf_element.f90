module xdmf_element
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF element handling module
!< @TODO enhance and generalice the module.
!< @TODO add comments.
!--------------------------------------------------------------------- -----------------------------------------------------------

use FoX_wxml, only: xmlf_t
use FoX_dom,  only: Node, getTagName

implicit none

    type, abstract:: xdmf_element_t
    private
        character(len=:), allocatable :: Tag
        logical, public               :: warn = .true. !< @Note Remove this variable or set to .false.. .true. Only valid under development
    contains
    private
        procedure, public                 :: get_tag             => element_get_tag
        procedure, public                 :: set_tag             => element_set_tag
        procedure, public                 :: node_is_xdmf        => element_node_is_xdmf
        procedure, public                 :: node_is_domain      => element_node_is_domain
        procedure, public                 :: node_is_grid        => element_node_is_grid
        procedure, public                 :: node_is_topology    => element_node_is_topology
        procedure, public                 :: node_is_geometry    => element_node_is_geometry
        procedure, public                 :: node_is_attribute   => element_node_is_attribute
        procedure, public                 :: node_is_dataitem    => element_node_is_dataitem
        procedure, public                 :: node_is_time        => element_node_is_time
        procedure, public                 :: node_is_information => element_node_is_information
        procedure(element_default_initialization), deferred :: default_initialization
        procedure(element_free),   public,         deferred :: free

        procedure(element_parse),  public,         deferred :: parse
        procedure(element_close),  public,         deferred :: close
        procedure(element_print),  public,         deferred :: print
    end type xdmf_element_t

    abstract interface
        subroutine element_default_initialization(this)
            import xdmf_element_t
            class(xdmf_element_t), intent(INOUT) :: this
        end subroutine element_default_initialization

        subroutine element_free(this)
            import xdmf_element_t
            class(xdmf_element_t), intent(INOUT) :: this
        end subroutine element_free

        subroutine element_open(this)
            import xdmf_element_t
            class(xdmf_element_t), intent(INOUT) :: this
        end subroutine element_open

        subroutine element_parse(this, DOMNode)
            import xdmf_element_t
            import Node
            class(xdmf_element_t), intent(INOUT) :: this
            type(Node), pointer,   intent(IN)    :: DOMNode
        end subroutine element_parse

        subroutine element_close(this, xml_handler)
            import xdmf_element_t
            import xmlf_t
            class(xdmf_element_t), intent(IN)    :: this
            type(xmlf_t),          intent(INOUT) :: xml_handler
        end subroutine element_close

        subroutine element_print(this)
            import xdmf_element_t
            class(xdmf_element_t), intent(IN) :: this
        end subroutine element_print
    end interface


contains

    function element_get_tag(this) result(tag)
    !----------------------------------------------------------------- 
    !< Get the element Tag
    !----------------------------------------------------------------- 
        class(xdmf_element_t) :: this
        character(len=:), allocatable :: tag
    !----------------------------------------------------------------- 
        if(allocated(this%tag)) tag = this%tag
    end function element_get_tag


    subroutine element_set_tag(this, tag) 
    !-----------------------------------------------------------------
    !< Set the element Tag
    !----------------------------------------------------------------- 
        class(xdmf_element_t) :: this
        character(len=*), intent(IN) :: tag
    !----------------------------------------------------------------- 
        this%Tag = tag
    end subroutine element_set_tag


    function element_node_is_xdmf(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Xdmf
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                  !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode               !< FoX DOM Node 
        logical                              :: element_node_is_xdmf  !< True if DOM Node is a XDMF Xdmf
    !----------------------------------------------------------------- 
        element_node_is_xdmf = (getTagName(DOMNode) == 'Xdmf')
    end function element_node_is_Xdmf


    function element_node_is_domain(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Domain
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                  !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode               !< FoX DOM Node 
        logical                              :: element_node_is_domain!< True if DOM Node is a XDMF Domain
    !----------------------------------------------------------------- 
        element_node_is_domain = (getTagName(DOMNode) == 'Domain')
    end function element_node_is_domain


    function element_node_is_grid(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Grid
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                  !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode               !< FoX DOM Node 
        logical                              :: element_node_is_grid  !< True if DOM Node is a XDMF Grid
    !----------------------------------------------------------------- 
        element_node_is_grid = (getTagName(DOMNode) == 'Grid')
    end function element_node_is_grid


    function element_node_is_topology(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Topology
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                     !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                  !< FoX DOM Node 
        logical                              :: element_node_is_topology !< True if DOM Node is a XDMF Topology
    !----------------------------------------------------------------- 
        element_node_is_topology = (getTagName(DOMNode) == 'Topology')
    end function element_node_is_topology


    function element_node_is_geometry(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Geometry
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                     !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                  !< FoX DOM Node 
        logical                              :: element_node_is_geometry !< True if DOM Node is a XDMF Geometry
    !----------------------------------------------------------------- 
        element_node_is_geometry = (getTagName(DOMNode) == 'Geometry')
    end function element_node_is_geometry


    function element_node_is_attribute(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Attribute
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                      !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                   !< FoX DOM Node 
        logical                              :: element_node_is_attribute !< True if DOM Node is a XDMF Attribute
    !----------------------------------------------------------------- 
        element_node_is_attribute = (getTagName(DOMNode) == 'Attribute')
    end function element_node_is_attribute


    function element_node_is_dataitem(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a DataItem
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                     !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                  !< FoX DOM Node 
        logical                              :: element_node_is_dataitem !< True if DOM Node is a XDMF DataItem
    !----------------------------------------------------------------- 
        element_node_is_dataitem = (getTagName(DOMNode) == 'DataItem')
    end function element_node_is_dataitem


    function element_node_is_time(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Time
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                  !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode               !< FoX DOM Node 
        logical                              :: element_node_is_time  !< True if DOM Node is a XDMF Time
    !----------------------------------------------------------------- 
        element_node_is_time = (getTagName(DOMNode) == 'Time')
    end function element_node_is_time


    function element_node_is_information(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Information
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                        !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                     !< FoX DOM Node 
        logical                              :: element_node_is_information !< True if DOM Node is a XDMF Information
    !----------------------------------------------------------------- 
        element_node_is_information = (getTagName(DOMNode) == 'Information')
    end function element_node_is_information

end module xdmf_element
