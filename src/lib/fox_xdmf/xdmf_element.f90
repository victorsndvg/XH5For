module xdmf_element
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF element handling module
!< @TODO enhance and generalice the module.
!< @TODO add comments.
!--------------------------------------------------------------------- -----------------------------------------------------------


use FoX_wxml,     only: xmlf_t
use FoX_dom,      only: Node, getTagName
use IR_Precision, only: I4P

implicit none

    type, abstract:: xdmf_element_t
    private
        character(len=:), allocatable :: Tag
        logical, public               :: warn = .true. !< @Note Remove this variable or set to .false.. .true. Only valid under development
    contains
    private
        procedure                         :: element_open        => xdmf_element_open
        procedure, public                 :: get_tag             => xdmf_element_get_tag
        procedure, public                 :: set_tag             => xdmf_element_set_tag
        procedure, public                 :: node_is_xdmf        => xdmf_element_node_is_xdmf
        procedure, public                 :: node_is_domain      => xdmf_element_node_is_domain
        procedure, public                 :: node_is_grid        => xdmf_element_node_is_grid
        procedure, public                 :: node_is_topology    => xdmf_element_node_is_topology
        procedure, public                 :: node_is_geometry    => xdmf_element_node_is_geometry
        procedure, public                 :: node_is_attribute   => xdmf_element_node_is_attribute
        procedure, public                 :: node_is_dataitem    => xdmf_element_node_is_dataitem
        procedure, public                 :: node_is_time        => xdmf_element_node_is_time
        procedure, public                 :: node_is_information => xdmf_element_node_is_information
        generic,                        public                   :: open => element_open
        procedure(xdmf_element_default_initialization), deferred :: default_initialization
        procedure(xdmf_element_free),   public,         deferred :: free
        procedure(xdmf_element_parse),  public,         deferred :: parse
        procedure(xdmf_element_close),  public,         deferred :: close
        procedure(xdmf_element_print),  public,         deferred :: print
    end type xdmf_element_t

    abstract interface
        subroutine xdmf_element_default_initialization(this)
            import xdmf_element_t
            class(xdmf_element_t), intent(INOUT) :: this
        end subroutine xdmf_element_default_initialization

        subroutine xdmf_element_free(this)
            import xdmf_element_t
            class(xdmf_element_t), intent(INOUT) :: this
        end subroutine xdmf_element_free

        subroutine xdmf_element_parse(this, DOMNode)
            import xdmf_element_t
            import Node
            class(xdmf_element_t), intent(INOUT) :: this
            type(Node), pointer,   intent(IN)    :: DOMNode
        end subroutine xdmf_element_parse

        subroutine xdmf_element_close(this, xml_handler)
            import xdmf_element_t
            import xmlf_t
            class(xdmf_element_t), intent(IN)    :: this
            type(xmlf_t),          intent(INOUT) :: xml_handler
        end subroutine xdmf_element_close

        subroutine xdmf_element_print(this, IndentationLevel)
            import xdmf_element_t
            import I4P
            class(xdmf_element_t),  intent(IN) :: this
            integer(I4P), optional, intent(IN) :: IndentationLevel
        end subroutine xdmf_element_print
    end interface


contains


    subroutine xdmf_element_open(this)
    !----------------------------------------------------------------- 
    !< Open and XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this
    !----------------------------------------------------------------- 
    end subroutine xdmf_element_open


    function xdmf_element_get_tag(this) result(tag)
    !----------------------------------------------------------------- 
    !< Get the element Tag
    !----------------------------------------------------------------- 
        class(xdmf_element_t) :: this
        character(len=:), allocatable :: tag
    !----------------------------------------------------------------- 
        if(allocated(this%tag)) tag = this%tag
    end function xdmf_element_get_tag


    subroutine xdmf_element_set_tag(this, tag) 
    !-----------------------------------------------------------------
    !< Set the element Tag
    !----------------------------------------------------------------- 
        class(xdmf_element_t) :: this
        character(len=*), intent(IN) :: tag
    !----------------------------------------------------------------- 
        this%Tag = tag
    end subroutine xdmf_element_set_tag


    function xdmf_element_node_is_xdmf(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Xdmf
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                  !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode               !< FoX DOM Node 
        logical                              :: xdmf_element_node_is_xdmf  !< True if DOM Node is a XDMF Xdmf
    !----------------------------------------------------------------- 
        xdmf_element_node_is_xdmf = (getTagName(DOMNode) == 'Xdmf')
    end function xdmf_element_node_is_Xdmf


    function xdmf_element_node_is_domain(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Domain
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                  !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode               !< FoX DOM Node 
        logical                              :: xdmf_element_node_is_domain!< True if DOM Node is a XDMF Domain
    !----------------------------------------------------------------- 
        xdmf_element_node_is_domain = (getTagName(DOMNode) == 'Domain')
    end function xdmf_element_node_is_domain


    function xdmf_element_node_is_grid(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Grid
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                  !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode               !< FoX DOM Node 
        logical                              :: xdmf_element_node_is_grid  !< True if DOM Node is a XDMF Grid
    !----------------------------------------------------------------- 
        xdmf_element_node_is_grid = (getTagName(DOMNode) == 'Grid')
    end function xdmf_element_node_is_grid


    function xdmf_element_node_is_topology(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Topology
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                     !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                  !< FoX DOM Node 
        logical                              :: xdmf_element_node_is_topology !< True if DOM Node is a XDMF Topology
    !----------------------------------------------------------------- 
        xdmf_element_node_is_topology = (getTagName(DOMNode) == 'Topology')
    end function xdmf_element_node_is_topology


    function xdmf_element_node_is_geometry(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Geometry
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                     !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                  !< FoX DOM Node 
        logical                              :: xdmf_element_node_is_geometry !< True if DOM Node is a XDMF Geometry
    !----------------------------------------------------------------- 
        xdmf_element_node_is_geometry = (getTagName(DOMNode) == 'Geometry')
    end function xdmf_element_node_is_geometry


    function xdmf_element_node_is_attribute(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Attribute
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                      !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                   !< FoX DOM Node 
        logical                              :: xdmf_element_node_is_attribute !< True if DOM Node is a XDMF Attribute
    !----------------------------------------------------------------- 
        xdmf_element_node_is_attribute = (getTagName(DOMNode) == 'Attribute')
    end function xdmf_element_node_is_attribute


    function xdmf_element_node_is_dataitem(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a DataItem
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                     !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                  !< FoX DOM Node 
        logical                              :: xdmf_element_node_is_dataitem !< True if DOM Node is a XDMF DataItem
    !----------------------------------------------------------------- 
        xdmf_element_node_is_dataitem = (getTagName(DOMNode) == 'DataItem')
    end function xdmf_element_node_is_dataitem


    function xdmf_element_node_is_time(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Time
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                  !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode               !< FoX DOM Node 
        logical                              :: xdmf_element_node_is_time  !< True if DOM Node is a XDMF Time
    !----------------------------------------------------------------- 
        xdmf_element_node_is_time = (getTagName(DOMNode) == 'Time')
    end function xdmf_element_node_is_time


    function xdmf_element_node_is_information(this, DOMNode)
    !-----------------------------------------------------------------
    !< Check if a DOM Node is a Information
    !----------------------------------------------------------------- 
        class(xdmf_element_t), intent(INOUT) :: this                        !< XDMF Element type
        type(Node), pointer,   intent(IN)    :: DOMNode                     !< FoX DOM Node 
        logical                              :: xdmf_element_node_is_information !< True if DOM Node is a XDMF Information
    !----------------------------------------------------------------- 
        xdmf_element_node_is_information = (getTagName(DOMNode) == 'Information')
    end function xdmf_element_node_is_information

end module xdmf_element
