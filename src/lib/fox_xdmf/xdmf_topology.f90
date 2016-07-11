module xdmf_topology
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF topology handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str, cton
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use FoX_dom,      only: Node, getTagName, hasAttribute, getAttribute
use xdmf_element, only: xdmf_element_t
use xdmf_utils

implicit none
private
!---------------------------------------------------------------------
! XDMFTopology properties (* Default):
! Name               (no default)
! TopologyType       Polyvertex | Polyline | Polygon |
!                    Triangle | Quadrilateral | Tetrahedron | Pyramid| Wedge | Hexahedron |
!                    Edge_3 | Triagle_6 | Quadrilateral_8 | Tetrahedron_10 | Pyramid_13 |
!                    Wedge_15 | Hexahedron_20 |
!                    Mixed |
!                    2DSMesh | 2DRectMesh | 2DCoRectMesh |
!                    3DSMesh | 3DRectMesh | 3DCoRectMesh
! NodesPerElement    (no default) Only Important for Polyvertex, Polygon and Polyline
! NumberOfElement    (no default)
!     OR
! Dimensions         (no default)
! Order              each cell type has its own default
! BaseOffset         *0 | #
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_topology_t
    !-----------------------------------------------------------------
    !< XDMF Topology type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: Name
        character(len=:), allocatable :: TopologyType
        integer(I4P)                  :: NodesPerElement
        integer(I8P), allocatable     :: Dimensions(:)
        integer(I4P)                  :: Order
        integer(I4P)                  :: BaseOffset
    contains
    private
        procedure         :: xdmf_topology_open_no_dimensions
        procedure         :: xdmf_topology_open_I4P_dimension
        procedure         :: xdmf_topology_open_I8P_dimension
        procedure         :: xdmf_topology_open_I4P_dimensions
        procedure         :: xdmf_topology_open_I8P_dimensions
        procedure         :: default_initialization => xdmf_topology_default_initialization
        procedure, public :: free                   => xdmf_topology_free
        generic,   public :: open                   => xdmf_topology_open_no_dimensions,  &
                                                       xdmf_topology_open_I4P_dimension,  &
                                                       xdmf_topology_open_I8P_dimension,  &
                                                       xdmf_topology_open_I4P_dimensions, &
                                                       xdmf_topology_open_I8P_dimensions
        procedure, public :: close                  => xdmf_topology_close
        procedure, public :: parse                  => xdmf_topology_parse
        procedure, public :: print                  => xdmf_topology_print
        procedure, public :: get_Name               => xdmf_topology_get_Name
        procedure, public :: get_TopologyType       => xdmf_topology_get_TopologyType
        procedure, public :: get_NodesPerElement    => xdmf_topology_get_NodesPerElement
        procedure, public :: get_Dimensions         => xdmf_topology_get_Dimensions
        procedure, public :: get_Order              => xdmf_topology_get_Order
        procedure, public :: get_BaseOffset         => xdmf_topology_get_BaseOffset
    end type xdmf_topology_t

public :: xdmf_topology_t

contains


    function xdmf_topology_get_Name(this)
    !-----------------------------------------------------------------
    !< Return the Topology Name
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(IN) :: this                    !< XDMF Topology type
        character(len=:), allocatable :: xdmf_topology_get_name       !< Topology Name
    !----------------------------------------------------------------- 
        xdmf_topology_get_name = this%Name
    end function xdmf_topology_get_Name


    function xdmf_topology_get_TopologyType(this)
    !-----------------------------------------------------------------
    !< Return the Topology TopologyType
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(IN) :: this                      !< XDMF Topology type
        character(len=:), allocatable :: xdmf_topology_get_TopologyType !< Topology TopologyType
    !----------------------------------------------------------------- 
        xdmf_topology_get_TopologyType = this%TopologyType
    end function xdmf_topology_get_TopologyType


    function xdmf_topology_get_NodesPerElement(this)
    !-----------------------------------------------------------------
    !< Return the Topology NodesPerElement
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(IN) :: this                    !< XDMF Topology type
        integer(I4P) :: xdmf_topology_get_NodesPerElement             !< Topology NodePerElement
    !----------------------------------------------------------------- 
        xdmf_topology_get_NodesPerElement = this%NodesPerElement
    end function xdmf_topology_get_NodesPerElement


    function xdmf_topology_get_Dimensions(this)
    !-----------------------------------------------------------------
    !< Return the Topology Dimensions
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(IN) :: this                    !< XDMF Topology type
        integer(I4P), allocatable :: xdmf_topology_get_Dimensions(:)  !< Topology Dimensions
    !----------------------------------------------------------------- 
        allocate(xdmf_topology_get_Dimensions(size(this%Dimensions, dim=1)))
        xdmf_topology_get_Dimensions = this%Dimensions
    end function xdmf_topology_get_Dimensions


    function xdmf_topology_get_Order(this)
    !-----------------------------------------------------------------
    !< Return the Topology Order
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(IN) :: this                    !< XDMF Topology type
        integer(I4P) :: xdmf_topology_get_Order                       !< Topology Order
    !----------------------------------------------------------------- 
        xdmf_topology_get_Order = this%Order
    end function xdmf_topology_get_Order


    function xdmf_topology_get_BaseOffset(this)
    !-----------------------------------------------------------------
    !< Return the Topology BaseOffset
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(IN) :: this                    !< XDMF Topology type
        integer(I4P) :: xdmf_topology_get_BaseOffset                  !< Topology BaseOffset
    !----------------------------------------------------------------- 
        xdmf_topology_get_baseOffset = this%BaseOffset
    end function xdmf_topology_get_BaseOffset


    subroutine xdmf_topology_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF Topology type
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(INOUT) :: this                 !< XDMF topology type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))         deallocate(this%Name)
        if(allocated(this%TopologyType)) deallocate(this%TopologyType)
        if(allocated(this%Dimensions))   deallocate(This%Dimensions)
        this%NodesPerElement = 0
        this%Order = 0
        this%BaseOffset = 0
    end subroutine xdmf_topology_free


    subroutine xdmf_topology_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF Topology with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(INOUT) :: this                 !< XDMF topology type
    !----------------------------------------------------------------- 
        call this%free()
    end subroutine xdmf_topology_default_initialization


    subroutine xdmf_topology_open_no_dimensions(this, xml_handler, Name, TopologyType, NodesPerElement, Order, BaseOffset)
    !-----------------------------------------------------------------
    !< Open a new topology XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t),     intent(INOUT) :: this             !< XDMF Topology type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Topology Name attribute
        character(len=*), optional, intent(IN)    :: TopologyType     !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: NodesPerElement  !< XDMF Topology NodesPerElement attribute
        integer(i4P)    , optional, intent(IN)    :: Order            !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: BaseOffset       !< XDMF Topology TopologyType attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
        integer(I4P)                              :: i                !< Aux index variable
    !----------------------------------------------------------------- 
        call this%set_tag('Topology')

        call xml_NewElement(xml_handler, 'Topology')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(TopologyType)) then; if(isSupportedTopologyTypeName(TopologyType)) &
            call xml_AddAttribute(xml_handler, name="TopologyType", value=TopologyType)
        endif

        if(PRESENT(NodesPerElement)) &
            call xml_AddAttribute(xml_handler, name="NodesPerElement", value=NodesPerElement)

        if(PRESENT(Order)) &
            call xml_AddAttribute(xml_handler, name="Order", value=Order)

        if(PRESENT(BaseOffset)) &
            call xml_AddAttribute(xml_handler, name="BaseOffset", value=BaseOffset)
    end subroutine xdmf_topology_open_no_dimensions


    subroutine xdmf_topology_open_I4P_dimension(this, xml_handler, Dimensions, Name, TopologyType, NodesPerElement, Order, BaseOffset)
    !-----------------------------------------------------------------
    !< Open a new topology XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t),     intent(INOUT) :: this             !< XDMF Topology type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        integer(i4P)    ,           intent(IN)    :: Dimensions       !< XDMF Topology I4P Dimensions attribute
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Topology Name attribute
        character(len=*), optional, intent(IN)    :: TopologyType     !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: NodesPerElement  !< XDMF Topology NodesPerElement attribute
        integer(i4P)    , optional, intent(IN)    :: Order            !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: BaseOffset       !< XDMF Topology TopologyType attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
        integer(I4P)                              :: i                !< Aux index variable
    !----------------------------------------------------------------- 
        call this%set_tag('Topology')

        call xml_NewElement(xml_handler, 'Topology')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(TopologyType)) then; if(isSupportedTopologyTypeName(TopologyType)) &
            call xml_AddAttribute(xml_handler, name="TopologyType", value=TopologyType)
        endif

        if(PRESENT(NodesPerElement)) &
            call xml_AddAttribute(xml_handler, name="NodesPerElement", value=NodesPerElement)

        char_dims = trim(adjustl(str(no_sign=.true., n=Dimensions)))
        call xml_AddAttribute(xml_handler, name="Dimensions", value=trim(char_dims) )
        deallocate(char_dims)

        if(PRESENT(Order)) &
            call xml_AddAttribute(xml_handler, name="Order", value=Order)

        if(PRESENT(BaseOffset)) &
            call xml_AddAttribute(xml_handler, name="BaseOffset", value=BaseOffset)
    end subroutine xdmf_topology_open_I4P_dimension


    subroutine xdmf_topology_open_I8P_dimension(this, xml_handler, Dimensions, Name, TopologyType, NodesPerElement, Order, BaseOffset)
    !-----------------------------------------------------------------
    !< Open a new topology XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t),     intent(INOUT) :: this             !< XDMF Topology type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        integer(i8P)    ,           intent(IN)    :: Dimensions       !< XDMF Topology I8P Dimensions attribute
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Topology Name attribute
        character(len=*), optional, intent(IN)    :: TopologyType     !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: NodesPerElement  !< XDMF Topology NodesPerElement attribute
        integer(i4P)    , optional, intent(IN)    :: Order            !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: BaseOffset       !< XDMF Topology TopologyType attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
        integer(I4P)                              :: i                !< Aux index variable
    !----------------------------------------------------------------- 
        call this%set_tag('Topology')

        call xml_NewElement(xml_handler, 'Topology')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(TopologyType)) then; if(isSupportedTopologyTypeName(TopologyType)) &
            call xml_AddAttribute(xml_handler, name="TopologyType", value=TopologyType)
        endif

        if(PRESENT(NodesPerElement)) &
            call xml_AddAttribute(xml_handler, name="NodesPerElement", value=NodesPerElement)

        char_dims = trim(adjustl(str(no_sign=.true., n=Dimensions)))
        call xml_AddAttribute(xml_handler, name="Dimensions", value=trim(char_dims) )
        deallocate(char_dims)

        if(PRESENT(Order)) &
            call xml_AddAttribute(xml_handler, name="Order", value=Order)

        if(PRESENT(BaseOffset)) &
            call xml_AddAttribute(xml_handler, name="BaseOffset", value=BaseOffset)
    end subroutine xdmf_topology_open_I8P_dimension



    subroutine xdmf_topology_open_I4P_dimensions(this, xml_handler, Dimensions, Name, TopologyType, NodesPerElement, Order, BaseOffset)
    !-----------------------------------------------------------------
    !< Open a new topology XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t),     intent(INOUT) :: this             !< XDMF Topology type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        integer(i4P)    ,           intent(IN)    :: Dimensions(:)    !< XDMF Topology I4P array Dimensions attribute
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Topology Name attribute
        character(len=*), optional, intent(IN)    :: TopologyType     !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: NodesPerElement  !< XDMF Topology NodesPerElement attribute
        integer(i4P)    , optional, intent(IN)    :: Order            !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: BaseOffset       !< XDMF Topology TopologyType attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
        integer(I4P)                              :: i                !< Aux index variable
    !----------------------------------------------------------------- 
        call this%set_tag('Topology')

        call xml_NewElement(xml_handler, 'Topology')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(TopologyType)) then; if(isSupportedTopologyTypeName(TopologyType)) &
            call xml_AddAttribute(xml_handler, name="TopologyType", value=TopologyType)
        endif

        if(PRESENT(NodesPerElement)) &
            call xml_AddAttribute(xml_handler, name="NodesPerElement", value=NodesPerElement)

        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Dimensions,dim=1)
        allocate(character(len=64*i) :: char_dims)
        write(char_dims, fmt=*)   (trim(adjustl(str(no_sign=.true., n=Dimensions(i))))//' ',i=1, size(Dimensions,dim=1) )
        call xml_AddAttribute(xml_handler, name="Dimensions", value=trim(char_dims) )
        deallocate(char_dims)

        if(PRESENT(Order)) &
            call xml_AddAttribute(xml_handler, name="Order", value=Order)

        if(PRESENT(BaseOffset)) &
            call xml_AddAttribute(xml_handler, name="BaseOffset", value=BaseOffset)
    end subroutine xdmf_topology_open_I4P_dimensions


    subroutine xdmf_topology_open_I8P_dimensions(this, xml_handler, Dimensions, Name, TopologyType, NodesPerElement, Order, BaseOffset)
    !-----------------------------------------------------------------
    !< Open a new topology XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t),     intent(INOUT) :: this             !< XDMF Topology type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        integer(i8P)    ,           intent(IN)    :: Dimensions(:)    !< XDMF Topology I8P array Dimensions attribute
        character(len=*), optional, intent(IN)    :: Name             !< XDMF Topology Name attribute
        character(len=*), optional, intent(IN)    :: TopologyType     !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: NodesPerElement  !< XDMF Topology NodesPerElement attribute
        integer(i4P)    , optional, intent(IN)    :: Order            !< XDMF Topology TopologyType attribute
        integer(i4P)    , optional, intent(IN)    :: BaseOffset       !< XDMF Topology TopologyType attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
        integer(I4P)                              :: i                !< Aux index variable
    !----------------------------------------------------------------- 
        call this%set_tag('Topology')

        call xml_NewElement(xml_handler, 'Topology')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)

        if(PRESENT(TopologyType)) then; if(isSupportedTopologyTypeName(TopologyType)) &
            call xml_AddAttribute(xml_handler, name="TopologyType", value=TopologyType)
        endif

        if(PRESENT(NodesPerElement)) &
            call xml_AddAttribute(xml_handler, name="NodesPerElement", value=NodesPerElement)

        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Dimensions,dim=1)
        allocate(character(len=64*i) :: char_dims)
        write(char_dims, fmt=*)   (trim(adjustl(str(no_sign=.true., n=Dimensions(i))))//' ',i=1, size(Dimensions,dim=1) )
        call xml_AddAttribute(xml_handler, name="Dimensions", value=trim(char_dims) )
        deallocate(char_dims)

        if(PRESENT(Order)) &
            call xml_AddAttribute(xml_handler, name="Order", value=Order)

        if(PRESENT(BaseOffset)) &
            call xml_AddAttribute(xml_handler, name="BaseOffset", value=BaseOffset)
    end subroutine xdmf_topology_open_I8P_dimensions


    subroutine xdmf_topology_parse(this, DOMNode)
    !-----------------------------------------------------------------
    !< Parse a DOM topology into a XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t),  intent(INOUT) :: this                !< XDMF Topology type
        type(Node),    pointer,  intent(IN)    :: DOMNode             !< FoX DOM Node containig a Topology element
        character(len=:), allocatable          :: Name                !< XDMF Topology Name attribute
        character(len=:), allocatable          :: TopologyType        !< XDMF Topology TopologyType attribute
        integer(I4P)                           :: NodesPerElement     !< XDMF Topology NodesPerElement attribute
        integer(I8P),     allocatable          :: Dimensions(:)       !< XDMF Topology Dimensions attribute
        integer(I4P)                           :: Order               !< XDMF Topology Order attribute
        integer(I4P)                           :: BaseOffset          !< XDMF Topology BaseOffset attribute
        character(len=:), allocatable          :: AuxDims             !< Aux dimensions string
        integer(I4P)                           :: NumTokens           !< Number of tokens in a string
        integer(I4P)                           :: i                   !< Loop index in NumTokens
        integer(I4P)                           :: pos                 !< Start position of next token
    !----------------------------------------------------------------- 
        call this%default_initialization()

        if(this%node_is_topology(DOMNode)) then
            if(hasAttribute(DOMNode, 'Name')) then
                this%Name = getAttribute(DOMNode, 'Name')
            endif

            if(hasAttribute(DOMNode, 'TopologyType')) then
                TopologyType = getAttribute(DOMNode, 'TopologyType')
                if(isSupportedTopologyTypeName(TopologyType)) this%TopologyType = TopologyType
            endif

            if(hasAttribute(DOMNode, 'NodesPerElement')) then
                this%NodesPerElement = cton(str=getAttribute(DOMNode, 'NodesPerElement'),knd=1_I4P)
            endif

            if(hasAttribute(DOMNode, 'Dimensions')) then
                AuxDims = getAttribute(DOMNode, 'Dimensions')
                NumTokens = Count_tokens(AuxDims)
                allocate(this%Dimensions(NumTokens))
                pos = 1
                do i=1,NumTokens
                    this%Dimensions(i) = cton(str=Next_token(AuxDims,pos), knd=1_I8P)
                enddo
            endif

            if(hasAttribute(DOMNode, 'Order')) then
                this%Order = cton(str=getAttribute(DOMNode, 'Order'),knd=1_I4P)
            endif

            if(hasAttribute(DOMNode, 'BaseOffset')) then
                this%BaseOffset = cton(str=getAttribute(DOMNode, 'BaseOffset'),knd=1_I8P)
            endif

        endif
    end subroutine xdmf_topology_parse


    subroutine xdmf_topology_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new topology XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t), intent(IN)    :: this                 !< XDMF topology type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, 'Topology')
    end subroutine xdmf_topology_close


    subroutine xdmf_topology_print(this, IndentationLevel)
    !-----------------------------------------------------------------
    !< Print on screen the Topology XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_topology_t),  intent(IN)    :: this                !< XDMF Topology type
        integer(I4P), optional,  intent(IN)    :: IndentationLevel    !< Indentation level
        integer(I4P)                           :: indlev = 0          !< Aux Indentation level
    !-----------------------------------------------------------------
        if(present(IndentationLevel)) indlev = IndentationLevel
        print*, repeat('  ',indlev)//'-------------------------------------------'
        print*, repeat('  ',indlev)//'TOPOLOGY:'
        print*, repeat('  ',indlev)//'-------------------------------------------'
        if(allocated(this%Name)) print*, repeat('  ',indlev)//'Name: '//this%Name
        if(allocated(this%TopologyType)) print*, repeat('  ',indlev)//'TopologyType: '//this%TopologyType
        if(allocated(this%Dimensions)) print*, repeat('  ',indlev)//'Dimensions: '//str(no_sign=.true., n=this%Dimensions)
        print*, repeat('  ',indlev)//'NodesPerElement: '//str(no_sign=.true.,n=this%NodesPerElement)
!        print*, repeat('  ',indlev)//'Order: '//str(no_sign=.true.,n=this%Order)
!        print*, repeat('  ',indlev)//'BaseOffset: '//str(no_sign=.true.,n=this%BaseOffset)
    end subroutine xdmf_topology_print


end module xdmf_topology
