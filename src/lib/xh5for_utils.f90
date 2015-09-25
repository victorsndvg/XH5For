module xh5for_utils
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XH5For: XDMF parallel partitioned mesh I/O on top of HDF5
!< XH5For utilities
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P
use xh5for_parameters

implicit none 

contains

    function GetNumberOfNodesPerElement(TopologyType) result(NodesPerElement)
        integer(I4P), intent(IN) :: TopologyType
        integer(I4P)             :: NodesPerElement

        select case(TopologyType)
!            case (XDMF_TOPOLOGY_TYPE_POLYVERTEX)
!                NodesPerElement = XDMF_NO_VALUE; return
!            case (XDMF_TOPOLOGY_TYPE_POLYLINE)
!                NodesPerElement = XDMF_NO_VALUE; return
!            case (XDMF_TOPOLOGY_TYPE_POLYGON)
!                NodesPerElement = XDMF_NO_VALUE; return
            case (XDMF_TOPOLOGY_TYPE_TRIANGLE)
                NodesPerElement = 3; return
            case (XDMF_TOPOLOGY_TYPE_QUADRILATERAL)
                NodesPerElement = 4; return
            case (XDMF_TOPOLOGY_TYPE_TETRAHEDRON)
                NodesPerElement = 4; return
            case (XDMF_TOPOLOGY_TYPE_PYRAMID)
                NodesPerElement = 5; return
            case (XDMF_TOPOLOGY_TYPE_WEDGE)
                NodesPerElement = 6; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON)
                NodesPerElement = 8; return
            case (XDMF_TOPOLOGY_TYPE_EDGE_3)
                NodesPerElement = 3; return
            case (XDMF_TOPOLOGY_TYPE_TRIANGLE_6)
                NodesPerElement = 6; return
            case (XDMF_TOPOLOGY_TYPE_QUADRILATERAL_8)
                NodesPerElement = 8; return
            case (XDMF_TOPOLOGY_TYPE_QUADRILATERAL_9)
                NodesPerElement = 9; return
            case (XDMF_TOPOLOGY_TYPE_TETRAHEDRON_10)
                NodesPerElement = 10; return
            case (XDMF_TOPOLOGY_TYPE_PYRAMID_13)
                NodesPerElement = 13; return
            case (XDMF_TOPOLOGY_TYPE_WEDGE_15)
                NodesPerElement = 15; return
            case (XDMF_TOPOLOGY_TYPE_WEDGE_18)
                NodesPerElement = 18; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_20)
                NodesPerElement = 20; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_24)
                NodesPerElement = 24; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_27)
                NodesPerElement = 27; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_64)
                NodesPerElement = 64; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_125)
                NodesPerElement = 125; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_216)
                NodesPerElement = 216; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_343)
                NodesPerElement = 343; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_512)
                NodesPerElement = 512; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_729)
                NodesPerElement = 729; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1000)
                NodesPerElement = 1000; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1331)
                NodesPerElement = 1331; return
!            case (XDMF_TOPOLOGY_TYPE_MIXED)
!                NodesPerElement = XDMF_NO_VALUE; return
            case DEFAULT
                NodesPerElement = XDMF_NO_VALUE; return
        end select
    end function

    function GetXDMFTopologyTypeName(TopologyType) result(topologyName)
        integer(I4P), intent(IN)     :: TopologyType
        character(len=:), allocatable :: topologyName
!< @Note: How we can manage 2DSMesh, 2DRectMesh, 2DCoRectMesh, 3DSMesh, 3DRectMesh and 3DCoRectMesh TopologyTypes
!        allowed_topologyTypes = 'Polyvertex&Polyline&Polygon&Triangle&Quadrilateral' // &
!                            '&Tetrahedron&Pyramid&Wedge&Hexahedron&Edge_3&Triangle_6'// &
!                            '&Quadrilateral_8&Tetrahedron_10&Pyramid_13&Wedge_15'    // &
!                            '&Hexahedron_20&Mixed&2DSMesh&2DRectMesh&2DCoRectMesh'   // &
!                            '&3DSMesh&3DRectMesh&3DCoRectMesh'

        select case(TopologyType)
            case (XDMF_TOPOLOGY_TYPE_POLYVERTEX)
                topologyName = 'Polyvertex'; return
            case (XDMF_TOPOLOGY_TYPE_POLYLINE)
                topologyName = 'Polyline'; return
            case (XDMF_TOPOLOGY_TYPE_POLYGON)
                topologyName = 'Polygon'; return
            case (XDMF_TOPOLOGY_TYPE_TRIANGLE)
                topologyName = 'Triangle'; return
            case (XDMF_TOPOLOGY_TYPE_QUADRILATERAL)
                topologyName = 'Quadrilateral'; return
            case (XDMF_TOPOLOGY_TYPE_TETRAHEDRON)
                topologyName = 'Tetrahedron'; return
            case (XDMF_TOPOLOGY_TYPE_PYRAMID)
                topologyName = 'Pyramid'; return
            case (XDMF_TOPOLOGY_TYPE_WEDGE)
                topologyName = 'Wedge'; return
            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON)
                topologyName = 'Hexahedron'; return
            case (XDMF_TOPOLOGY_TYPE_EDGE_3)
                topologyName = 'Edge_3'; return
            case (XDMF_TOPOLOGY_TYPE_TRIANGLE_6)
                topologyName = 'Triangle_6'; return
            case (XDMF_TOPOLOGY_TYPE_QUADRILATERAL_8)
                topologyName = 'Quadrilateral_8'; return
            case (XDMF_TOPOLOGY_TYPE_QUADRILATERAL_9)
                topologyName = 'Quadrilateral_9'; return
            case (XDMF_TOPOLOGY_TYPE_TETRAHEDRON_10)
                topologyName = 'Tetrahedron_10'; return
            case (XDMF_TOPOLOGY_TYPE_PYRAMID_13)
                topologyName = 'Pyramid_13'; return
            case (XDMF_TOPOLOGY_TYPE_WEDGE_15)
                topologyName = 'Wedge_15'; return
!            case (XDMF_TOPOLOGY_TYPE_WEDGE_18)
!                topologyName = 'Wedge_18'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_20)
!                topologyName = 'Hexahedron_20'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_24)
!                topologyName = 'Hexahedron_24'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_27)
!                topologyName = 'Hexahedron_27'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_64)
!                topologyName = 'Hexahedron_64'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_125)
!                topologyName = 'Hexahedron_125'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_216)
!                topologyName = 'Hexahedron_216'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_343)
!                topologyName = 'Hexahedron_343'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_512)
!                topologyName = 'Hexahedron_512'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_729)
!                topologyName = 'Hexahedron_727'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1000)
!                topologyName = 'Hexahedron_1000'; return
!            case (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1331)
!                topologyName = 'Hexahedron_1331'; return
            case (XDMF_TOPOLOGY_TYPE_MIXED)
                topologyName = 'mixed_27'; return
            case DEFAULT
                topologyName = 'Triangle'; return
        end select
    end function GetXDMFTopologyTypeName

    function GetXDMFGeometryTypeName(GeometryType) result(GeometryName)
        integer(I4P), intent(IN)     :: GeometryType
        character(len=:), allocatable :: GeometryName
!< @Note: How we can manage X_Y_Z, VxVyVz, Origin_DxDyDz and Origin_DxDy GeometryTypes
!        allowed_GeometryTypes = 'XYZ&XY&X_Y_Z&VxVyVz&Origin_DxDyDz%Origin_DxDy'
        select case(GeometryType)
            case (XDMF_GEOMETRY_TYPE_XYZ)
                GeometryName = 'XYZ'; return
            case (XDMF_GEOMETRY_TYPE_XY)
                GeometryName = 'XY'; return
!            case (XDMF_GEOMETRY_TYPE_X_Y_Z)
!                GeometryName = 'X_Y_Z'; return
!            case (XDMF_GEOMETRY_TYPE_VxVyVz)
!                GeometryName = 'VxVyVz'; return
!            case (XDMF_GEOMETRY_TYPE_Origin_DxDyDz)
!                GeometryName = 'Origin_DxDyDz'; return
!            case (XDMF_GEOMETRY_TYPE_Origin_DxDy)
!                GeometryName = 'Origin_DxDy'; return
            case DEFAULT
                GeometryName='XYZ'; return
        end select

    end function GetXDMFGeometryTypeName

    function GetSpaceDimension(GeometryType) result(SpaceDimension)
        integer(I4P), intent(IN) :: GeometryType
        integer(I4P)             :: SpaceDimension

        select case(GeometryType)
            case(XDMF_GEOMETRY_TYPE_XYZ)
                SpaceDimension = 3; return
            case(XDMF_GEOMETRY_TYPE_XY)
                SpaceDimension = 2; return
!            case(XDMF_GEOMETRY_TYPE_X_Y_Z)
!                SpaceDimension = 3; return
!            case (XDMF_GEOMETRY_TYPE_VxVyVz)
!                SpaceDimension = 3; return
!            case (XDMF_GEOMETRY_TYPE_Origin_DxDyDz)
!                SpaceDimension = 3; return
!            case (XDMF_GEOMETRY_TYPE_Origin_DxDy)
!                SpaceDimension = 2; return
            case DEFAULT
                SpaceDimension = 3; return
        end select
    end function GetSpaceDimension

    function GetXDMFCenterTypeName(CenterType) result(CenterName)
        integer(I4P), intent(IN)     :: CenterType
        character(len=:), allocatable :: CenterName
!< @Note: How we can manage Grid, Face and Edge CenterTypes
!        allowed_Centers = 'Node&Cell&Grid&Face&Edge'
        select case(CenterType)
            case (XDMF_ATTRIBUTE_CENTER_NODE)
                CenterName = 'Node'; return
            case (XDMF_ATTRIBUTE_CENTER_CELL)
                CenterName = 'Cell'; return
            case (XDMF_ATTRIBUTE_CENTER_GRID)
                CenterName = 'Grid'; return
            case (XDMF_ATTRIBUTE_CENTER_FACE)
                CenterName = 'Face'; return
            case (XDMF_ATTRIBUTE_CENTER_EDGE)
                CenterName = 'Edge'; return
            case DEFAULT
                CenterName ='Node'; return
        end select

    end function GetXDMFCenterTypeName


    function GetXDMFAttributeTypeName(AttributeType) result(AttributeName)
        integer(I4P), intent(IN)     :: AttributeType
        character(len=:), allocatable :: AttributeName
!< @Note: How we can manage NOTYPE CenterTypes
!        allowed_AttributeTypes = 'Scalar&Vector&Tensor&Tensor6&Matrix&GlobalID'
        select case(AttributeType)
            case (XDMF_ATTRIBUTE_TYPE_SCALAR)
                AttributeName = 'Scalar'; return
            case (XDMF_ATTRIBUTE_TYPE_VECTOR)
                AttributeName = 'Vector'; return
            case (XDMF_ATTRIBUTE_TYPE_TENSOR)
                AttributeName = 'Tensor'; return
            case (XDMF_ATTRIBUTE_TYPE_TENSOR6)
                AttributeName = 'Tensor6'; return
            case (XDMF_ATTRIBUTE_TYPE_MATRIX)
                AttributeName = 'Matrix'; return
            case (XDMF_ATTRIBUTE_TYPE_GLOBALID)
                AttributeName = 'GlobalID'; return
!            case (XDMF_ATTRIBUTE_TYPE_NOTYPE)
!                AttributeName = 'Edge'; return
            case DEFAULT
                AttributeName ='Scalar'; return
        end select
    end function GetXDMFAttributeTypeName


    function GetNumberOfComponentsFromAttributeType(AttributeType) result(NumberOfComponents)
        integer(I4P), intent(IN) :: attributeType
        integer(I4P)             :: NumberOfComponents

    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_SCALAR   = 200
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_VECTOR   = 201
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_TENSOR   = 202
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_MATRIX   = 203
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_TENSOR6  = 204
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_GLOBALID = 205
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_NOTYPE   = 206

        select case(AttributeType)
            case(XDMF_ATTRIBUTE_TYPE_SCALAR)
                NumberOfComponents = 1; return
            case(XDMF_ATTRIBUTE_TYPE_VECTOR)
                NumberOfComponents = 3; return
            case(XDMF_ATTRIBUTE_TYPE_TENSOR)
                NumberOfComponents = 9; return
!            case(XDMF_ATTRIBUTE_TYPE_MATRIX)
!                NumberOfComponents = 1; return
            case(XDMF_ATTRIBUTE_TYPE_TENSOR6)
                NumberOfComponents = 6; return
!            case(XDMF_ATTRIBUTE_TYPE_GLOBALID)
!                NumberOfComponents = 1; return
!            case(XDMF_ATTRIBUTE_TYPE_NOTYPE)
!                NumberOfComponents = 1; return
            case DEFAULT
                NumberOfComponents = 1; return
        end select
    end function GetNumberOfComponentsFromAttributeType


end module xh5for_utils
