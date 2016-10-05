module xh5for_utils
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XH5For: XDMF parallel partitioned mesh I/O on top of HDF5
!< XH5For utilities
!--------------------------------------------------------------------- -----------------------------------------------------------
use PENF,       only: I4P, R8P, str
use xdmf_utils, only : warning_message
use xh5for_parameters

#ifdef MPI_MOD
    use mpi
#endif
implicit none 
#ifdef MPI_H
include 'mpif.h'
#endif	
private

public :: Abort
public :: Wtime
public :: GetNumberOfNodesPerElement
public :: GetXDMFTopologyTypeName
public :: GetXDMFTopologyTypeFromName
public :: GetXDMFGeometryTypeName
public :: GetXDMFGeometryTypeFromName
public :: GetSpaceDimension
public :: GetXDMFCenterTypeName
public :: GetXDMFCenterTypeFromName
public :: GetXDMFAttributeTypeName
public :: GetNumberOfComponentsFromAttributeType
public :: isSupportedStrategy
public :: isSupportedGridType
public :: isSupportedTopologyType
public :: isSupportedGeometryType

contains

    subroutine Abort()
        integer(I4P) :: code, info, ierror
        logical(I4P) :: initialized_mpi, finalized_mpi

        code = -1
        initialized_mpi = .false.
        finalized_mpi   = .false.
#ifdef __GFORTRAN__
        call backtrace()
#endif
#ifdef ENABLE_MPI
        call mpi_initialized(initialized_mpi, ierror)
        call mpi_finalized(finalized_mpi, ierror)
        if(initialized_mpi .and. .not. finalized_mpi) then
            call mpi_abort(mpi_comm_world,code,info)
        else
#else
        if(.true.) then
#endif
            stop -1
        endif
    end subroutine Abort


    !  Discussion:
    !    To get the elapsed wall clock time, call WTIME before and after a given
    !    operation, and subtract the first reading from the second.
    !    This function is meant to suggest the similar routines:
    !      "omp_get_wtime ( )" in OpenMP,
    !      "MPI_Wtime ( )" in MPI,
    !      and "tic" and "toc" in MATLAB.
    !  Licensing:
    !    This code is distributed under the GNU LGPL license.
    !  Modified:
    !    27 April 2009
    !  Author:
    !    John Burkardt
    !  Parameters:
    !    Output, real ( kind = 8 ) WTIME, the wall clock reading, in seconds.
    function Wtime() result(time)
        real(R8P)    :: time
        integer(I4P) :: clock_max
        integer(I4P) :: clock_rate
        integer(I4P) :: clock_reading

        call system_clock(clock_reading,clock_rate,clock_max)
        time = real(clock_reading,kind=R8P)/real(clock_rate,kind=R8P)
        return
    end function Wtime

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
        integer(I4P),     intent(IN)  :: TopologyType
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
                topologyName = 'Mixed'; return
            case (XDMF_TOPOLOGY_TYPE_2DSMESH)
                topologyName = '2DSMesh'; return
            case (XDMF_TOPOLOGY_TYPE_3DSMESH)
                topologyName = '3DSMesh'; return
            case (XDMF_TOPOLOGY_TYPE_2DRECTMESH)
                topologyName = '2DRectMesh'; return
            case (XDMF_TOPOLOGY_TYPE_3DRECTMESH)
                topologyName = '3DRectMesh'; return
            case (XDMF_TOPOLOGY_TYPE_2DCORECTMESH)
                topologyName = '2DCoRectMesh'; return
            case (XDMF_TOPOLOGY_TYPE_3DCORECTMESH)
                topologyName = '3DCoRectMesh'; return
            case DEFAULT
                topologyName = 'Mixed'; return
        end select
    end function GetXDMFTopologyTypeName


    function GetXDMFTopologyTypeFromName(TopologyNAme) result(topologyType)
        character(len=*), intent(IN) :: TopologyName
        integer(I4P)                 :: topologyType
!< @Note: How we can manage 2DSMesh, 2DRectMesh, 2DCoRectMesh, 3DSMesh, 3DRectMesh and 3DCoRectMesh TopologyTypes
!        allowed_topologyTypes = 'Polyvertex&Polyline&Polygon&Triangle&Quadrilateral' // &
!                            '&Tetrahedron&Pyramid&Wedge&Hexahedron&Edge_3&Triangle_6'// &
!                            '&Quadrilateral_8&Tetrahedron_10&Pyramid_13&Wedge_15'    // &
!                            '&Hexahedron_20&Mixed&2DSMesh&2DRectMesh&2DCoRectMesh'   // &
!                            '&3DSMesh&3DRectMesh&3DCoRectMesh'

        select case(TopologyName)
            case ('Polyvertex')
                topologyType = XDMF_TOPOLOGY_TYPE_POLYVERTEX; return
            case ('Polyline')
                topologyType = XDMF_TOPOLOGY_TYPE_POLYLINE; return
            case ('Polygon')
                topologyType = XDMF_TOPOLOGY_TYPE_POLYGON; return
            case ('Triangle')
                topologyType = XDMF_TOPOLOGY_TYPE_TRIANGLE; return
            case ('Quadrilateral')
                topologyType = XDMF_TOPOLOGY_TYPE_QUADRILATERAL; return
            case ('Tetrahedron')
                topologyType = XDMF_TOPOLOGY_TYPE_TETRAHEDRON; return
            case ('Pyramid')
                topologyType = XDMF_TOPOLOGY_TYPE_PYRAMID; return
            case ('Wedge')
                topologyType = XDMF_TOPOLOGY_TYPE_WEDGE; return
            case ('Hexahedron')
                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON; return
            case ('Edge_3')
                topologyType = XDMF_TOPOLOGY_TYPE_EDGE_3; return
            case ('Triangle_6')
                topologyType = XDMF_TOPOLOGY_TYPE_TRIANGLE_6; return
            case ('Quadrilateral_8')
                topologyType = XDMF_TOPOLOGY_TYPE_QUADRILATERAL_8; return
            case ('Quadrilateral_9')
                topologyType = XDMF_TOPOLOGY_TYPE_QUADRILATERAL_9; return
            case ('Tetrahedron_10')
                topologyType = XDMF_TOPOLOGY_TYPE_TETRAHEDRON_10; return
            case ('Pyramid_13')
                topologyType = XDMF_TOPOLOGY_TYPE_PYRAMID_13; return
            case ('Wedge_15')
                topologyType = XDMF_TOPOLOGY_TYPE_WEDGE_15; return
!            case ('Wedge_18')
!                topologyType = XDMF_TOPOLOGY_TYPE_WEDGE_18; return
!            case ('Hexahedron_20')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_20; return
!            case ('Hexahedron_24')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_24; return
!            case ('Hexahedron_27')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_27; return
!            case ('Hexahedron_64')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_64; return
!            case ('Hexahedron_125')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_125; return
!            case ('Hexahedron_216')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_216; return
!            case ('Hexahedron_343')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_343; return
!            case ('Hexahedron_512')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_512; return
!            case ('Hexahedron_727')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_727; return
!            case ('Hexahedron_1000')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1000; return
!            case ('Hexahedron_1331')
!                topologyType = XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1331; return
            case ('Mixed')
                topologyType = XDMF_TOPOLOGY_TYPE_MIXED; return
            case DEFAULT
                topologyType = XDMF_TOPOLOGY_TYPE_TRIANGLE; return
        end select
    end function GetXDMFTopologyTypeFromName


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
            case (XDMF_GEOMETRY_TYPE_X_Y_Z)
                GeometryName = 'X_Y_Z'; return
            case (XDMF_GEOMETRY_TYPE_VXVYVZ)
                GeometryName = 'VxVyVz'; return
            case (XDMF_GEOMETRY_TYPE_VXVY)
                GeometryName = 'VxVy'; return
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                GeometryName = 'Origin_DxDyDz'; return
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                GeometryName = 'Origin_DxDy'; return
            case DEFAULT
                GeometryName='XYZ'; return
        end select

    end function GetXDMFGeometryTypeName


    function GetXDMFGeometryTypeFromName(GeometryName) result(GeometryType)
        character(len=*), intent(IN) :: GeometryName
        integer(I4P)                 :: GeometryType

!< @Note: How we can manage X_Y_Z, VxVyVz, Origin_DxDyDz and Origin_DxDy GeometryTypes
!        allowed_GeometryTypes = 'XYZ&XY&X_Y_Z&VxVyVz&Origin_DxDyDz%Origin_DxDy'
        select case(GeometryName)
            case ('XYZ')
                GeometryType = XDMF_GEOMETRY_TYPE_XYZ; return
            case ('XY')
                GeometryType = XDMF_GEOMETRY_TYPE_XY; return
            case ('X_Y_Z')
                GeometryType = XDMF_GEOMETRY_TYPE_X_Y_Z; return
            case ('VxVy')
                GeometryType = XDMF_GEOMETRY_TYPE_VXVY; return
            case ('VxVyVz')
                GeometryType = XDMF_GEOMETRY_TYPE_VXVYVZ; return
            case ('Origin_DxDyDz')
                GeometryType = XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ; return
            case ('Origin_DxDy')
                GeometryType = XDMF_GEOMETRY_TYPE_ORIGIN_DXDY; return
            case DEFAULT
                GeometryType = XDMF_GEOMETRY_TYPE_XYZ; return
        end select

    end function GetXDMFGeometryTypeFromName


    function GetSpaceDimension(GeometryType) result(SpaceDimension)
        integer(I4P), intent(IN) :: GeometryType
        integer(I4P)             :: SpaceDimension

        select case(GeometryType)
            case(XDMF_GEOMETRY_TYPE_XYZ)
                SpaceDimension = 3; return
            case(XDMF_GEOMETRY_TYPE_XY)
                SpaceDimension = 2; return
            case(XDMF_GEOMETRY_TYPE_X_Y_Z)
                SpaceDimension = 3; return
            case (XDMF_GEOMETRY_TYPE_VXVYVZ)
                SpaceDimension = 3; return
            case (XDMF_GEOMETRY_TYPE_VXVY)
                SpaceDimension = 2; return
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                SpaceDimension = 3; return
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                SpaceDimension = 2; return
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


    function GetXDMFCenterTypeFromName(CenterName) result(CenterType)
        character(len=*), intent(IN) :: CenterName
        integer(I4P)                 :: CenterType
!< @Note: How we can manage Grid, Face and Edge CenterTypes
!        allowed_Centers = 'Node&Cell&Grid&Face&Edge'
        select case(CenterName)
            case ('Node')
                CenterType = XDMF_ATTRIBUTE_CENTER_NODE; return
            case ('Cell')
                CenterType = XDMF_ATTRIBUTE_CENTER_CELL; return
            case ('Grid')
                CenterType = XDMF_ATTRIBUTE_CENTER_GRID; return
            case ('Face')
                CenterType = XDMF_ATTRIBUTE_CENTER_FACE; return
            case ('Edge')
                CenterType = XDMF_ATTRIBUTE_CENTER_EDGE; return
            case DEFAULT
                CenterType = XDMF_ATTRIBUTE_CENTER_NODE; return
        end select

    end function GetXDMFCenterTypeFromName


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
!                AttributeName = 'NoType'; return
            case DEFAULT
                AttributeName ='Scalar'; return
        end select
    end function GetXDMFAttributeTypeName


    function GetXDMFAttributeTypeFromName(AttributeName) result(AttributeType)
        character(len=*), intent(IN) :: AttributeName
        integer(I4P)                 :: AttributeType
!< @Note: How we can manage NOTYPE CenterTypes
!        allowed_AttributeTypes = 'Scalar&Vector&Tensor&Tensor6&Matrix&GlobalID'
        select case(AttributeName)
            case ('Scalar')
                AttributeType = XDMF_ATTRIBUTE_TYPE_SCALAR; return
            case ('Vector')
                AttributeType = XDMF_ATTRIBUTE_TYPE_VECTOR; return
            case ('Tensor')
                AttributeType = XDMF_ATTRIBUTE_TYPE_TENSOR; return
            case ('Tensor6')
                AttributeType = XDMF_ATTRIBUTE_TYPE_TENSOR6; return
            case ('Matrix')
                AttributeType = XDMF_ATTRIBUTE_TYPE_MATRIX; return
            case ('GlobalID')
                AttributeType = XDMF_ATTRIBUTE_TYPE_GLOBALID; return
!            case ('NoType')
!                AttributeType = XDMF_ATTRIBUTE_TYPE_NOTYPE; return
            case DEFAULT
                AttributeType = XDMF_ATTRIBUTE_TYPE_SCALAR; return
        end select
    end function GetXDMFAttributeTypeFromName


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


    function isSupportedStrategy(Strategy) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a supported Strategy
    !----------------------------------------------------------------- 
        integer(I4P),    intent(IN)  :: Strategy
        logical                      :: supported
    !----------------------------------------------------------------- 
        supported = MINVAL(ABS(SUPPORTED_STRATEGIES - Strategy)) == 0_I4P
        if(.not. supported) call warning_message('Not supported Strategy: "'//trim(str(no_sign=.true., n=Strategy))//'"')
    end function isSupportedStrategy


    function isSupportedGridType(GridType) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a supported GridType
    !----------------------------------------------------------------- 
        integer(I4P),    intent(IN)  :: GridType
        logical                      :: supported
    !----------------------------------------------------------------- 
        supported = MINVAL(ABS(SUPPORTED_GRIDTYPES - GridType)) == 0_I4P
        if(.not. supported) call warning_message('Not supported Grid Type: "'//trim(str(no_sign=.true., n=GridType))//'"')
    end function isSupportedGridType


    function isSupportedTopologyType(TopologyType) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a supported topology type
    !----------------------------------------------------------------- 
        integer(I4P),                      intent(IN) :: TopologyType  !< XDMF Topology Type
        logical                                       :: supported     !< Valid Topology Type confirmation flag
    !----------------------------------------------------------------- 
        supported = MINVAL(ABS(SUPPORTED_TOPOLOGYTYPES - TopologyType)) == 0_I4P
        if(.not. supported) call warning_message('Not supported Topology Type: "'//trim(str(no_sign=.true., n=TopologyType))//'"')
    end function isSupportedTopologyType


    function isSupportedGeometryType(GeometryType) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem NumberType
    !----------------------------------------------------------------- 
        integer(I4P),                      intent(IN) :: GeometryType  !< XDMF Geometry Type
        logical                                       :: supported     !< Valid Geometry Type confirmation flag
    !----------------------------------------------------------------- 
        supported = MINVAL(ABS(SUPPORTED_GEOMETRYTYPES - GeometryType)) == 0_I4P
        if(.not. supported) call warning_message('Not supported Geometry Type: "'//trim(str(no_sign=.true., n=GeometryType))//'"')
    end function isSupportedGeometryType


end module xh5for_utils
