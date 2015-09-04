module xh5for_parameters
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XH5For: XDMF parallel partitioned mesh I/O on top of HDF5
!< XH5For global parameters
!< @Note: Taked from Xdmf.f of the official XDMF API (http://www.xdmf.org)
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P

implicit none 

    integer(i4P), parameter (XDMF_ARRAY_TYPE_INT8    = 0)
    integer(i4P), parameter (XDMF_ARRAY_TYPE_INT16   = 1)
    integer(i4P), parameter (XDMF_ARRAY_TYPE_INT32   = 2)
    integer(i4P), parameter (XDMF_ARRAY_TYPE_INT64   = 3)
    integer(i4P), parameter (XDMF_ARRAY_TYPE_UINT8   = 4)
    integer(i4P), parameter (XDMF_ARRAY_TYPE_UINT16  = 5)
    integer(i4P), parameter (XDMF_ARRAY_TYPE_UINT32  = 6)
    integer(i4P), parameter (XDMF_ARRAY_TYPE_FLOAT32 = 7)
    integer(i4P), parameter (XDMF_ARRAY_TYPE_FLOAT64 = 8)

    integer(i4P), parameter (XDMF_ATTRIBUTE_CENTER_GRID = 100)
    integer(i4P), parameter (XDMF_ATTRIBUTE_CENTER_CELL = 101)
    integer(i4P), parameter (XDMF_ATTRIBUTE_CENTER_FACE = 102)
    integer(i4P), parameter (XDMF_ATTRIBUTE_CENTER_EDGE = 103)
    integer(i4P), parameter (XDMF_ATTRIBUTE_CENTER_NODE = 104)

    integer(i4P), parameter (XDMF_ATTRIBUTE_TYPE_SCALAR   = 200)
    integer(i4P), parameter (XDMF_ATTRIBUTE_TYPE_VECTOR   = 201)
    integer(i4P), parameter (XDMF_ATTRIBUTE_TYPE_TENSOR   = 202)
    integer(i4P), parameter (XDMF_ATTRIBUTE_TYPE_MATRIX   = 203)
    integer(i4P), parameter (XDMF_ATTRIBUTE_TYPE_TENSOR6  = 204)
    integer(i4P), parameter (XDMF_ATTRIBUTE_TYPE_GLOBALID = 205)
    integer(i4P), parameter (XDMF_ATTRIBUTE_TYPE_NOTYPE   = 206)

    integer(i4P), parameter (XDMF_GEOMETRY_TYPE_XYZ  = 301)
    integer(i4P), parameter (XDMF_GEOMETRY_TYPE_XY   = 302)

    integer(i4P), parameter (XDMF_GRID_COLLECTION_TYPE_SPATIAL  = 400)
    integer(i4P), parameter (XDMF_GRID_COLLECTION_TYPE_TEMPORAL = 401)

    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_POLYVERTEX       = 500)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_POLYLINE         = 501)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_POLYGON          = 502)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_TRIANGLE         = 503)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_QUADRILATERAL    = 504)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_TETRAHEDRON      = 505)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_PYRAMID          = 506)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_WEDGE            = 507)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON       = 508)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_EDGE_3           = 509)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_TRIANGLE_6       = 510)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_QUADRILATERAL_8  = 511)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_QUADRILATERAL_9  = 512)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_TETRAHEDRON_10   = 513)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_PYRAMID_13       = 514)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_WEDGE_15         = 515)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_WEDGE_18         = 516)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_20    = 517)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_24    = 518)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_27    = 519)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_64    = 520)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_125   = 521)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_216   = 522)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_343   = 523)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_512   = 524)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_729   = 525)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1000  = 526)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1331  = 527)
    integer(i4P), parameter (XDMF_TOPOLOGY_TYPE_MIXED            = 528)

    integer(i4P), parameter (XDMF_SET_TYPE_NODE = 601)
    integer(i4P), parameter (XDMF_SET_TYPE_CELL = 602)
    integer(i4P), parameter (XDMF_SET_TYPE_FACE = 603)
    integer(i4P), parameter (XDMF_SET_TYPE_EDGE = 604)

    integer(i4P), parameter (XDMF_GRID_TYPE_CURVILINEAR   = 701)
    integer(i4P), parameter (XDMF_GRID_TYPE_RECTILINEAR   = 702)
    integer(i4P), parameter (XDMF_GRID_TYPE_REGULAR       = 703)
    integer(i4P), parameter (XDMF_GRID_TYPE_UNSTRUCTURED  = 704)

end module xh5for_parameters
