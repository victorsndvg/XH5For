# XH5For
The Fortran way to easy write parallel partitioned XDMF/HDF5 meshes

[![Build Status](https://travis-ci.org/victorsndvg/XH5For.svg?branch=master)](https://travis-ci.org/victorsndvg/XH5For)
[![codecov](https://codecov.io/gh/victorsndvg/XH5For/branch/master/graph/badge.svg)](https://codecov.io/gh/victorsndvg/XH5For)

## License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)

## XH5For Basic usage

### Unstructured grid

Write:

```fortran
    !< Write XDMF/HDF5 file
    call xh5%Open(FilePrefix='xh5for_unstructured_hexahedron', Action=XDMF_ACTION_WRITE)
    call xh5%SetGrid(NumberOfNodes=8, NumberOfElements=1,TopologyType=XDMF_TOPOLOGY_TYPE_HEXAHEDRON, GeometryType=XDMF_GEOMETRY_TYPE_XYZ)
    call xh5%WriteTopology(Connectivities=topology)
    call xh5%WriteGeometry(XYZ=geometry)
    call xh5%WriteAttribute(Name='Temperature', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=temperature)
    call xh5%Close()
    call xh5%Free()
```

Read:

```fortran
    !< Read XDMF/HDF5 file
    call xh5%Open(FilePrefix='fileprefix', Action=XDMF_ACTION_READ)
    call xh5%ParseGrid()
    call xh5%ReadTopology(Connectivities=out_topology)
    call xh5%ReadGeometry(XYZ=out_geometry)
    call xh5%ReadAttribute(Name='Temperature', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=out_temperature)
    call xh5%Close()
    call xh5%Free()
```

### Structured grid

Write:

```fortran
    !< Write XDMF/HDF5 file
    call xh5%Open(FilePrefix='fileprefix', GridType=XDMF_GRID_TYPE_RECTILINEAR, Action=XDMF_ACTION_WRITE)
    call xh5%SetGrid(GridShape=(/size(X), size(Y), size(Z)/))
    call xh5%WriteGeometry(X=X, Y=Y, Z=Z)
    call xh5%WriteAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=temperature)  
    call xh5%Close()
    call xh5%Free()
```

Read:

```fortran
    !< Read XDMF/HDF5 file
    call xh5%Open(FilePrefix='fileprefix', GridType=XDMF_GRID_TYPE_RECTILINEAR, Action=XDMF_ACTION_READ)
    call xh5%ParseGrid()
    call xh5%ReadGeometry(X=out_X, Y=out_Y, Z=out_Z)
    call xh5%ReadAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=out_temperature)
    call xh5%Close()
    call xh5%Free()
```

### Temporal series with non-static grid

```fortran
    !< Write XDMF/HDF5 file.    
    call xh5%Open(FilePrefix='fileprefix', GridType=XDMF_GRID_TYPE_RECTILINEAR, Action=XDMF_ACTION_WRITE)

    do i=1, num_steps
        call xh5%AppendStep(Value=time+i)
        call xh5%SetGrid(GridShape=(/size(X), size(Y), size(Z)/))
        call xh5%WriteGeometry(X=X, Y=Y, Z=Z)
        call xh5%WriteAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=scalartempI4P+i)  
    enddo

    call xh5%Close()
    call xh5%Free()
```

### Temporal series with static grid

```fortran
    call xh5%Open(FilePrefix='fileprefix', GridType=XDMF_GRID_TYPE_UNSTRUCTURED, StaticGrid=.true., Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB, Action=XDMF_ACTION_WRITE)
    call xh5%SetGrid(NumberOfNodes=24, NumberOfElements=10, TopologyType=XDMF_TOPOLOGY_TYPE_MIXED, GeometryType=XDMF_GEOMETRY_TYPE_XYZ)

    call xh5%WriteTopology(Connectivities = topology)
    call xh5%WriteGeometry(XYZ = geometry)

    do i=1, num_steps
        time = time+1.0
        call xh5%AppendStep(Value=time)
        call xh5%WriteAttribute(Name='NodeField', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=nodefield+i)
        call xh5%WriteAttribute(Name='CellField', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=cellfield+i)
    enddo

    call xh5%Close()
    call xh5%Free()
```


## XH5For public parameters

```fortran
    !< Attribute centered on:
    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_GRID = 100
    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_CELL = 101
    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_FACE = 102 !< Not supported
    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_EDGE = 103 !< Not supported
    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_NODE = 104

    !< Attribute types:
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_SCALAR   = 200
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_VECTOR   = 201
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_TENSOR   = 202 !< Not supported
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_MATRIX   = 203 !< Not supported
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_TENSOR6  = 204 !< Not supported
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_GLOBALID = 205 !< Not supported
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_NOTYPE   = 206 !< Not supported

    !< Geometry types
    !<   * Unstructured
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_XY             = 301
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_XYZ            = 302
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_X_Y_Z          = 303

    !<   * Structured
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_VXVY           = 304
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_VXVYVZ         = 305
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_ORIGIN_DXDY    = 306
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ  = 307

    !< Collection types
    integer(I4P), parameter :: XDMF_GRID_COLLECTION_TYPE_SPATIAL  = 400
    integer(I4P), parameter :: XDMF_GRID_COLLECTION_TYPE_TEMPORAL = 401

    !< Topology types
    !<   * Unstructured
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_POLYVERTEX       = 500
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_POLYLINE         = 501
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_POLYGON          = 502
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_TRIANGLE         = 503
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_QUADRILATERAL    = 504
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_TETRAHEDRON      = 505
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_PYRAMID          = 506
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_WEDGE            = 507
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON       = 508
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_EDGE_3           = 509 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_TRIANGLE_6       = 510 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_QUADRILATERAL_8  = 511 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_QUADRILATERAL_9  = 512 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_TETRAHEDRON_10   = 513 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_PYRAMID_13       = 514 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_WEDGE_15         = 515 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_WEDGE_18         = 516 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_20    = 517 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_24    = 518 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_27    = 519 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_64    = 520 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_125   = 521 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_216   = 522 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_343   = 523 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_512   = 524 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_729   = 525 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1000  = 526 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1331  = 527 !< Not supported
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_MIXED            = 528

    !<   * Structured
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_2DSMESH          = 529
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_3DSMESH          = 530
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_2DRECTMESH       = 531
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_3DRECTMESH       = 532
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_2DCORECTMESH     = 533
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_3DCORECTMESH     = 534

    !< Set type
    integer(I4P), parameter :: XDMF_SET_TYPE_NODE = 601 !< Not supported
    integer(I4P), parameter :: XDMF_SET_TYPE_CELL = 602 !< Not supported
    integer(I4P), parameter :: XDMF_SET_TYPE_FACE = 603 !< Not supported
    integer(I4P), parameter :: XDMF_SET_TYPE_EDGE = 604 !< Not supported

    !< Grid type
    integer(I4P), parameter :: XDMF_GRID_TYPE_CURVILINEAR   = 701
    integer(I4P), parameter :: XDMF_GRID_TYPE_RECTILINEAR   = 702
    integer(I4P), parameter :: XDMF_GRID_TYPE_REGULAR       = 703
    integer(I4P), parameter :: XDMF_GRID_TYPE_UNSTRUCTURED  = 704

    !< HDF5 Strategies
    integer(I4P), parameter :: XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB  = 800
    integer(I4P), parameter :: XDMF_STRATEGY_DATASET_PER_PROCESS   = 801

    !< Actions
    integer(I4P), parameter :: XDMF_ACTION_READ   = 900
    integer(I4P), parameter :: XDMF_ACTION_WRITE  = 901
```

