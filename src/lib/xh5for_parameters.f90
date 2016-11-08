!-----------------------------------------------------------------
! XH5For (XDMF parallel partitioned mesh I/O on top of HDF5)
! Copyright (c) 2015 Santiago Badia, Alberto F. Martín, 
! Javier Principe and Víctor Sande.
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library.
!-----------------------------------------------------------------
module xh5for_parameters
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XH5For: XDMF parallel partitioned mesh I/O on top of HDF5
!< XH5For global parameters
!< @Note: Taked from Xdmf.f of the official XDMF API (http://www.xdmf.org)
!--------------------------------------------------------------------- -----------------------------------------------------------
use PENF, only: I4P

implicit none 

    character(len=4), parameter :: XDMF_EXT = '.xmf'
    character(len=3), parameter :: XI_EXT   = '.xi'
    character(len=3), parameter :: HDF5_EXT = '.h5'

    integer(I4P), parameter :: XDMF_STATIC_STEP  = 0 

    integer(I4P), parameter :: XDMF_NO_VALUE  = -1

    integer(I4P), parameter :: XDMF_ARRAY_TYPE_INT8    = 0
    integer(I4P), parameter :: XDMF_ARRAY_TYPE_INT16   = 1
    integer(I4P), parameter :: XDMF_ARRAY_TYPE_INT32   = 2
    integer(I4P), parameter :: XDMF_ARRAY_TYPE_INT64   = 3
    integer(I4P), parameter :: XDMF_ARRAY_TYPE_UINT8   = 4
    integer(I4P), parameter :: XDMF_ARRAY_TYPE_UINT16  = 5
    integer(I4P), parameter :: XDMF_ARRAY_TYPE_UINT32  = 6
    integer(I4P), parameter :: XDMF_ARRAY_TYPE_FLOAT32 = 7
    integer(I4P), parameter :: XDMF_ARRAY_TYPE_FLOAT64 = 8

    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_GRID = 100
    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_CELL = 101
    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_FACE = 102
    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_EDGE = 103
    integer(I4P), parameter :: XDMF_ATTRIBUTE_CENTER_NODE = 104

    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_SCALAR   = 200
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_VECTOR   = 201
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_TENSOR   = 202
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_MATRIX   = 203
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_TENSOR6  = 204
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_GLOBALID = 205
    integer(I4P), parameter :: XDMF_ATTRIBUTE_TYPE_NOTYPE   = 206

    ! Unstructured
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_XY             = 301
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_XYZ            = 302
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_X_Y_Z          = 303

    ! Structured
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_VXVY           = 304
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_VXVYVZ         = 305
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_ORIGIN_DXDY    = 306
    integer(I4P), parameter :: XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ  = 307

    integer(I4P), parameter :: XDMF_GRID_COLLECTION_TYPE_SPATIAL  = 400
    integer(I4P), parameter :: XDMF_GRID_COLLECTION_TYPE_TEMPORAL = 401

    ! Unstructured
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_POLYVERTEX       = 500
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_POLYLINE         = 501
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_POLYGON          = 502
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_TRIANGLE         = 503
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_QUADRILATERAL    = 504
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_TETRAHEDRON      = 505
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_PYRAMID          = 506
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_WEDGE            = 507
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON       = 508
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_EDGE_3           = 509
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_TRIANGLE_6       = 510
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_QUADRILATERAL_8  = 511
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_QUADRILATERAL_9  = 512
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_TETRAHEDRON_10   = 513
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_PYRAMID_13       = 514
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_WEDGE_15         = 515
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_WEDGE_18         = 516
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_20    = 517
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_24    = 518
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_27    = 519
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_64    = 520
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_125   = 521
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_216   = 522
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_343   = 523
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_512   = 524
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_729   = 525
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1000  = 526
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1331  = 527
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_MIXED            = 528

    ! Structured
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_2DSMESH          = 529
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_3DSMESH          = 530
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_2DRECTMESH       = 531
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_3DRECTMESH       = 532
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_2DCORECTMESH     = 533
    integer(I4P), parameter :: XDMF_TOPOLOGY_TYPE_3DCORECTMESH     = 534

    integer(I4P), parameter :: XDMF_SET_TYPE_NODE = 601
    integer(I4P), parameter :: XDMF_SET_TYPE_CELL = 602
    integer(I4P), parameter :: XDMF_SET_TYPE_FACE = 603
    integer(I4P), parameter :: XDMF_SET_TYPE_EDGE = 604

    integer(I4P), parameter :: XDMF_GRID_TYPE_CURVILINEAR   = 701
    integer(I4P), parameter :: XDMF_GRID_TYPE_RECTILINEAR   = 702
    integer(I4P), parameter :: XDMF_GRID_TYPE_REGULAR       = 703
    integer(I4P), parameter :: XDMF_GRID_TYPE_UNSTRUCTURED  = 704

    integer(I4P), parameter :: XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB  = 800
    integer(I4P), parameter :: XDMF_STRATEGY_DATASET_PER_PROCESS   = 801

    integer(I4P), parameter :: XDMF_ACTION_READ   = 900
    integer(I4P), parameter :: XDMF_ACTION_WRITE  = 901

    !-----------------------------------------------------------------
    !< ACTUALLY SUPPORTED PARAMETERS
    !----------------------------------------------------------------- 

    integer(I4P), parameter :: SUPPORTED_STRATEGIES(2)  = (/ &
                                                                XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB, &
                                                                XDMF_STRATEGY_DATASET_PER_PROCESS   &
                                                          /)

    integer(I4P), parameter :: SUPPORTED_GEOMETRYTYPES(7)  = (/ &
                                                                ! Unstructured
                                                                XDMF_GEOMETRY_TYPE_XY,            &
                                                                XDMF_GEOMETRY_TYPE_XYZ,           &
                                                                XDMF_GEOMETRY_TYPE_X_Y_Z,         &
                                                                ! Structured
                                                                XDMF_GEOMETRY_TYPE_VXVY,          &
                                                                XDMF_GEOMETRY_TYPE_VXVYVZ,        &
                                                                XDMF_GEOMETRY_TYPE_ORIGIN_DXDY,   &
                                                                XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ  &
                                                             /)

    integer(I4P), parameter :: SUPPORTED_TOPOLOGYTYPES(32) = (/ &
                                                                ! Unstructured
!                                                               XDMF_TOPOLOGY_TYPE_POLYVERTEX,      &
!                                                               XDMF_TOPOLOGY_TYPE_POLYLINE,        &
!                                                               XDMF_TOPOLOGY_TYPE_POLYGON,         &
                                                                XDMF_TOPOLOGY_TYPE_TRIANGLE,        &
                                                                XDMF_TOPOLOGY_TYPE_QUADRILATERAL,   &
                                                                XDMF_TOPOLOGY_TYPE_TETRAHEDRON,     &
                                                                XDMF_TOPOLOGY_TYPE_PYRAMID,         &
                                                                XDMF_TOPOLOGY_TYPE_WEDGE,           &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON,      &
                                                                XDMF_TOPOLOGY_TYPE_EDGE_3,          &
                                                                XDMF_TOPOLOGY_TYPE_TRIANGLE_6,      &
                                                                XDMF_TOPOLOGY_TYPE_QUADRILATERAL_8, &
                                                                XDMF_TOPOLOGY_TYPE_QUADRILATERAL_9, &
                                                                XDMF_TOPOLOGY_TYPE_TETRAHEDRON_10,  &
                                                                XDMF_TOPOLOGY_TYPE_PYRAMID_13,      &
                                                                XDMF_TOPOLOGY_TYPE_WEDGE_15,        &
                                                                XDMF_TOPOLOGY_TYPE_WEDGE_18,        &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_20,   &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_24,   &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_27,   &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_64,   &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_125,  &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_216,  &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_343,  &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_512,  &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_729,  &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1000, &
                                                                XDMF_TOPOLOGY_TYPE_HEXAHEDRON_1331, &
                                                                XDMF_TOPOLOGY_TYPE_MIXED,           &
                                                                ! Structured
                                                                XDMF_TOPOLOGY_TYPE_2DSMESH,         &
                                                                XDMF_TOPOLOGY_TYPE_3DSMESH,         &
                                                                XDMF_TOPOLOGY_TYPE_2DRECTMESH,      &
                                                                XDMF_TOPOLOGY_TYPE_3DRECTMESH,      &
                                                                XDMF_TOPOLOGY_TYPE_2DCORECTMESH,    &
                                                                XDMF_TOPOLOGY_TYPE_3DCORECTMESH     &
                                                             /)

    integer(I4P), parameter :: SUPPORTED_GRIDTYPES(4)  = (/ &
                                                            XDMF_GRID_TYPE_CURVILINEAR , &
                                                            XDMF_GRID_TYPE_RECTILINEAR , &
                                                            XDMF_GRID_TYPE_REGULAR     , &
                                                            XDMF_GRID_TYPE_UNSTRUCTURED  &
                                                         /)

end module xh5for_parameters
