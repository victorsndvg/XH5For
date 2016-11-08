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
module xdmf_parameters
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XH5For: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF global parameters
!< @Note: Taked from Xdmf.f of the official XDMF API (http://www.xdmf.org)
!--------------------------------------------------------------------- -----------------------------------------------------------

USE PENF, only: I4P

implicit none 

    integer(I4P), parameter :: SUPPORTED_DATAITEMPRECISIONS(4) = (/ 1, 2, 4, 8 /)

    character(len=*), parameter :: SUPPORTED_TOPOLOGYTYPENAMES =                                        &
                                            'Polyvertex&Polyline&Polygon&Triangle&Quadrilateral'     // &
                                            '&Tetrahedron&Pyramid&Wedge&Hexahedron&Edge_3&Triangle_6'// &
                                            '&Quadrilateral_8&Tetrahedron_10&Pyramid_13&Wedge_15'    // &
                                            '&Hexahedron_20&Mixed&2DSMesh&2DRectMesh&2DCoRectMesh'   // &
                                            '&3DSMesh&3DRectMesh&3DCoRectMesh'

    character(len=*), parameter :: SUPPORTED_TIMETYPENAMES = 'Single&HyperSlab&List&Range'

    character(len=*), parameter :: SUPPORTED_GRIDTYPENAMES = 'Uniform&Collection&Tree&Subset'

    character(len=*), parameter :: SUPPORTED_GRIDCOLLECTIONTYPENAMES = 'Spatial&Temporal'

    character(len=*), parameter :: SUPPORTED_GRIDCOLLECTIONSECTIONAMES = 'DataItem&All'

    character(len=*), parameter :: SUPPORTED_ATTRIBUTETYPENAMES = 'Scalar&Vector&Tensor&Tensor6&Matrix&GlobalID'

    character(len=*), parameter :: SUPPORTED_ATTRIBUTECENTERNAMES = 'Node&Cell&Grid&Face&Edge'

    character(len=*), parameter :: SUPPORTED_DATAITEMTYPENAMES = 'Uniform&Collection&Tree&HyperSlab&Coordinates&Function'

    character(len=*), parameter :: SUPPORTED_DATAITEMNUMBERTYPENAMES = 'Float&Int&UInt&Char&UChar'

    character(len=*), parameter :: SUPPORTED_DATAITEMFORMATNAMES = 'XML&HDF'

end module xdmf_parameters
