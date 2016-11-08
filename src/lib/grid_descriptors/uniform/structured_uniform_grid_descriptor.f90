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
module structured_uniform_grid_descriptor

use PENF, only: I4P, I8P
use uniform_grid_descriptor
use xh5for_parameters

implicit none
private

    type, extends(uniform_grid_descriptor_t) :: structured_uniform_grid_descriptor_t
    contains
    private
        procedure :: Unstructured_Initialize => str_uniform_grid_descriptor_unstructured_initialize
        procedure :: Structured_Initialize   => str_uniform_grid_descriptor_structured_initialize
    end type structured_uniform_grid_descriptor_t

public:: structured_uniform_grid_descriptor_t

contains

    subroutine str_uniform_grid_descriptor_unstructured_initialize(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType, GridType)
    !-----------------------------------------------------------------
    !< Unstructured Uniform grid descriptor initization procedure
    !----------------------------------------------------------------- 
        class(structured_uniform_grid_descriptor_t), intent(INOUT) :: this  !< Local grid descriptor
        integer(I8P),                     intent(IN)    :: NumberOfNodes    !< Number of nodes of the local grid
        integer(I8P),                     intent(IN)    :: NumberOfElements !< Number of elements of the local grid
        integer(I4P),                     intent(IN)    :: TopologyType     !< Topology type of the local grid
        integer(I4P),                     intent(IN)    :: GeometryType     !< Geometry type of the local grid
        integer(I4P),                     intent(IN)    :: GridType         !< Grid type of the local grid
    !-----------------------------------------------------------------
        ! Not supported
        call this%Free()
        call this%SetGridType(GridType = GridType)
    end subroutine str_uniform_grid_descriptor_unstructured_initialize

    subroutine str_uniform_grid_descriptor_structured_initialize(this, Xdim, YDim, ZDim, GridType)
    !-----------------------------------------------------------------
    !< Structured Uniform grid descriptor initization procedure
    !----------------------------------------------------------------- 
        class(structured_uniform_grid_descriptor_t), intent(INOUT) :: this  !< Local grid descriptor
        integer(I8P),                     intent(IN)    :: XDim             !< Number of point on X axis
        integer(I8P),                     intent(IN)    :: YDim             !< Number of point on Y axis
        integer(I8P),                     intent(IN)    :: ZDim             !< Number of point on Z axis
        integer(I4P),                     intent(IN)    :: GridType         !< Grid type of the local grid
        integer(I8P)                                    :: NumberOfNodes    !< Number of nodes of the local grid
        integer(I8P)                                    :: NumberOfElements !< Number of elements of the local gri
    !-----------------------------------------------------------------
        call this%Free()
        call this%SetGridType(GridType = GridType)
        select case(GridType)
            case (XDMF_GRID_TYPE_CURVILINEAR)
                if(ZDim == 0_I8P) then
                    call this%SetTopologyType(TopologyType=XDMF_TOPOLOGY_TYPE_2DSMESH)
                    call this%SetGeometryType(GeometryType=XDMF_GEOMETRY_TYPE_VXVY)
                else
                    call this%SetTopologyType(TopologyType=XDMF_TOPOLOGY_TYPE_3DSMESH)
                    call this%SetGeometryType(GeometryType=XDMF_GEOMETRY_TYPE_VXVYVZ)
                endif
            case (XDMF_GRID_TYPE_RECTILINEAR)
                if(ZDim == 0_I8P) then
                    call this%SetTopologyType(TopologyType=XDMF_TOPOLOGY_TYPE_2DRECTMESH)
                    call this%SetGeometryType(GeometryType=XDMF_GEOMETRY_TYPE_VXVY)
                else
                    call this%SetTopologyType(TopologyType=XDMF_TOPOLOGY_TYPE_3DRECTMESH)
                    call this%SetGeometryType(GeometryType=XDMF_GEOMETRY_TYPE_VXVYVZ)
                endif
            case (XDMF_GRID_TYPE_REGULAR)
                if(ZDim == 0_I8P) then
                    call this%SetTopologyType(TopologyType=XDMF_TOPOLOGY_TYPE_2DCORECTMESH)
                    call this%SetGeometryType(GeometryType=XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                else
                    call this%SetTopologyType(TopologyType=XDMF_TOPOLOGY_TYPE_3DCORECTMESH)
                    call this%SetGeometryType(GeometryType=XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                endif
        end select
        NumberOfNodes = MAX(1,XDim)*MAX(1,YDim)*MAX(1,ZDim)
        NumberOfElements = (MAX(2,XDim)-1)*(MAX(2,YDim)-1)*(MAX(2,ZDim)-1)
        call this%SetNumberOfNodes(NumberOfNodes=NumberOfNodes)
        call this%SetNumberOfElements(NumberOfElements=NumberOfElements)
    end subroutine str_uniform_grid_descriptor_structured_initialize

end module structured_uniform_grid_descriptor
