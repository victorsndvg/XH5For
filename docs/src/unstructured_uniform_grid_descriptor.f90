module unstructured_uniform_grid_descriptor

use PENF, only: I4P, I8P
use uniform_grid_descriptor
use xh5for_parameters

implicit none
private

    type, extends(uniform_grid_descriptor_t) :: unstructured_uniform_grid_descriptor_t
    contains
    private
        procedure :: Unstructured_Initialize => unstr_uniform_grid_descriptor_unstructured_initialize
        procedure :: Structured_Initialize   => unstr_uniform_grid_descriptor_structured_initialize
    end type unstructured_uniform_grid_descriptor_t

public:: unstructured_uniform_grid_descriptor_t

contains

    subroutine unstr_uniform_grid_descriptor_unstructured_initialize(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType, GridType)
    !-----------------------------------------------------------------
    !< Unstructured Uniform grid descriptor initization procedure
    !----------------------------------------------------------------- 
        class(unstructured_uniform_grid_descriptor_t), intent(INOUT) :: this !< Local grid descriptor
        integer(I8P),                     intent(IN)    :: NumberOfNodes     !< Number of nodes of the local grid
        integer(I8P),                     intent(IN)    :: NumberOfElements  !< Number of elements of the local grid
        integer(I4P),                     intent(IN)    :: TopologyType      !< Topology type of the local grid
        integer(I4P),                     intent(IN)    :: GeometryType      !< Geometry type of the local grid
        integer(I4P),                     intent(IN)    :: GridType          !< Grid type of the local grid
    !-----------------------------------------------------------------
        call this%Free()
        call this%SetGridType(GridType=GridType)
        select case(GridType)
            case (XDMF_GRID_TYPE_UNSTRUCTURED)
                call this%SetNumberOfNodes(NumberOfNodes=NumberOfNodes)
                call this%SetNumberOfElements(NumberOfElements=NumberOfElements)
                call this%SetTopologyType(TopologyType=TopologyType)
                call this%SetGeometryType(GeometryType=GeometryType)
        end select
    end subroutine unstr_uniform_grid_descriptor_unstructured_initialize


    subroutine unstr_uniform_grid_descriptor_structured_initialize(this, XDim, YDim, ZDim, GridType)
    !-----------------------------------------------------------------
    !< Structured Uniform grid descriptor initization procedure
    !----------------------------------------------------------------- 
        class(unstructured_uniform_grid_descriptor_t), intent(INOUT) :: this !< Local grid descriptor
        integer(I8P),                     intent(IN)    :: XDim              !< Number of points on X axis
        integer(I8P),                     intent(IN)    :: YDim              !< Number of points on Y axis
        integer(I8P),                     intent(IN)    :: ZDim              !< Number of points on Z axis
        integer(I8P)                                    :: NumberOfNodes     !< Number of nodes of the local grid
        integer(I8P)                                    :: NumberOfElements  !< Number of elements of the local grid
        integer(I4P),                     intent(IN)    :: GridType          !< Grid type of the local grid
    !----------------------------------------------------------------- 
        call this%Free()
        call this%SetGridType(GridType=GridType)
        select case(GridType)
            case (XDMF_GRID_TYPE_UNSTRUCTURED)
                if(ZDim == 0_I8P) then
                    call this%SetGeometryType(GeometryType=XDMF_GEOMETRY_TYPE_XY)
                    call this%SetTopologyType(TopologyType=XDMF_TOPOLOGY_TYPE_QUADRILATERAL)
                else
                    call this%SetTopologyType(TopologyType=XDMF_GEOMETRY_TYPE_XYZ)
                    call this%SetTopologyType(TopologyType=XDMF_TOPOLOGY_TYPE_HEXAHEDRON)
                endif
        end select
        NumberOfNodes = MAX(1,XDim)*MAX(1,YDim)*MAX(1,ZDim)
        NumberOfElements = (MAX(2,XDim)-1)*(MAX(2,YDim)-1)*(MAX(2,ZDim)-1)
        call this%SetNumberOfNodes(NumberOfNodes=NumberOfNodes)
        call this%SetNumberOfElements(NumberOfElements=NumberOfElements)
    end subroutine unstr_uniform_grid_descriptor_structured_initialize

end module unstructured_uniform_grid_descriptor
