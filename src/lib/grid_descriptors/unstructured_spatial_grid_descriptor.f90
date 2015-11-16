module unstructured_spatial_grid_descriptor

use spatial_grid_descriptor

implicit none

private

    type, extends(spatial_grid_descriptor_t) :: unstructured_spatial_grid_descriptor_t
    private
    end type unstructured_spatial_grid_descriptor_t

public:: unstructured_spatial_grid_descriptor_t


end module unstructured_spatial_grid_descriptor
