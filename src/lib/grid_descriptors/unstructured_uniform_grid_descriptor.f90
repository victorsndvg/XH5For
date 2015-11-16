module unstructured_uniform_grid_descriptor

use uniform_grid_descriptor

implicit none

private

    type, extends(uniform_grid_descriptor_t) :: unstructured_uniform_grid_descriptor_t
    private
    end type unstructured_uniform_grid_descriptor_t

public:: unstructured_uniform_grid_descriptor_t


end module unstructured_uniform_grid_descriptor
