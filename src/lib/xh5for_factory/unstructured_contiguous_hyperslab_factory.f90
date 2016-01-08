module unstructured_contiguous_hyperslab_factory

use xh5for_abstract_factory
use xdmf_handler
use hdf5_handler
use uniform_grid_descriptor
use spatial_grid_descriptor
use unstructured_uniform_grid_descriptor
use unstructured_spatial_grid_descriptor
use xdmf_unstructured_contiguous_hyperslab_handler
use hdf5_unstructured_contiguous_hyperslab_handler

implicit none
private

    type, extends(xh5for_abstract_factory_t) :: unstructured_contiguous_hyperslab_factory_t
    contains
        procedure :: CreateUniformGridDescriptor => unstructured_contiguous_hyperslab_CreateUniformGridDescriptor
        procedure :: CreateSpatialGridDescriptor => unstructured_contiguous_hyperslab_CreateSpatialGridDescriptor
        procedure :: CreateXDMFHandler           => unstructured_contiguous_hyperslab_CreateXDMFHandler
        procedure :: CreateHDF5Handler           => unstructured_contiguous_hyperslab_CreateHDF5Handler
    end type unstructured_contiguous_hyperslab_factory_t

public :: unstructured_contiguous_hyperslab_factory_t

contains

    subroutine unstructured_contiguous_hyperslab_CreateUniformGridDescriptor(this, UniformGridDescriptor)
    !-----------------------------------------------------------------
    !< Return an unstructured uniform grid descriptor
    !----------------------------------------------------------------- 
        class(unstructured_contiguous_hyperslab_factory_t), intent(IN)  :: this                  !< Unstructured grid descriptor factory
        class(uniform_grid_descriptor_t), allocatable,      intent(OUT) :: UniformGridDescriptor !< Uniform grid descriptor
    !----------------------------------------------------------------- 
        allocate(unstructured_uniform_grid_descriptor_t :: UniformGridDescriptor)
    end subroutine unstructured_contiguous_hyperslab_CreateUniformGridDescriptor


    subroutine unstructured_contiguous_hyperslab_CreateSpatialGridDescriptor(this, SpatialGridDescriptor)
    !-----------------------------------------------------------------
    !< Return an unstructured spatial grid descriptor
    !----------------------------------------------------------------- 
        class(unstructured_contiguous_hyperslab_factory_t), intent(IN)  :: this                  !< Unstructured grid descriptor factory
        class(spatial_grid_descriptor_t), allocatable,      intent(OUT) :: SpatialGridDescriptor !< Spatial grid descriptor
    !----------------------------------------------------------------- 
        allocate(unstructured_spatial_grid_descriptor_t :: SpatialGridDescriptor)
    end subroutine unstructured_contiguous_hyperslab_CreateSpatialGridDescriptor


    subroutine unstructured_contiguous_hyperslab_CreateXDMFHandler(this, XDMFHandler)
    !-----------------------------------------------------------------
    !< Return an unstructured contiguous hyperslab XDMF handler
    !----------------------------------------------------------------- 
        class(unstructured_contiguous_hyperslab_factory_t), intent(IN)  :: this        !< Unstructured contiguous hyperslab factory
        class(xdmf_handler_t), allocatable,                 intent(OUT) :: XDMFHandler !< XDMF handler
    !----------------------------------------------------------------- 
        allocate(xdmf_unstructured_contiguous_hyperslab_handler_t :: XDMFHandler)
    end subroutine unstructured_contiguous_hyperslab_CreateXDMFHandler


    subroutine unstructured_contiguous_hyperslab_CreateHDF5Handler(this, HDF5Handler)
    !-----------------------------------------------------------------
    !< Return an unstructured contiguous hyperslab HDF5 handler
    !----------------------------------------------------------------- 
        class(unstructured_contiguous_hyperslab_factory_t), intent(IN)  :: this        !< Unstructured contiguous hyperslab factory
        class(hdf5_handler_t), allocatable,                 intent(OUT) :: HDF5Handler !< HDF5 handler
    !----------------------------------------------------------------- 
        allocate(hdf5_unstructured_contiguous_hyperslab_handler_t :: HDF5Handler)
    end subroutine unstructured_contiguous_hyperslab_CreateHDF5Handler

end module unstructured_contiguous_hyperslab_factory
