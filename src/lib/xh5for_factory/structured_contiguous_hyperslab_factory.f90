module structured_contiguous_hyperslab_factory

use xh5for_abstract_factory
use xdmf_handler
use hdf5_handler
use uniform_grid_descriptor
use spatial_grid_descriptor
use structured_uniform_grid_descriptor
use structured_spatial_grid_descriptor
use xdmf_structured_contiguous_hyperslab_handler
use hdf5_structured_contiguous_hyperslab_handler

implicit none
private

    type, extends(xh5for_abstract_factory_t) :: structured_contiguous_hyperslab_factory_t
    contains
        procedure :: CreateUniformGridDescriptor => structured_contiguous_hyperslab_CreateUniformGridDescriptor
        procedure :: CreateSpatialGridDescriptor => structured_contiguous_hyperslab_CreateSpatialGridDescriptor
        procedure :: CreateXDMFHandler           => structured_contiguous_hyperslab_CreateXDMFHandler
        procedure :: CreateHDF5Handler           => structured_contiguous_hyperslab_CreateHDF5Handler
    end type structured_contiguous_hyperslab_factory_t

public :: structured_contiguous_hyperslab_factory_t

contains

    subroutine structured_contiguous_hyperslab_CreateUniformGridDescriptor(this, UniformGridDescriptor)
    !-----------------------------------------------------------------
    !< Return an structured uniform grid descriptor
    !----------------------------------------------------------------- 
        class(structured_contiguous_hyperslab_factory_t), intent(IN)  :: this                  !< structured grid descriptor factory
        class(uniform_grid_descriptor_t), allocatable,    intent(OUT) :: UniformGridDescriptor !< Uniform grid descriptor
    !----------------------------------------------------------------- 
        allocate(structured_uniform_grid_descriptor_t :: UniformGridDescriptor)
    end subroutine structured_contiguous_hyperslab_CreateUniformGridDescriptor


    subroutine structured_contiguous_hyperslab_CreateSpatialGridDescriptor(this, SpatialGridDescriptor)
    !-----------------------------------------------------------------
    !< Return an structured spatial grid descriptor
    !----------------------------------------------------------------- 
        class(structured_contiguous_hyperslab_factory_t), intent(IN)  :: this                  !< structured grid descriptor factory
        class(spatial_grid_descriptor_t), allocatable,    intent(OUT) :: SpatialGridDescriptor !< Spatial grid descriptor
    !----------------------------------------------------------------- 
        allocate(structured_spatial_grid_descriptor_t :: SpatialGridDescriptor)
    end subroutine structured_contiguous_hyperslab_CreateSpatialGridDescriptor


    subroutine structured_contiguous_hyperslab_CreateXDMFHandler(this, XDMFHandler)
    !-----------------------------------------------------------------
    !< Return an structured contiguous hyperslab XDMF handler
    !----------------------------------------------------------------- 
        class(structured_contiguous_hyperslab_factory_t), intent(IN)  :: this        !< structured contiguous hyperslab factory
        class(xdmf_handler_t), allocatable,               intent(OUT) :: XDMFHandler !< XDMF handler
    !----------------------------------------------------------------- 
        allocate(xdmf_structured_contiguous_hyperslab_handler_t :: XDMFHandler)
    end subroutine structured_contiguous_hyperslab_CreateXDMFHandler


    subroutine structured_contiguous_hyperslab_CreateHDF5Handler(this, HDF5Handler)
    !-----------------------------------------------------------------
    !< Return an structured contiguous hyperslab HDF5 handler
    !----------------------------------------------------------------- 
        class(structured_contiguous_hyperslab_factory_t), intent(IN)  :: this        !< structured contiguous hyperslab factory
        class(hdf5_handler_t), allocatable,               intent(OUT) :: HDF5Handler !< HDF5 handler
    !----------------------------------------------------------------- 
        allocate(hdf5_structured_contiguous_hyperslab_handler_t :: HDF5Handler)
    end subroutine structured_contiguous_hyperslab_CreateHDF5Handler

end module structured_contiguous_hyperslab_factory
