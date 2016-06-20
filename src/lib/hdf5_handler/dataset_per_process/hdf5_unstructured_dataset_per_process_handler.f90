module hdf5_unstructured_dataset_per_process_handler

use IR_Precision, only : I4P, I8P, R4P, R8P, str
#ifdef ENABLE_HDF5
use HDF5
#endif
use hdf5_dataset_per_process_handler
use xh5for_utils
use xh5for_parameters
use mpi_environment
use uniform_grid_descriptor
use spatial_grid_descriptor

implicit none

#include "assert.i90"

private

    type, extends(hdf5_dataset_per_process_handler_t) :: hdf5_unstructured_dataset_per_process_handler_t
    !-----------------------------------------------------------------
    !< HDF5 dataset per process handler for Unstructured grids
    !----------------------------------------------------------------- 
    contains
    private
        procedure :: WriteGeometry_XYZ_R4P   => hdf5_unstructured_dataset_per_process_WriteGeometry_XYZ_R4P
        procedure :: WriteGeometry_XYZ_R8P   => hdf5_unstructured_dataset_per_process_WriteGeometry_XYZ_R8P
        procedure :: WriteGeometry_X_Y_Z_R4P => hdf5_unstructured_dataset_per_process_WriteGeometry_X_Y_Z_R4P
        procedure :: WriteGeometry_X_Y_Z_R8P => hdf5_unstructured_dataset_per_process_WriteGeometry_X_Y_Z_R8P
        procedure :: WriteGeometry_DXDYDZ_R4P=> hdf5_unstructured_dataset_per_process_WriteGeometry_DXDYDZ_R4P
        procedure :: WriteGeometry_DXDYDZ_R8P=> hdf5_unstructured_dataset_per_process_WriteGeometry_DXDYDZ_R8P
        procedure :: ReadGeometry_XYZ_R4P    => hdf5_unstructured_dataset_per_process_ReadGeometry_XYZ_R4P
        procedure :: ReadGeometry_XYZ_R8P    => hdf5_unstructured_dataset_per_process_ReadGeometry_XYZ_R8P
        procedure :: ReadGeometry_X_Y_Z_R4P  => hdf5_unstructured_dataset_per_process_ReadGeometry_X_Y_Z_R4P
        procedure :: ReadGeometry_X_Y_Z_R8P  => hdf5_unstructured_dataset_per_process_ReadGeometry_X_Y_Z_R8P
        procedure :: ReadGeometry_DXDYDZ_R4P => hdf5_unstructured_dataset_per_process_ReadGeometry_DXDYDZ_R4P
        procedure :: ReadGeometry_DXDYDZ_R8P => hdf5_unstructured_dataset_per_process_ReadGeometry_DXDYDZ_R8P
        procedure :: WriteTopology_I4P       => hdf5_unstructured_dataset_per_process_WriteTopology_I4P
        procedure :: WriteTopology_I8P       => hdf5_unstructured_dataset_per_process_WriteTopology_I8P
        procedure :: ReadTopology_I4P        => hdf5_unstructured_dataset_per_process_ReadTopology_I4P
        procedure :: ReadTopology_I8P        => hdf5_unstructured_dataset_per_process_ReadTopology_I8P
    end type hdf5_unstructured_dataset_per_process_handler_t

public :: hdf5_unstructured_dataset_per_process_handler_t

contains

    subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_XYZ_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R4P),                                  intent(IN) :: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: LocalGeometrySize   !< Local size of the geometry hyperslab
        integer(I4P)                                           :: GridID              !< Index to loop on GridID's
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        do GridID=0, MPIEnvironment%get_comm_size()-1
            LocalGeometrySize  = int(SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID),HSIZE_T)
            call this%WriteMetadata(                                                           &
                    DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalGeometrySize/),                                   &
                    HyperSlabOffset = (/0_HSIZE_T/),                                           &
                    HyperSlabSize   = (/LocalGeometrySize/),                                   &
                    Values          = XYZ)
        enddo
        LocalGeometrySize  = int(SpatialGridDescriptor%GetGeometrySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteData(                                                                                       &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalGeometrySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalGeometrySize/),                                                           &
                Values          = XYZ)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_XYZ_R4P


    subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_XYZ_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R8P),                                  intent(IN) :: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: LocalGeometrySize   !< Local size of the geometry hyperslab
        integer(I4P)                                           :: GridID              !< Index to loop on GridID's
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        do GridID=0, MPIEnvironment%get_comm_size()-1
            LocalGeometrySize  = int(SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID),HSIZE_T)
            call this%WriteMetadata(                                                           &
                    DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalGeometrySize/),                                   &
                    HyperSlabOffset = (/0_HSIZE_T/),                                           &
                    HyperSlabSize   = (/LocalGeometrySize/),                                   &
                    Values          = XYZ)
        enddo
        LocalGeometrySize  = int(SpatialGridDescriptor%GetGeometrySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteData(                                                                                       &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalGeometrySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalGeometrySize/),                                                           &
                Values          = XYZ)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_XYZ_R8P


    subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R4P),                                  intent(IN) :: X(:)                !< X Grid coordinates
        real(R4P),                                  intent(IN) :: Y(:)                !< Y Grid coordinates
        real(R4P),                                  intent(IN) :: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: LocalNumberOfNodes  !< Local number of nodes
        integer(I4P)                                           :: GridID              !< Index to loop over GridID's
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        do GridID=0, MPIEnvironment%get_comm_size()-1
            LocalNumberOfNodes  = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=GridID),HSIZE_T)
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalNumberOfNodes/),                                        &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalNumberOfNodes/),                                        &
                    Values          = X)
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalNumberOfNodes/),                                        &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalNumberOfNodes/),                                        &
                    Values          = Y)
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalNumberOfNodes/),                                        &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalNumberOfNodes/),                                        &
                    Values          = Z)
        enddo
        LocalNumberOfNodes  = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteData(                                                                                         &
                DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalNumberOfNodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalNumberOfNodes/),                                                                &
                Values          = X)
        call this%WriteData(                                                                                         &
                DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalNumberOfNodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalNumberOfNodes/),                                                                &
                Values          = Y)
        call this%WriteData(                                                                                         &
                DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalNumberOfNodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalNumberOfNodes/),                                                                &
                Values          = Z)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_X_Y_Z_R4P


    subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R8P),                                  intent(IN) :: X(:)                !< X Grid coordinates
        real(R8P),                                  intent(IN) :: Y(:)                !< Y Grid coordinates
        real(R8P),                                  intent(IN) :: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: LocalNumberOfNodes  !< Local number of nodes
        integer(I4P)                                           :: GridID              !< Index to loop over GridID's
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        do GridID=0, MPIEnvironment%get_comm_size()-1
            LocalNumberOfNodes  = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=GridID),HSIZE_T)
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalNumberOfNodes/),                                        &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalNumberOfNodes/),                                        &
                    Values          = X)
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalNumberOfNodes/),                                        &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalNumberOfNodes/),                                        &
                    Values          = Y)
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalNumberOfNodes/),                                        &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalNumberOfNodes/),                                        &
                    Values          = Z)
        enddo
        LocalNumberOfNodes  = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteData(                                                                                         &
                DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalNumberOfNodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalNumberOfNodes/),                                                                &
                Values          = X)
        call this%WriteData(                                                                                         &
                DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalNumberOfNodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalNumberOfNodes/),                                                                &
                Values          = Y)
        call this%WriteData(                                                                                         &
                DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalNumberOfNodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalNumberOfNodes/),                                                                &
                Values          = Z)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_X_Y_Z_R8P


    subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R4P),                                  intent(IN) :: Origin(:)           !< Origin coordinates
        real(R4P),                                  intent(IN) :: DxDyDz(:)           !< Coordinates sted to the next point
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! Not supported
#endif
    end subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_DXDYDZ_R4P


    subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R8P),                                  intent(IN) :: Origin(:)           !< Origin coordinates
        real(R8P),                                  intent(IN) :: DxDyDz(:)           !< Coordinates sted to the next point
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! Not supported
#endif
    end subroutine hdf5_unstructured_dataset_per_process_WriteGeometry_DXDYDZ_R8P


    subroutine hdf5_unstructured_dataset_per_process_WriteTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Writes I4P connectivities to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this   !< HDF5 dataset per process handler for Unstructured grids
        integer(I4P),                               intent(IN) :: Connectivities(:)  !< I4P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(I4P)                                           :: GridID             !< Index to loop over GridID's
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(uniform_grid_descriptor_t), pointer              :: UniformGridDescriptor !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        call UniformGridDescriptor%SetTopologySize(int(size(connectivities,dim=1),I8P))
        call SpatialGridDescriptor%SetTopologySizePerGridID(int(size(connectivities,dim=1),I8P),ID=MPIEnvironment%get_rank())
        do GridID=0, MPIEnvironment%get_comm_size()-1
            LocalTopologySize = int(SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID),HSIZE_T)
            call this%WriteMetaData(                                                           &
                    DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalTopologySize/),                                   &
                    HyperSlabOffset = (/0_HSIZE_T/),                                           &
                    HyperSlabSize   = (/LocalTopologySize/),                                   &
                    Values          = Connectivities)
        enddo
        LocalTopologySize = int(SpatialGridDescriptor%GetTopologySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteData(                                                                                   &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalTopologySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalTopologySize/),                                                           &
                Values          = Connectivities)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_WriteTopology_I4P


    subroutine hdf5_unstructured_dataset_per_process_WriteTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Writes I8P connectivities to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this   !< HDF5 dataset per process handler for Unstructured grids
        integer(I8P),                               intent(IN) :: Connectivities(:)  !< I8P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(I4P)                                           :: GridID             !< Index to loop over GridID's
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(uniform_grid_descriptor_t), pointer              :: UniformGridDescriptor !< Uniform grid descriptor
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        UniformGridDescriptor => this%GetUniformGridDescriptor()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(UniformGridDescriptor) .and. associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        call UniformGridDescriptor%SetTopologySize(int(size(connectivities,dim=1),I8P))
        call SpatialGridDescriptor%SetTopologySizePerGridID(int(size(connectivities,dim=1),I8P),ID=MPIEnvironment%get_rank())
        do GridID=0, MPIEnvironment%get_comm_size()-1
            LocalTopologySize = int(SpatialGridDescriptor%GetTopologySizePerGridID(ID=GridID),HSIZE_T)
            call this%WriteMetadata(                                                           &
                    DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalTopologySize/),                                   &
                    HyperSlabOffset = (/0_HSIZE_T/),                                           &
                    HyperSlabSize   = (/LocalTopologySize/),                                   &
                    Values          = Connectivities)
        enddo
        LocalTopologySize = int(SpatialGridDescriptor%GetTopologySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteData(                                                                                   &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalTopologySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalTopologySize/),                                                           &
                Values          = Connectivities)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_WriteTopology_I8P


    subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_XYZ_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Read XY[Z] R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R4P), allocatable,                     intent(OUT):: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        spacedim = int(GetSpaceDimension(SpatialGridDescriptor%GetGeometryTypePerGridID(ID=MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffset = int(SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%ReadDataset(                                                                                     &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/spacedim*localnumberofnodes/),                                                 &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),                                                 &
                Values          = XYZ)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_XYZ_R4P


    subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_XYZ_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Read XY[Z] R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R8P), allocatable,                     intent(OUT):: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        spacedim = int(GetSpaceDimension(SpatialGridDescriptor%GetGeometryTypePerGridID(ID=MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffset = int(SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%ReadDataset(                                                                                     &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/spacedim*localnumberofnodes/),                                                 &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),                                                 &
                Values          = XYZ)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_XYZ_R8P


    subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Read R4P X_Y_Z coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R4P), allocatable,                     intent(OUT):: X(:)                !< X Grid coordinates
        real(R4P), allocatable,                     intent(OUT):: Y(:)                !< Y Grid coordinates
        real(R4P), allocatable,                     intent(OUT):: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        globalnumberofnodes = int(SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffset = int(SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
        call this%ReadDataset(                                                                                           &
                DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnumberofnodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnumberofnodes/),                                                                &
                Values          = X)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
        call this%ReadDataset(                                                                                           &
                DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnumberofnodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnumberofnodes/),                                                                &
                Values          = Y)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
        call this%ReadDataset(                                                                                           &
                DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnumberofnodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnumberofnodes/),                                                                &
                Values          = Z)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_X_Y_Z_R4P


    subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Read X_Y_Z R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R8P), allocatable,                     intent(OUT):: X(:)                !< X Grid coordinates
        real(R8P), allocatable,                     intent(OUT):: Y(:)                !< Y Grid coordinates
        real(R8P), allocatable,                     intent(OUT):: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        globalnumberofnodes = int(SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffset = int(SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
        call this%ReadDataset(                                                                                           &
                DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnumberofnodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnumberofnodes/),                                                                &
                Values          = X)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
        call this%ReadDataset(                                                                                           &
                DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnumberofnodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnumberofnodes/),                                                                &
                Values          = Y)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
        call this%ReadDataset(                                                                                           &
                DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnumberofnodes/),                                                                &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnumberofnodes/),                                                                &
                Values          = Z)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_X_Y_Z_R8P


    subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Read R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R4P), allocatable,                     intent(OUT) :: Origin(:)          !< Origin coordinates
        real(R4P), allocatable,                     intent(OUT) :: DxDyDz(:)          !< Coordinates sted to the next point
        character(len=*),                           intent(IN)  :: Name               !< Geometry dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! Not supported
#endif
    end subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_DXDYDZ_R4P


    subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Read R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this    !< HDF5 dataset per process handler for Unstructured grids
        real(R8P), allocatable,                     intent(OUT) :: Origin(:)          !< Origin coordinates
        real(R8P), allocatable,                     intent(OUT) :: DxDyDz(:)          !< Coordinates sted to the next point
        character(len=*),                           intent(IN)  :: Name               !< Geometry dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! Not supported
#endif
    end subroutine hdf5_unstructured_dataset_per_process_ReadGeometry_DXDYDZ_R8P


    subroutine hdf5_unstructured_dataset_per_process_ReadTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read I4P connectivities to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this   !< HDF5 dataset per process handler for Unstructured grids
        integer(I4P), allocatable,                  intent(OUT):: Connectivities(:)  !< I4P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        GlobalTopologySize = int(SpatialGridDescriptor%GetGlobalTopologySize(),HSIZE_T)
        LocalTopologySize  =  int(SpatialGridDescriptor%GetTopologySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        TopologySizeOffset = int(SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%ReadDataset(                                                                                     &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalTopologySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalTopologySize/),                                                           &
                Values          = Connectivities)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_ReadTopology_I4P


    subroutine hdf5_unstructured_dataset_per_process_ReadTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read I8P connectivities to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_dataset_per_process_handler_t), intent(IN) :: this   !< HDF5 dataset per process handler for Unstructured grids
        integer(I8P), allocatable,                  intent(OUT):: Connectivities(:)  !< I8P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        GlobalTopologySize = int(SpatialGridDescriptor%GetGlobalTopologySize(),HSIZE_T)
        LocalTopologySize =  int(SpatialGridDescriptor%GetTopologySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        TopologySizeOffset = int(SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%ReadDataset(                                                                                     &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalTopologySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalTopologySize/),                                                           &
                Values          = Connectivities)
#endif
    end subroutine hdf5_unstructured_dataset_per_process_ReadTopology_I8P

end module hdf5_unstructured_dataset_per_process_handler
