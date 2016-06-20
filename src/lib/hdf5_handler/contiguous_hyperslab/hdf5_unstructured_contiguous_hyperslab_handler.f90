module hdf5_unstructured_contiguous_hyperslab_handler

use IR_Precision, only : I4P, I8P, R4P, R8P
#ifdef ENABLE_HDF5
use HDF5
#endif
use hdf5_contiguous_hyperslab_handler
use xh5for_utils
use xh5for_parameters
use mpi_environment
use spatial_grid_descriptor

implicit none

private

    type, extends(hdf5_contiguous_hyperslab_handler_t) :: hdf5_unstructured_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< HDF5 contiguous hyperslab handler for Unstructured grids
    !----------------------------------------------------------------- 
    contains
    private
        procedure :: WriteGeometry_XYZ_R4P   => hdf5_unstructured_contiguous_hyperslab_WriteGeometry_XYZ_R4P
        procedure :: WriteGeometry_XYZ_R8P   => hdf5_unstructured_contiguous_hyperslab_WriteGeometry_XYZ_R8P
        procedure :: WriteGeometry_X_Y_Z_R4P => hdf5_unstructured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R4P
        procedure :: WriteGeometry_X_Y_Z_R8P => hdf5_unstructured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R8P
        procedure :: WriteGeometry_DXDYDZ_R4P=> hdf5_unstructured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R4P
        procedure :: WriteGeometry_DXDYDZ_R8P=> hdf5_unstructured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R8P
        procedure :: ReadGeometry_XYZ_R4P    => hdf5_unstructured_contiguous_hyperslab_ReadGeometry_XYZ_R4P
        procedure :: ReadGeometry_XYZ_R8P    => hdf5_unstructured_contiguous_hyperslab_ReadGeometry_XYZ_R8P
        procedure :: ReadGeometry_X_Y_Z_R4P  => hdf5_unstructured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R4P
        procedure :: ReadGeometry_X_Y_Z_R8P  => hdf5_unstructured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R8P
        procedure :: ReadGeometry_DXDYDZ_R4P => hdf5_unstructured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R4P
        procedure :: ReadGeometry_DXDYDZ_R8P => hdf5_unstructured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R8P
        procedure :: WriteTopology_I4P       => hdf5_unstructured_contiguous_hyperslab_WriteTopology_I4P
        procedure :: WriteTopology_I8P       => hdf5_unstructured_contiguous_hyperslab_WriteTopology_I8P
        procedure :: ReadTopology_I4P        => hdf5_unstructured_contiguous_hyperslab_ReadTopology_I4P
        procedure :: ReadTopology_I8P        => hdf5_unstructured_contiguous_hyperslab_ReadTopology_I8P
    end type hdf5_unstructured_contiguous_hyperslab_handler_t

public :: hdf5_unstructured_contiguous_hyperslab_handler_t

contains

    subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_XYZ_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R4P),                                  intent(IN) :: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: GlobalGeometrySize  !< Total size of the geometry dataset
        integer(HSIZE_T)                                       :: LocalGeometrySize   !< Local size of the geometry hyperslab
        integer(HSIZE_T)                                       :: GeometrySizeOffset  !< Geometry size offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        GlobalGeometrySize = int(SpatialGridDescriptor%GetGlobalGeometrySize(),HSIZE_T)
        LocalGeometrySize  = int(SpatialGridDescriptor%GetGeometrySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        GeometrySizeOffset = int(SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName=Name,        &
                DatasetDims     = (/GlobalGeometrySize/), &
                HyperSlabOffset = (/GeometrySizeOffset/), &
                HyperSlabSize   = (/LocalGeometrySize/),  &
                Values          = XYZ)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_XYZ_R4P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_XYZ_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R8P),                                  intent(IN) :: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: GlobalGeometrySize  !< Total size of the geometry dataset
        integer(HSIZE_T)                                       :: LocalGeometrySize   !< Local size of the geometry hyperslab
        integer(HSIZE_T)                                       :: GeometrySizeOffset  !< Geometry size offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        GlobalGeometrySize = int(SpatialGridDescriptor%GetGlobalGeometrySize(),HSIZE_T)
        LocalGeometrySize  = int(SpatialGridDescriptor%GetGeometrySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        GeometrySizeOffset = int(SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName=Name,        &
                DatasetDims     = (/GlobalGeometrySize/), &
                HyperSlabOffset = (/GeometrySizeOffset/), &
                HyperSlabSize   = (/LocalGeometrySize/),  &
                Values          = XYZ)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_XYZ_R8P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R4P),                                  intent(IN) :: X(:)                !< X Grid coordinates
        real(R4P),                                  intent(IN) :: Y(:)                !< Y Grid coordinates
        real(R4P),                                  intent(IN) :: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: GlobalNumberOfNodes !< Total number of nodes
        integer(HSIZE_T)                                       :: LocalNumberOfNodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: NodeOffset          !< Node offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        GlobalNumberOfNodes = int(SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        LocalNumberOfNodes  = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        NodeOffset = int(SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName='X_'//Name,   &
                DatasetDims     = (/GlobalNumberOfNodes/), &
                HyperSlabOffset = (/NodeOffset/),          &
                HyperSlabSize   = (/LocalNumberOfNodes/),  &
                Values          = X)
        call this%WriteHyperSlab(DatasetName='Y_'//Name,   &
                DatasetDims     = (/GlobalNumberOfNodes/), &
                HyperSlabOffset = (/NodeOffset/),          &
                HyperSlabSize   = (/LocalNumberOfNodes/),  &
                Values          = Y)
        call this%WriteHyperSlab(DatasetName='Z_'//Name,   &
                DatasetDims     = (/GlobalNumberOfNodes/), &
                HyperSlabOffset = (/NodeOffset/),          &
                HyperSlabSize   = (/LocalNumberOfNodes/),  &
                Values          = Z)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R4P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R8P),                                  intent(IN) :: X(:)                !< X Grid coordinates
        real(R8P),                                  intent(IN) :: Y(:)                !< Y Grid coordinates
        real(R8P),                                  intent(IN) :: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: GlobalNumberOfNodes !< Total number of nodes
        integer(HSIZE_T)                                       :: LocalNumberOfNodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: NodeOffset          !< Node offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        GlobalNumberOfNodes = int(SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        LocalNumberOfNodes  = int(SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        NodeOffset = int(SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName='X_'//Name,   &
                DatasetDims     = (/GlobalNumberOfNodes/), &
                HyperSlabOffset = (/NodeOffset/),          &
                HyperSlabSize   = (/LocalNumberOfNodes/),  &
                Values          = X)
        call this%WriteHyperSlab(DatasetName='Y_'//Name,   &
                DatasetDims     = (/GlobalNumberOfNodes/), &
                HyperSlabOffset = (/NodeOffset/),          &
                HyperSlabSize   = (/LocalNumberOfNodes/),  &
                Values          = Y)
        call this%WriteHyperSlab(DatasetName='Z_'//Name,   &
                DatasetDims     = (/GlobalNumberOfNodes/), &
                HyperSlabOffset = (/NodeOffset/),          &
                HyperSlabSize   = (/LocalNumberOfNodes/),  &
                Values          = Z)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R8P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
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
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R4P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
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
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R8P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Writes I4P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this      !< HDF5 contiguous hyperslab handler for Unstructured grids
        integer(I4P),                               intent(IN) :: Connectivities(:)      !< I4P Grid connectivities
        character(len=*),                           intent(IN) :: Name                   !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        call this%UniformGridDescriptor%SetTopologySize(int(size(connectivities,dim=1),I8P))
        call SpatialGridDescriptor%SetTopologySizePerGridID(int(size(connectivities,dim=1),I8P),ID=MPIEnvironment%get_rank())
        GlobalTopologySize = int(SpatialGridDescriptor%GetGlobalTopologySize(),HSIZE_T)
        LocalTopologySize = int(SpatialGridDescriptor%GetTopologySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        TopologySizeOffset = int(SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)

        call this%WriteHyperSlab(DatasetName=Name,                            &
                DatasetDims     = (/GlobalTopologySize/), &
                HyperSlabOffset = (/TopologySizeOffset/), &
                HyperSlabSize   = (/LocalTopologySize/),  &
                Values          = Connectivities)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteTopology_I4P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Writes I8P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this      !< HDF5 contiguous hyperslab handler for Unstructured grids
        integer(I8P),                               intent(IN) :: Connectivities(:)      !< I8P Grid connectivities
        character(len=*),                           intent(IN) :: Name                   !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        MPIEnvironment        => this%GetMPIEnvironment()
        SpatialGridDescriptor => this%GetSpatialGridDescriptor()
        assert(associated(SpatialGridDescriptor) .and. associated(MPIEnvironment))
        call this%UniformGridDescriptor%SetTopologySize(int(size(connectivities,dim=1),I8P))
        call SpatialGridDescriptor%SetTopologySizePerGridID(int(size(connectivities,dim=1),I8P),ID=MPIEnvironment%get_rank())
        GlobalTopologySize = int(SpatialGridDescriptor%GetGlobalTopologySize(),HSIZE_T)
        LocalTopologySize = int(SpatialGridDescriptor%GetTopologySizePerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        TopologySizeOffset = int(SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName=Name,                            &
                DatasetDims     = (/GlobalTopologySize/), &
                HyperSlabOffset = (/TopologySizeOffset/), &
                HyperSlabSize   = (/LocalTopologySize/),  &
                Values          = Connectivities)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteTopology_I8P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_XYZ_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Read XY[Z] R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R4P), allocatable,                     intent(OUT):: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
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
        call this%ReadHyperSlab(DatasetName = Name,                 &
                DatasetDims     = (/spacedim*globalnumberofnodes/), &
                HyperSlabOffset = (/spacedim*nodeoffset/),          &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),  &
                Values          = XYZ)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_XYZ_R4P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_XYZ_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Read XY[Z] R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R8P), allocatable,                     intent(OUT):: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
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
        call this%ReadHyperSlab(DatasetName = Name,                 &
                DatasetDims     = (/spacedim*globalnumberofnodes/), &
                HyperSlabOffset = (/spacedim*nodeoffset/),          &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),  &
                Values          = XYZ)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_XYZ_R8P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Read R4P X_Y_Z coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R4P), allocatable,                     intent(OUT):: X(:)                !< X Grid coordinates
        real(R4P), allocatable,                     intent(OUT):: Y(:)                !< Y Grid coordinates
        real(R4P), allocatable,                     intent(OUT):: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
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
        call this%ReadHyperSlab(DatasetName = 'X_'//Name,  &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = X)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
        call this%ReadHyperSlab(DatasetName = 'Y_'//Name,  &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = Y)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
        call this%ReadHyperSlab(DatasetName = 'Z_'//Name,  &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = Z)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R4P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Read X_Y_Z R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R8P), allocatable,                     intent(OUT):: X(:)                !< X Grid coordinates
        real(R8P), allocatable,                     intent(OUT):: Y(:)                !< Y Grid coordinates
        real(R8P), allocatable,                     intent(OUT):: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
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
        call this%ReadHyperSlab(DatasetName = 'X_'//Name,  &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = X)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
        call this%ReadHyperSlab(DatasetName = 'Y_'//Name,  &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = Y)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
        call this%ReadHyperSlab(DatasetName = 'Z_'//Name,  &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = Z)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R8P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Read R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
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
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R4P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Read R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
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
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R8P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read I4P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this      !< HDF5 contiguous hyperslab handler for Unstructured grids
        integer(I4P), allocatable,                  intent(OUT):: Connectivities(:)      !< I4P Grid connectivities
        character(len=*),                           intent(IN) :: Name                   !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
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
        call this%ReadHyperSlab(DatasetName = Name,           &
                DatasetDims     = (/GlobalTopologySize/), &
                HyperSlabOffset = (/TopologySizeOffset/), &
                HyperSlabSize   = (/LocalTopologySize/),  &
                Values          = Connectivities)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadTopology_I4P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read I8P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this      !< HDF5 contiguous hyperslab handler for Unstructured grids
        integer(I8P), allocatable,                  intent(OUT):: Connectivities(:)      !< I8P Grid connectivities
        character(len=*),                           intent(IN) :: Name                   !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
        class(mpi_env_t),                 pointer              :: MPIEnvironment        !< MPI Environment
        class(spatial_grid_descriptor_t), pointer              :: SpatialGridDescriptor !< Spatial grid descriptor
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
        call this%ReadHyperSlab(DatasetName = Name,           &
                DatasetDims     = (/GlobalTopologySize/), &
                HyperSlabOffset = (/TopologySizeOffset/), &
                HyperSlabSize   = (/LocalTopologySize/),  &
                Values          = Connectivities)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadTopology_I8P

end module hdf5_unstructured_contiguous_hyperslab_handler
