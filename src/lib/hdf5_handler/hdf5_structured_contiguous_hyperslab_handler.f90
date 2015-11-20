module hdf5_structured_contiguous_hyperslab_handler

use IR_Precision, only : I4P, I8P, R4P, R8P
#ifdef ENABLE_HDF5
use HDF5
#endif
use hdf5_contiguous_hyperslab_handler
use xh5for_utils

implicit none

private

    type, extends(hdf5_contiguous_hyperslab_handler_t) :: hdf5_structured_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< HDF5 contiguous hyperslab handler for structured grids
    !----------------------------------------------------------------- 
    contains
        procedure :: WriteGeometry_XYZ_R4P    => hdf5_structured_contiguous_hyperslab_WriteGeometry_XYZ_R4P
        procedure :: WriteGeometry_XYZ_R8P    => hdf5_structured_contiguous_hyperslab_WriteGeometry_XYZ_R8P
        procedure :: WriteGeometry_X_Y_Z_R4P  => hdf5_structured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R4P
        procedure :: WriteGeometry_X_Y_Z_R8P  => hdf5_structured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R8P
        procedure :: WriteGeometry_DXDYDZ_R4P => hdf5_structured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R4P
        procedure :: WriteGeometry_DXDYDZ_R8P => hdf5_structured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R8P
        procedure :: ReadGeometry_XYZ_R4P     => hdf5_structured_contiguous_hyperslab_ReadGeometry_XYZ_R4P
        procedure :: ReadGeometry_XYZ_R8P     => hdf5_structured_contiguous_hyperslab_ReadGeometry_XYZ_R8P
        procedure :: ReadGeometry_X_Y_Z_R4P   => hdf5_structured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R4P
        procedure :: ReadGeometry_DXDYDZ_R4P  => hdf5_structured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R4P
        procedure :: ReadGeometry_DXDYDZ_R8P  => hdf5_structured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R8P
        procedure :: ReadGeometry_X_Y_Z_R8P   => hdf5_structured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R8P
        procedure :: WriteTopology_I4P        => hdf5_structured_contiguous_hyperslab_WriteTopology_I4P
        procedure :: WriteTopology_I8P        => hdf5_structured_contiguous_hyperslab_WriteTopology_I8P
        procedure :: ReadTopology_I4P         => hdf5_structured_contiguous_hyperslab_ReadTopology_I4P
        procedure :: ReadTopology_I8P         => hdf5_structured_contiguous_hyperslab_ReadTopology_I8P
    end type hdf5_structured_contiguous_hyperslab_handler_t

public :: hdf5_structured_contiguous_hyperslab_handler_t

contains

    subroutine hdf5_structured_contiguous_hyperslab_ReadHyperSlab_R8P(this, DatasetName, DatasetDims, HyperSlabOffset, HyperSlabSize, Values)
    !-----------------------------------------------------------------
    !< read R8P dataset to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this      !< HDF5 contiguous hyperslab handler for structured grids
        character(len=*),                           intent(IN)  :: DatasetName         !< Dataset name
        integer(HSIZE_T),                           intent(IN)  :: DatasetDims(:)      !< Dataset dimensions
        integer(HSIZE_T),                           intent(IN)  :: HyperSlabOffset(:)  !< Hyperslab offset
        integer(HSIZE_T),                           intent(IN)  :: HyperSlabSize(:)    !< Hyperslab size
        real(R8P), allocatable,                     intent(OUT) :: Values(:)           !< R8P Dataset values
        integer(HID_T)                                          :: filespace           !< HDF5 file Dataspace identifier
        integer(HID_T)                                          :: memspace            !< HDF5 memory Dataspace identifier
        integer(HID_T)                                          :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                          :: dset_id             !< HDF5 Dataset identifier 
        integer                                                 :: hdferror            !< HDF5 error code
        integer                                                 :: rank                !< Hyperslab rank 
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        rank = 1
        allocate(Values(HyperSlabSize(rank)))
        ! Create filespace
        call H5Screate_simple_f(rank = rank,                  &
                dims     = DatasetDims,                       &
                space_id = filespace,                         &
                hdferr   = hdferror)
        ! Create the dataset with default properties.
        call H5Pcreate_f(H5P_DATASET_XFER_F, prp_id = plist_id, hdferr=hdferror) 
        ! Set MPIO data transfer mode to COLLECTIVE
        call H5Pset_dxpl_mpio_f(prp_id = plist_id, data_xfer_mode = H5FD_MPIO_COLLECTIVE_F, hdferr = hdferror)
        ! Open dataset 
        call H5Dopen_f(loc_id = this%file_id,               &
                name     = '/'//trim(adjustl(DatasetName)), &
                dset_id  = dset_id,                         & 
                hdferr   = hdferror)
        ! Select hyperslab
        call H5Sselect_hyperslab_f (space_id = filespace,   &
                operator = H5S_SELECT_SET_F,                &
                start    = HyperSlabOffset,                 &
                count    = HyperSlabSize,                   &
                hdferr   = hdferror)
        ! Create memspace
        call H5Screate_simple_f(rank = 1,                   &
                dims     = HyperSlabSize,                   &
                space_id = memspace,                        &
                hdferr   = hdferror) 
        ! Read data
        call H5Dread_f(dset_id = dset_id,           &
                mem_type_id   = H5T_NATIVE_DOUBLE,  &
                buf           = Values,             &
                dims          = HyperSlabSize,      &
                hdferr        = hdferror,           &
                file_space_id = filespace,          &
                mem_space_id  = memspace,           &
                xfer_prp      = plist_id)
        ! Close data space, dataset, property list .
        call H5Sclose_f(space_id = memspace,  hdferr = hdferror) 
        call H5Dclose_f(dset_id  = dset_id,   hdferr = hdferror)
        call H5Pclose_f(prp_id   = plist_id,  hdferr = hdferror)
        call H5Sclose_f(space_id = filespace, hdferr = hdferror)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_ReadHyperSlab_R8P


    subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_XYZ_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this     !< HDF5 contiguous hyperslab handler for structured grids
        real(R4P),                                  intent(IN) :: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: GlobalGeometrySize  !< Total size of the geometry dataset
        integer(HSIZE_T)                                       :: LocalGeometrySize   !< Local size of the geometry hyperslab
        integer(HSIZE_T)                                       :: GeometrySizeOffset  !< Geometry size offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        GlobalGeometrySize = int(this%SpatialGridDescriptor%GetGlobalGeometrySize(),HSIZE_T)
        LocalGeometrySize  = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        GeometrySizeOffset = int(this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName=Name,        &
                DatasetDims     = (/GlobalGeometrySize/), &
                HyperSlabOffset = (/GeometrySizeOffset/), &
                HyperSlabSize   = (/LocalGeometrySize/),  &
                Values          = XYZ)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_XYZ_R4P


    subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_XYZ_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this     !< HDF5 contiguous hyperslab handler for structured grids
        real(R8P),                                  intent(IN) :: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: GlobalGeometrySize  !< Total size of the geometry dataset
        integer(HSIZE_T)                                       :: LocalGeometrySize   !< Local size of the geometry hyperslab
        integer(HSIZE_T)                                       :: GeometrySizeOffset  !< Geometry size offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        GlobalGeometrySize = int(this%SpatialGridDescriptor%GetGlobalGeometrySize(),HSIZE_T)
        LocalGeometrySize  = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        GeometrySizeOffset = int(this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName=Name,        &
                DatasetDims     = (/GlobalGeometrySize/), &
                HyperSlabOffset = (/GeometrySizeOffset/), &
                HyperSlabSize   = (/LocalGeometrySize/),  &
                Values          = XYZ)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_XYZ_R8P


    subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this       !< HDF5 contiguous hyperslab handler for structured grids
        real(R4P),                                  intent(IN) :: X(:)                  !< X Grid coordinates
        real(R4P),                                  intent(IN) :: Y(:)                  !< Y Grid coordinates
        real(R4P),                                  intent(IN) :: Z(:)                  !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                  !< Geometry dataset name
        integer(HSIZE_T)                                       :: GlobalGeometrySize(3) !< Total number of nodes per axis
        integer(HSIZE_T)                                       :: LocalGeometrySize(3)  !< Total number of nodes per axis
        integer(HSIZE_T)                                       :: GeometrySizeOffset(3) !< Total number of nodes per axis
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        GlobalGeometrySize(1) = int(this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=1),HSIZE_T)
        GlobalGeometrySize(2) = int(this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=2),HSIZE_T)
        GlobalGeometrySize(3) = int(this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=3),HSIZE_T)
        LocalGeometrySize(1) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=1),HSIZE_T)
        LocalGeometrySize(2) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=2),HSIZE_T)
        LocalGeometrySize(3) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=3),HSIZE_T)
        GeometrySizeOffset(1) = int(this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=1),HSIZE_T)
        GeometrySizeOffset(2) = int(this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=2),HSIZE_T)
        GeometrySizeOffset(3) = int(this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=3),HSIZE_T)
        call this%WriteHyperSlab(DatasetName='X_'//Name,     &
                DatasetDims     = (/GlobalGeometrySize(1)/), &
                HyperSlabOffset = (/GeometrySizeOffSet(1)/), &
                HyperSlabSize   = (/LocalGeometrySize(1)/),  &
                Values          = X)
        call this%WriteHyperSlab(DatasetName='Y_'//Name,     &
                DatasetDims     = (/GlobalGeometrySize(2)/), &
                HyperSlabOffset = (/GeometrySizeOffSet(2)/), &
                HyperSlabSize   = (/LocalGeometrySize(2)/),  &
                Values          = Y)
        call this%WriteHyperSlab(DatasetName='Z_'//Name,     &
                DatasetDims     = (/GlobalGeometrySize(3)/), &
                HyperSlabOffset = (/GeometrySizeOffSet(3)/), &
                HyperSlabSize   = (/LocalGeometrySize(3)/),  &
                Values          = Z)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R4P


    subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this       !< HDF5 contiguous hyperslab handler for structured grids
        real(R8P),                                  intent(IN) :: X(:)                  !< X Grid coordinates
        real(R8P),                                  intent(IN) :: Y(:)                  !< Y Grid coordinates
        real(R8P),                                  intent(IN) :: Z(:)                  !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                  !< Geometry dataset name
        integer(HSIZE_T)                                       :: GlobalGeometrySize(3) !< Total number of nodes per axis
        integer(HSIZE_T)                                       :: LocalGeometrySize(3)  !< Total number of nodes per axis
        integer(HSIZE_T)                                       :: GeometrySizeOffset(3) !< Total number of nodes per axis
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        GlobalGeometrySize(1) = int(this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=1),HSIZE_T)
        GlobalGeometrySize(2) = int(this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=2),HSIZE_T)
        GlobalGeometrySize(3) = int(this%SpatialGridDescriptor%GetGlobalGeometrySize(Dimension=3),HSIZE_T)
        LocalGeometrySize(1) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=1),HSIZE_T)
        LocalGeometrySize(2) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=2),HSIZE_T)
        LocalGeometrySize(3) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=3),HSIZE_T)
        GeometrySizeOffset(1) = int(this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=1),HSIZE_T)
        GeometrySizeOffset(2) = int(this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=2),HSIZE_T)
        GeometrySizeOffset(3) = int(this%SpatialGridDescriptor%GetGeometrySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=3),HSIZE_T)
        call this%WriteHyperSlab(DatasetName='X_'//Name,     &
                DatasetDims     = (/GlobalGeometrySize(1)/), &
                HyperSlabOffset = (/GeometrySizeOffSet(1)/), &
                HyperSlabSize   = (/LocalGeometrySize(1)/),  &
                Values          = X)
        call this%WriteHyperSlab(DatasetName='Y_'//Name,     &
                DatasetDims     = (/GlobalGeometrySize(2)/), &
                HyperSlabOffset = (/GeometrySizeOffSet(2)/), &
                HyperSlabSize   = (/LocalGeometrySize(2)/),  &
                Values          = Y)
        call this%WriteHyperSlab(DatasetName='Z_'//Name,     &
                DatasetDims     = (/GlobalGeometrySize(3)/), &
                HyperSlabOffset = (/GeometrySizeOffSet(3)/), &
                HyperSlabSize   = (/LocalGeometrySize(3)/),  &
                Values          = Z)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_X_Y_Z_R8P


    subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this       !< HDF5 contiguous hyperslab handler for structured grids
        real(R4P),                                  intent(IN) :: Origin(:)             !< Origin coordinates
        real(R4P),                                  intent(IN) :: DxDyDz(:)             !< Coodinates step for the next point
        character(len=*),                           intent(IN) :: Name                  !< Geometry dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        select case (this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank()))
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                ! Origin and DxDyDz size must be 3
                call this%WriteHyperSlab(DatasetName='Origin_'//Name,     &
                        DatasetDims     = (/3_I8P*int(this%MPIEnvironment%get_comm_size(),I8P)/), &
                        HyperSlabOffset = (/3_I8P*int(this%MPIEnvironment%get_rank(),I8P)/), &
                        HyperSlabSize   = (/3_I8P/),  &
                        Values          = Origin(3:1:-1))
                call this%WriteHyperSlab(DatasetName='DxDyDz_'//Name,     &
                        DatasetDims     = (/3_I8P*int(this%MPIEnvironment%get_comm_size(),I8P)/), &
                        HyperSlabOffset = (/3_I8P*int(this%MPIEnvironment%get_rank(),I8P)/), &
                        HyperSlabSize   = (/3_I8P/),  &
                        Values          = DxDyDz(3:1:-1))
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                ! Origin and DxDyDz size must be 2
                call this%WriteHyperSlab(DatasetName='Origin_'//Name,     &
                        DatasetDims     = (/2_I8P*int(this%MPIEnvironment%get_comm_size(),I8P)/), &
                        HyperSlabOffset = (/2_I8P*int(this%MPIEnvironment%get_rank(),I8P)/), &
                        HyperSlabSize   = (/2_I8P/),  &
                        Values          = Origin(2:1:-1))
                call this%WriteHyperSlab(DatasetName='DxDyDz_'//Name,     &
                        DatasetDims     = (/2_I8P*int(this%MPIEnvironment%get_comm_size(),I8P)/), &
                        HyperSlabOffset = (/2_I8P*int(this%MPIEnvironment%get_rank(),I8P)/), &
                        HyperSlabSize   = (/2_I8P/),  &
                        Values          = DxDyDz(2:1:-1))
        end select
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R4P


    subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this       !< HDF5 contiguous hyperslab handler for structured grids
        real(R8P),                                  intent(IN) :: Origin(:)             !< Origin coordinates
        real(R8P),                                  intent(IN) :: DxDyDz(:)             !< Coodinates step for the next point
        character(len=*),                           intent(IN) :: Name                  !< Geometry dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        select case (this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank()))
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                ! Origin and DxDyDz size must be 3
                call this%WriteHyperSlab(DatasetName='Origin_'//Name,     &
                        DatasetDims     = (/3_I8P*int(this%MPIEnvironment%get_comm_size(),I8P)/), &
                        HyperSlabOffset = (/3_I8P*int(this%MPIEnvironment%get_rank(),I8P)/), &
                        HyperSlabSize   = (/3_I8P/),  &
                        Values          = Origin(3:1:-1))
                call this%WriteHyperSlab(DatasetName='DxDyDz_'//Name,     &
                        DatasetDims     = (/3_I8P*int(this%MPIEnvironment%get_comm_size(),I8P)/), &
                        HyperSlabOffset = (/3_I8P*int(this%MPIEnvironment%get_rank(),I8P)/), &
                        HyperSlabSize   = (/3_I8P/),  &
                        Values          = DxDyDz(3:1:-1))
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                ! Origin and DxDyDz size must be 2
                call this%WriteHyperSlab(DatasetName='Origin_'//Name,     &
                        DatasetDims     = (/2_I8P*int(this%MPIEnvironment%get_comm_size(),I8P)/), &
                        HyperSlabOffset = (/2_I8P*int(this%MPIEnvironment%get_rank(),I8P)/), &
                        HyperSlabSize   = (/2_I8P/),  &
                        Values          = Origin(2:1:-1))
                call this%WriteHyperSlab(DatasetName='DxDyDz_'//Name,     &
                        DatasetDims     = (/2_I8P*int(this%MPIEnvironment%get_comm_size(),I8P)/), &
                        HyperSlabOffset = (/2_I8P*int(this%MPIEnvironment%get_rank(),I8P)/), &
                        HyperSlabSize   = (/2_I8P/),  &
                        Values          = DxDyDz(2:1:-1))
        end select
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_WriteGeometry_DXDYDZ_R8P


    subroutine hdf5_structured_contiguous_hyperslab_WriteTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Writes I4P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this    !< HDF5 contiguous hyperslab handler for structured grids
        integer(I4P),                               intent(IN) :: Connectivities(:)  !< I4P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! No topology data is written into HDF5 file
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_WriteTopology_I4P


    subroutine hdf5_structured_contiguous_hyperslab_WriteTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Writes I8P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this    !< HDF5 contiguous hyperslab handler for structured grids
        integer(I8P),                               intent(IN) :: Connectivities(:)  !< I8P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! No topology data is written into HDF5 file
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_WriteTopology_I8P


    subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_XYZ_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Read XY[Z] R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this     !< HDF5 contiguous hyperslab handler for structured grids
        real(R4P), allocatable,                     intent(OUT):: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%ReadHyperSlab(DatasetName = Name,                 &
                DatasetDims     = (/spacedim*globalnumberofnodes/), &
                HyperSlabOffset = (/spacedim*nodeoffset/),          &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),  &
                Values          = XYZ)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_XYZ_R4P


    subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_XYZ_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Read XY[Z] R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this     !< HDF5 contiguous hyperslab handler for structured grids
        real(R8P), allocatable,                     intent(OUT):: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%ReadHyperSlab(DatasetName = Name,                 &
                DatasetDims     = (/spacedim*globalnumberofnodes/), &
                HyperSlabOffset = (/spacedim*nodeoffset/),          &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),  &
                Values          = XYZ)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_XYZ_R8P


    subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Read R4P X_Y_Z coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this     !< HDF5 contiguous hyperslab handler for structured grids
        real(R4P), allocatable,                     intent(OUT):: X(:)                !< X Grid coordinates
        real(R4P), allocatable,                     intent(OUT):: Y(:)                !< Y Grid coordinates
        real(R4P), allocatable,                     intent(OUT):: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
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
    end subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R4P


    subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Read X_Y_Z R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this     !< HDF5 contiguous hyperslab handler for structured grids
        real(R8P), allocatable,                     intent(OUT):: X(:)                !< X Grid coordinates
        real(R8P), allocatable,                     intent(OUT):: Y(:)                !< Y Grid coordinates
        real(R8P), allocatable,                     intent(OUT):: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%SpatialGridDescriptor%GetNumberOfNodesPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
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
    end subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_X_Y_Z_R8P


    subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Read R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this  !< HDF5 contiguous hyperslab handler for structured grids
        real(R4P), allocatable,                     intent(OUT) :: Origin(:)       !< Origin coordinates
        real(R4P), allocatable,                     intent(OUT) :: DxDyDz(:)       !< Coodinates step for the next point
        character(len=*),                           intent(IN)  :: Name            !< Geometry dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        select case (this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank()))
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                ! Origin and DxDyDz size must be 3
                call this%ReadHyperSlab(DatasetName='Origin_'//Name,     &
                        DatasetDims     = (/3_HSIZE_T*int(this%MPIEnvironment%get_comm_size(),HSIZE_T)/), &
                        HyperSlabOffset = (/3_HSIZE_T*int(this%MPIEnvironment%get_rank(),HSIZE_T)/), &
                        HyperSlabSize   = (/3_HSIZE_T/),  &
                        Values          = Origin)
                call this%ReadHyperSlab(DatasetName='DxDyDz_'//Name,     &
                        DatasetDims     = (/3_HSIZE_T*int(this%MPIEnvironment%get_comm_size(),HSIZE_T)/), &
                        HyperSlabOffset = (/3_HSIZE_T*int(this%MPIEnvironment%get_rank(),HSIZE_T)/), &
                        HyperSlabSize   = (/3_HSIZE_T/),  &
                        Values          = DxDyDz)
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                ! Origin and DxDyDz size must be 2
                call this%ReadHyperSlab(DatasetName='Origin_'//Name,     &
                        DatasetDims     = (/2_HSIZE_T*int(this%MPIEnvironment%get_comm_size(),HSIZE_T)/), &
                        HyperSlabOffset = (/2_HSIZE_T*int(this%MPIEnvironment%get_rank(),HSIZE_T)/), &
                        HyperSlabSize   = (/2_HSIZE_T/),  &
                        Values          = Origin)
                call this%ReadHyperSlab(DatasetName='DxDyDz_'//Name,     &
                        DatasetDims     = (/2_HSIZE_T*int(this%MPIEnvironment%get_comm_size(),HSIZE_T)/), &
                        HyperSlabOffset = (/2_HSIZE_T*int(this%MPIEnvironment%get_rank(),HSIZE_T)/), &
                        HyperSlabSize   = (/2_HSIZE_T/),  &
                        Values          = DxDyDz)
        end select
        Origin(:) = Origin(size(Origin,dim=1):1:-1)
        DxDyDz(:) = DxDyDz(size(DxDyDz,dim=1):1:-1)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R4P


    subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Read R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this  !< HDF5 contiguous hyperslab handler for structured grids
        real(R8P), allocatable,                     intent(OUT) :: Origin(:)       !< Origin coordinates
        real(R8P), allocatable,                     intent(OUT) :: DxDyDz(:)       !< Coodinates step for the next point
        character(len=*),                           intent(IN)  :: Name            !< Geometry dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        select case (this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank()))
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                ! Origin and DxDyDz size must be 3
                call this%ReadHyperSlab(DatasetName='Origin_'//Name,     &
                        DatasetDims     = (/3_HSIZE_T*int(this%MPIEnvironment%get_comm_size(),HSIZE_T)/), &
                        HyperSlabOffset = (/3_HSIZE_T*int(this%MPIEnvironment%get_rank(),HSIZE_T)/), &
                        HyperSlabSize   = (/3_HSIZE_T/),  &
                        Values          = Origin)
                call this%ReadHyperSlab(DatasetName='DxDyDz_'//Name,     &
                        DatasetDims     = (/3_HSIZE_T*int(this%MPIEnvironment%get_comm_size(),HSIZE_T)/), &
                        HyperSlabOffset = (/3_HSIZE_T*int(this%MPIEnvironment%get_rank(),HSIZE_T)/), &
                        HyperSlabSize   = (/3_HSIZE_T/),  &
                        Values          = DxDyDz)
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                ! Origin and DxDyDz size must be 2
                call this%ReadHyperSlab(DatasetName='Origin_'//Name,     &
                        DatasetDims     = (/2_HSIZE_T*int(this%MPIEnvironment%get_comm_size(),HSIZE_T)/), &
                        HyperSlabOffset = (/2_HSIZE_T*int(this%MPIEnvironment%get_rank(),HSIZE_T)/), &
                        HyperSlabSize   = (/2_HSIZE_T/),  &
                        Values          = Origin)
                call this%ReadHyperSlab(DatasetName='DxDyDz_'//Name,     &
                        DatasetDims     = (/2_HSIZE_T*int(this%MPIEnvironment%get_comm_size(),HSIZE_T)/), &
                        HyperSlabOffset = (/2_HSIZE_T*int(this%MPIEnvironment%get_rank(),HSIZE_T)/), &
                        HyperSlabSize   = (/2_HSIZE_T/),  &
                        Values          = DxDyDz)
        end select
        Origin(:) = Origin(size(Origin,dim=1):1:-1)
        DxDyDz(:) = DxDyDz(size(DxDyDz,dim=1):1:-1)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_ReadGeometry_DXDYDZ_R8P


    subroutine hdf5_structured_contiguous_hyperslab_ReadTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read I4P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this    !< HDF5 contiguous hyperslab handler for structured grids
        integer(I4P), allocatable,                  intent(OUT):: Connectivities(:)  !< I4P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        GlobalTopologySize = int(this%SpatialGridDescriptor%GetGlobalTopologySize(),HSIZE_T)
        LocalTopologySize  =  int(this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        TopologySizeOffset = int(this%SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%ReadHyperSlab(DatasetName = Name,           &
                DatasetDims     = (/GlobalTopologySize/), &
                HyperSlabOffset = (/TopologySizeOffset/), &
                HyperSlabSize   = (/LocalTopologySize/),  &
                Values          = Connectivities)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_ReadTopology_I4P


    subroutine hdf5_structured_contiguous_hyperslab_ReadTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read I8P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_contiguous_hyperslab_handler_t), intent(IN) :: this    !< HDF5 contiguous hyperslab handler for structured grids
        integer(I8P), allocatable,                  intent(OUT):: Connectivities(:)  !< I8P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
        integer(HSIZE_T)                                       :: GlobalTopologySize !< Global size of connectivities
        integer(HSIZE_T)                                       :: LocalTopologySize  !< Local size of connectivities for a particular grid
        integer(HSIZE_T)                                       :: TopologySizeOffset !< Connectivity Size offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        GlobalTopologySize = int(this%SpatialGridDescriptor%GetGlobalTopologySize(),HSIZE_T)
        LocalTopologySize =  int(this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        TopologySizeOffset = int(this%SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%ReadHyperSlab(DatasetName = Name,           &
                DatasetDims     = (/GlobalTopologySize/), &
                HyperSlabOffset = (/TopologySizeOffset/), &
                HyperSlabSize   = (/LocalTopologySize/),  &
                Values          = Connectivities)
#endif
    end subroutine hdf5_structured_contiguous_hyperslab_ReadTopology_I8P


end module hdf5_structured_contiguous_hyperslab_handler
