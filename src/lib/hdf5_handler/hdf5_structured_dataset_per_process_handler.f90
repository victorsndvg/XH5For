module hdf5_structured_dataset_per_process_handler

use IR_Precision, only : I4P, I8P, R4P, R8P
#ifdef ENABLE_HDF5
use HDF5
#endif
use hdf5_dataset_per_process_handler
use xh5for_utils

implicit none

private

    type, extends(hdf5_dataset_per_process_handler_t) :: hdf5_structured_dataset_per_process_handler_t
    !-----------------------------------------------------------------
    !< HDF5 dataset per process handler for structured grids
    !----------------------------------------------------------------- 
    contains
        procedure :: WriteGeometry_XYZ_R4P    => hdf5_structured_dataset_per_process_WriteGeometry_XYZ_R4P
        procedure :: WriteGeometry_XYZ_R8P    => hdf5_structured_dataset_per_process_WriteGeometry_XYZ_R8P
        procedure :: WriteGeometry_X_Y_Z_R4P  => hdf5_structured_dataset_per_process_WriteGeometry_X_Y_Z_R4P
        procedure :: WriteGeometry_X_Y_Z_R8P  => hdf5_structured_dataset_per_process_WriteGeometry_X_Y_Z_R8P
        procedure :: WriteGeometry_DXDYDZ_R4P => hdf5_structured_dataset_per_process_WriteGeometry_DXDYDZ_R4P
        procedure :: WriteGeometry_DXDYDZ_R8P => hdf5_structured_dataset_per_process_WriteGeometry_DXDYDZ_R8P
        procedure :: ReadGeometry_XYZ_R4P     => hdf5_structured_dataset_per_process_ReadGeometry_XYZ_R4P
        procedure :: ReadGeometry_XYZ_R8P     => hdf5_structured_dataset_per_process_ReadGeometry_XYZ_R8P
        procedure :: ReadGeometry_X_Y_Z_R4P   => hdf5_structured_dataset_per_process_ReadGeometry_X_Y_Z_R4P
        procedure :: ReadGeometry_DXDYDZ_R4P  => hdf5_structured_dataset_per_process_ReadGeometry_DXDYDZ_R4P
        procedure :: ReadGeometry_DXDYDZ_R8P  => hdf5_structured_dataset_per_process_ReadGeometry_DXDYDZ_R8P
        procedure :: ReadGeometry_X_Y_Z_R8P   => hdf5_structured_dataset_per_process_ReadGeometry_X_Y_Z_R8P
        procedure :: WriteTopology_I4P        => hdf5_structured_dataset_per_process_WriteTopology_I4P
        procedure :: WriteTopology_I8P        => hdf5_structured_dataset_per_process_WriteTopology_I8P
        procedure :: ReadTopology_I4P         => hdf5_structured_dataset_per_process_ReadTopology_I4P
        procedure :: ReadTopology_I8P         => hdf5_structured_dataset_per_process_ReadTopology_I8P
    end type hdf5_structured_dataset_per_process_handler_t

public :: hdf5_structured_dataset_per_process_handler_t

contains

    subroutine hdf5_structured_dataset_per_process_WriteGeometry_XYZ_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this      !< HDF5 dataset per process handler for Unstructured grids
        real(R4P),                                  intent(IN) :: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: LocalGeometrySize   !< Local size of the geometry hyperslab
        integer(I4P)                                           :: GridID              !< Index to loop on GridID's
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        do GridID=0, this%MPIEnvironment%get_comm_size()-1
            LocalGeometrySize  = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID),HSIZE_T)
            call this%WriteMetadata(                                                           &
                    DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalGeometrySize/),                                   &
                    HyperSlabOffset = (/0_HSIZE_T/),                                           &
                    HyperSlabSize   = (/LocalGeometrySize/),                                   &
                    Values          = XYZ)
        enddo
        LocalGeometrySize  = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteData(                                                                                       &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalGeometrySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalGeometrySize/),                                                           &
                Values          = XYZ)
#endif
    end subroutine hdf5_structured_dataset_per_process_WriteGeometry_XYZ_R4P


    subroutine hdf5_structured_dataset_per_process_WriteGeometry_XYZ_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this      !< HDF5 dataset per process handler for structured grids
        real(R8P),                                  intent(IN) :: XYZ(:)              !< Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: LocalGeometrySize   !< Local size of the geometry hyperslab
        integer(I4P)                                           :: GridID              !< Index to loop on GridID's
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        do GridID=0, this%MPIEnvironment%get_comm_size()-1
            LocalGeometrySize  = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID),HSIZE_T)
            call this%WriteMetadata(                                                           &
                    DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalGeometrySize/),                                   &
                    HyperSlabOffset = (/0_HSIZE_T/),                                           &
                    HyperSlabSize   = (/LocalGeometrySize/),                                   &
                    Values          = XYZ)
        enddo
        LocalGeometrySize  = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteData(                                                                                       &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalGeometrySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalGeometrySize/),                                                           &
                Values          = XYZ)
#endif
    end subroutine hdf5_structured_dataset_per_process_WriteGeometry_XYZ_R8P


    subroutine hdf5_structured_dataset_per_process_WriteGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this        !< HDF5 dataset per process handler for structured grids
        real(R4P),                                  intent(IN) :: X(:)                  !< X Grid coordinates
        real(R4P),                                  intent(IN) :: Y(:)                  !< Y Grid coordinates
        real(R4P),                                  intent(IN) :: Z(:)                  !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                  !< Geometry dataset name
        integer(HSIZE_T)                                       :: LocalGeometrySize(3)  !< Total number of nodes per axis
        integer(I4P)                                           :: SpaceDimension        !< Space dimension
        integer(I4P)                                           :: GridID                !< Index to loop on GridID's
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        do GridID=0, this%MPIEnvironment%get_comm_size()-1
            LocalGeometrySize(1) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=1),HSIZE_T)
            LocalGeometrySize(2) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=2),HSIZE_T)
            LocalGeometrySize(3) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=3),HSIZE_T)
            SpaceDimension = GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=GridID))
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalGeometrySize(1)/),                                      &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalGeometrySize(1)/),                                      &
                    Values          = X)
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalGeometrySize(2)/),                                      &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalGeometrySize(2)/),                                      &
                    Values          = Y)
            if(SpaceDimension == 3) then
                call this%WriteMetadata(                                                                 &
                        DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                        DatasetDims     = (/LocalGeometrySize(3)/),                                      &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                        HyperSlabSize   = (/LocalGeometrySize(3)/),                                      &
                        Values          = Z)
            endif
        enddo
        LocalGeometrySize(1) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=1),HSIZE_T)
        LocalGeometrySize(2) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=2),HSIZE_T)
        LocalGeometrySize(3) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=3),HSIZE_T)
        SpaceDimension = GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank()))
        call this%WriteData(                                                                                         &
                DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalGeometrySize(1)/),                                                              &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalGeometrySize(1)/),                                                              &
                Values          = X)
        call this%WriteData(                                                                                         &
                DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalGeometrySize(2)/),                                                              &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalGeometrySize(2)/),                                                              &
                Values          = Y)
        if(SpaceDimension == 3) then
            call this%WriteData(                                                                                         &
                    DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                    DatasetDims     = (/LocalGeometrySize(3)/),                                                              &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                    HyperSlabSize   = (/LocalGeometrySize(3)/),                                                              &
                    Values          = Z)
        endif
#endif
    end subroutine hdf5_structured_dataset_per_process_WriteGeometry_X_Y_Z_R4P


    subroutine hdf5_structured_dataset_per_process_WriteGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this        !< HDF5 dataset per process handler for structured grids
        real(R8P),                                  intent(IN) :: X(:)                  !< X Grid coordinates
        real(R8P),                                  intent(IN) :: Y(:)                  !< Y Grid coordinates
        real(R8P),                                  intent(IN) :: Z(:)                  !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                  !< Geometry dataset name
        integer(HSIZE_T)                                       :: LocalGeometrySize(3)  !< Total number of nodes per axis
        integer(I4P)                                           :: SpaceDimension        !< Space dimension
        integer(I4P)                                           :: GridID                !< Index to loop on GridID's
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        do GridID=0, this%MPIEnvironment%get_comm_size()-1
            LocalGeometrySize(1) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=1),HSIZE_T)
            LocalGeometrySize(2) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=2),HSIZE_T)
            LocalGeometrySize(3) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=GridID, Dimension=3),HSIZE_T)
            SpaceDimension = GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=GridID))
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalGeometrySize(1)/),                                      &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalGeometrySize(1)/),                                      &
                    Values          = X)
            call this%WriteMetadata(                                                                 &
                    DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                    DatasetDims     = (/LocalGeometrySize(2)/),                                      &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                    HyperSlabSize   = (/LocalGeometrySize(2)/),                                      &
                    Values          = Y)
            if(SpaceDimension == 3) then
                call this%WriteMetadata(                                                                 &
                        DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                        DatasetDims     = (/LocalGeometrySize(3)/),                                      &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                 &
                        HyperSlabSize   = (/LocalGeometrySize(3)/),                                      &
                        Values          = Z)
            endif
        enddo
        LocalGeometrySize(1) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=1),HSIZE_T)
        LocalGeometrySize(2) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=2),HSIZE_T)
        LocalGeometrySize(3) = int(this%SpatialGridDescriptor%GetGeometrySizePerGridID(ID=this%MPIEnvironment%get_rank(), Dimension=3),HSIZE_T)
        SpaceDimension = GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank()))
        call this%WriteData(                                                                                         &
                DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalGeometrySize(1)/),                                                              &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalGeometrySize(1)/),                                                              &
                Values          = X)
        call this%WriteData(                                                                                         &
                DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalGeometrySize(2)/),                                                              &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/LocalGeometrySize(2)/),                                                              &
                Values          = Y)
        if(SpaceDimension == 3) then
            call this%WriteData(                                                                                         &
                    DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                    DatasetDims     = (/LocalGeometrySize(3)/),                                                              &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                    HyperSlabSize   = (/LocalGeometrySize(3)/),                                                              &
                    Values          = Z)
        endif
#endif
    end subroutine hdf5_structured_dataset_per_process_WriteGeometry_X_Y_Z_R8P


    subroutine hdf5_structured_dataset_per_process_WriteGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this        !< HDF5 dataset per process handler for structured grids
        real(R4P),                                  intent(IN) :: Origin(:)             !< Origin coordinates
        real(R4P),                                  intent(IN) :: DxDyDz(:)             !< Coodinates step for the next point
        character(len=*),                           intent(IN) :: Name                  !< Geometry dataset name
        integer(I4P)                                           :: GridID                !< Index to loop on GridID's
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        select case (this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank()))
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                ! Origin and DxDyDz size must be 3
                do GridID=0, this%MPIEnvironment%get_comm_size()-1
                    call this%WriteMetadata(                                                                      &
                            DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                            DatasetDims     = (/3_I8P/),                                                          &
                            HyperSlabOffset = (/0_HSIZE_T/),                                                      &
                            HyperSlabSize   = (/3_I8P/),                                                          &
                            Values          = Origin(3:1:-1))
                    call this%WriteMetadata(                                                                      &
                            DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                            DatasetDims     = (/3_I8P/),                                                          &
                            HyperSlabOffset = (/0_HSIZE_T/),                                                      &
                            HyperSlabSize   = (/3_I8P/),                                                          &
                            Values          = DxDyDz(3:1:-1))
                enddo
                call this%WriteData(                                                                                             &
                        DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/3_I8P/),                                                                                  &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/3_I8P/),                                                                                  &
                        Values          = Origin(3:1:-1))
                call this%WriteData(                                                                                             &
                        DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/3_I8P/),                                                                                  &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/3_I8P/),                                                                                  &
                        Values          = DxDyDz(3:1:-1))

            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                ! Origin and DxDyDz size must be 2
                do GridID=0, this%MPIEnvironment%get_comm_size()-1
                    call this%WriteMetadata(                                                                      &
                            DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                            DatasetDims     = (/2_I8P/),                                                          &
                            HyperSlabOffset = (/0_HSIZE_T/),                                                      &
                            HyperSlabSize   = (/2_I8P/),                                                          &
                            Values          = Origin(2:1:-1))
                    call this%WriteMetadata(                                                                      &
                            DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                            DatasetDims     = (/2_I8P/),                                                          &
                            HyperSlabOffset = (/0_HSIZE_T/),                                                      &
                            HyperSlabSize   = (/2_I8P/),                                                          &
                            Values          = DxDyDz(2:1:-1))
                enddo
                call this%WriteData(                                                                                              &
                        DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/2_I8P/),                                                                                  &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/2_I8P/),                                                                                  &
                        Values          = Origin(2:1:-1))
                call this%WriteData(                                                                                              &
                        DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/2_I8P/),                                                                                  &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/2_I8P/),                                                                                  &
                        Values          = DxDyDz(2:1:-1))
        end select
#endif
    end subroutine hdf5_structured_dataset_per_process_WriteGeometry_DXDYDZ_R4P


    subroutine hdf5_structured_dataset_per_process_WriteGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this        !< HDF5 dataset per process handler for structured grids
        real(R8P),                                  intent(IN) :: Origin(:)             !< Origin coordinates
        real(R8P),                                  intent(IN) :: DxDyDz(:)             !< Coodinates step for the next point
        character(len=*),                           intent(IN) :: Name                  !< Geometry dataset name
        integer(I4P)                                           :: GridID                !< Index to loop on GridID's
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        select case (this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank()))
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDYDZ)
                ! Origin and DxDyDz size must be 3
                do GridID=0, this%MPIEnvironment%get_comm_size()-1
                    call this%WriteMetadata(                                                                      &
                            DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                            DatasetDims     = (/3_I8P/),                                                          &
                            HyperSlabOffset = (/0_HSIZE_T/),                                                      &
                            HyperSlabSize   = (/3_I8P/),                                                          &
                            Values          = Origin(3:1:-1))
                    call this%WriteMetadata(                                                                      &
                            DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                            DatasetDims     = (/3_I8P/),                                                          &
                            HyperSlabOffset = (/0_HSIZE_T/),                                                      &
                            HyperSlabSize   = (/3_I8P/),                                                          &
                            Values          = DxDyDz(3:1:-1))
                enddo
                call this%WriteData(                                                                                             &
                        DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/3_I8P/),                                                                                  &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/3_I8P/),                                                                                  &
                        Values          = Origin(3:1:-1))
                call this%WriteData(                                                                                             &
                        DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/3_I8P/),                                                                                  &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/3_I8P/),                                                                                  &
                        Values          = DxDyDz(3:1:-1))

            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                ! Origin and DxDyDz size must be 2
                do GridID=0, this%MPIEnvironment%get_comm_size()-1
                    call this%WriteMetadata(                                                                      &
                            DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                            DatasetDims     = (/2_I8P/),                                                          &
                            HyperSlabOffset = (/0_HSIZE_T/),                                                      &
                            HyperSlabSize   = (/2_I8P/),                                                          &
                            Values          = Origin(2:1:-1))
                    call this%WriteMetadata(                                                                      &
                            DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=GridID))),  &
                            DatasetDims     = (/2_I8P/),                                                          &
                            HyperSlabOffset = (/0_HSIZE_T/),                                                      &
                            HyperSlabSize   = (/2_I8P/),                                                          &
                            Values          = DxDyDz(2:1:-1))
                enddo
                call this%WriteData(                                                                                              &
                        DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/2_I8P/),                                                                                  &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/2_I8P/),                                                                                  &
                        Values          = Origin(2:1:-1))
                call this%WriteData(                                                                                              &
                        DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/2_I8P/),                                                                                  &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/2_I8P/),                                                                                  &
                        Values          = DxDyDz(2:1:-1))
        end select
#endif
    end subroutine hdf5_structured_dataset_per_process_WriteGeometry_DXDYDZ_R8P


    subroutine hdf5_structured_dataset_per_process_WriteTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Writes I4P connectivities to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this     !< HDF5 dataset per process handler for structured grids
        integer(I4P),                               intent(IN) :: Connectivities(:)  !< I4P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! No topology data is written into HDF5 file
#endif
    end subroutine hdf5_structured_dataset_per_process_WriteTopology_I4P


    subroutine hdf5_structured_dataset_per_process_WriteTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Writes I8P connectivities to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this     !< HDF5 dataset per process handler for structured grids
        integer(I8P),                               intent(IN) :: Connectivities(:)  !< I8P Grid connectivities
        character(len=*),                           intent(IN) :: Name               !< Topology dataset name
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! No topology data is written into HDF5 file
#endif
    end subroutine hdf5_structured_dataset_per_process_WriteTopology_I8P


    subroutine hdf5_structured_dataset_per_process_ReadGeometry_XYZ_R4P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Read XY[Z] R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this      !< HDF5 dataset per process handler for structured grids
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
        call this%ReadHyperSlab(                                                                                   &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/spacedim*localnumberofnodes/),                                                 &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),                                                 &
                Values          = XYZ)
#endif
    end subroutine hdf5_structured_dataset_per_process_ReadGeometry_XYZ_R4P


    subroutine hdf5_structured_dataset_per_process_ReadGeometry_XYZ_R8P(this, XYZ, Name)
    !-----------------------------------------------------------------
    !< Read XY[Z] R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this      !< HDF5 dataset per process handler for structured grids
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
        call this%ReadHyperSlab(                                                                                   &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/spacedim*localnumberofnodes/),                                                 &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),                                                 &
                Values          = XYZ)
#endif
    end subroutine hdf5_structured_dataset_per_process_ReadGeometry_XYZ_R8P


    subroutine hdf5_structured_dataset_per_process_ReadGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Read R4P X_Y_Z coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this        !< HDF5 dataset per process handler for structured grids
        real(R4P), allocatable,                     intent(OUT):: X(:)                  !< X Grid coordinates
        real(R4P), allocatable,                     intent(OUT):: Y(:)                  !< Y Grid coordinates
        real(R4P), allocatable,                     intent(OUT):: Z(:)                  !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                  !< Geometry dataset name
        integer(HSIZE_T)                                       :: spacedim              !< Space dimension
        integer(HSIZE_T)                                       :: globalnodesperdim(3)  !< Global number of nodes per dimension
        integer(HSIZE_T)                                       :: localnodesperdim(3)   !< Local number of nodes per dimension
        integer(HSIZE_T)                                       :: nodeoffsetperdim(3)   !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnodesperdim(1) = int(this%SpatialGridDescriptor%GetGlobalXsize(),HSIZE_T)
        globalnodesperdim(2) = int(this%SpatialGridDescriptor%GetGlobalYsize(),HSIZE_T)
        localnodesperdim(1)  = int(this%SpatialGridDescriptor%GetXSizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        localnodesperdim(2)  = int(this%SpatialGridDescriptor%GetYSizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffsetperdim(1) = int(this%SpatialGridDescriptor%GetXSizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffsetperdim(2) = int(this%SpatialGridDescriptor%GetYSizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
        call this%ReadHyperSlab(                                                                                         &
                DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnodesperdim(1)/),                                                               &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnodesperdim(1)/),                                                               &
                Values          = X)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
        call this%ReadHyperSlab(                                                                                         &
                DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnodesperdim(2)/),                                                               &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnodesperdim(2)/),                                                               &
                Values          = Y)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
        if(spacedim == 3) then
            globalnodesperdim(3) = int(this%SpatialGridDescriptor%GetGlobalZsize(),HSIZE_T)
            localnodesperdim(3)  = int(this%SpatialGridDescriptor%GetZSizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
            nodeoffsetperdim(3)  = int(this%SpatialGridDescriptor%GetZSizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
            call this%ReadHyperSlab(                                                                                         &
                    DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                    DatasetDims     = (/LOCalnodesperdim(3)/),                                                               &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                    HyperSlabSize   = (/localnodesperdim(3)/),                                                               &
                    Values          = Z)
        else
            if(allocated(Z)) deallocate(Z); allocate(Z(1)); Z = 0.0_R4P
        endif
#endif
    end subroutine hdf5_structured_dataset_per_process_ReadGeometry_X_Y_Z_R4P


    subroutine hdf5_structured_dataset_per_process_ReadGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !-----------------------------------------------------------------
    !< Read X_Y_Z R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this      !< HDF5 dataset per process handler for structured grids
        real(R8P), allocatable,                     intent(OUT):: X(:)                !< X Grid coordinates
        real(R8P), allocatable,                     intent(OUT):: Y(:)                !< Y Grid coordinates
        real(R8P), allocatable,                     intent(OUT):: Z(:)                !< Z Grid coordinates
        character(len=*),                           intent(IN) :: Name                !< Geometry dataset name
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnodesperdim(3)!< Global number of nodes per dimension
        integer(HSIZE_T)                                       :: localnodesperdim(3) !< Local number of nodes per dimension
        integer(HSIZE_T)                                       :: nodeoffsetperdim(3) !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypePerGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnodesperdim(1) = int(this%SpatialGridDescriptor%GetGlobalXsize(),HSIZE_T)
        globalnodesperdim(2) = int(this%SpatialGridDescriptor%GetGlobalYsize(),HSIZE_T)
        localnodesperdim(1)  = int(this%SpatialGridDescriptor%GetXSizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        localnodesperdim(2)  = int(this%SpatialGridDescriptor%GetYSizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffsetperdim(1) = int(this%SpatialGridDescriptor%GetXSizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        nodeoffsetperdim(2) = int(this%SpatialGridDescriptor%GetYSizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
    !-----------------------------------------------------------------
    !< X
    !----------------------------------------------------------------- 
        call this%ReadHyperSlab(                                                                                         &
                DatasetName     = 'X_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnodesperdim(1)/),                                                               &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnodesperdim(1)/),                                                               &
                Values          = X)
    !-----------------------------------------------------------------
    !< Y
    !----------------------------------------------------------------- 
        call this%ReadHyperSlab(                                                                                         &
                DatasetName     = 'Y_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/localnodesperdim(2)/),                                                               &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                HyperSlabSize   = (/localnodesperdim(2)/),                                                               &
                Values          = Y)
    !-----------------------------------------------------------------
    !< Z
    !----------------------------------------------------------------- 
        if(spacedim == 3) then
            globalnodesperdim(3) = int(this%SpatialGridDescriptor%GetGlobalZsize(),HSIZE_T)
            localnodesperdim(3)  = int(this%SpatialGridDescriptor%GetZSizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
            nodeoffsetperdim(3)  = int(this%SpatialGridDescriptor%GetZSizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
            call this%ReadHyperSlab(                                                                                         &
                    DatasetName     = 'Z_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                    DatasetDims     = (/LOCalnodesperdim(3)/),                                                               &
                    HyperSlabOffset = (/0_HSIZE_T/),                                                                         &
                    HyperSlabSize   = (/localnodesperdim(3)/),                                                               &
                    Values          = Z)
        else
            if(allocated(Z)) deallocate(Z); allocate(Z(1)); Z = 0.0_R4P
        endif
#endif
    end subroutine hdf5_structured_dataset_per_process_ReadGeometry_X_Y_Z_R8P


    subroutine hdf5_structured_dataset_per_process_ReadGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Read R4P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this   !< HDF5 dataset per process handler for structured grids
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
                call this%ReadHyperSlab(                                                                                              &
                        DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/3_HSIZE_T/),                                                                              &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/3_HSIZE_T/),                                                                              &
                        Values          = Origin)
                call this%ReadHyperSlab(                                                                                              &
                        DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/3_HSIZE_T/),                                                                              &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/3_HSIZE_T/),                                                                              &
                        Values          = DxDyDz)
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                ! Origin and DxDyDz size must be 2
                call this%ReadHyperSlab(                                                                                              &
                        DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/2_HSIZE_T/),                                                                              &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/2_HSIZE_T/),                                                                              &
                        Values          = Origin)
                call this%ReadHyperSlab(                                                                                              &
                        DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/2_HSIZE_T/),                                                                              &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/2_HSIZE_T/),                                                                              &
                        Values          = DxDyDz)
        end select
        Origin(:) = Origin(size(Origin,dim=1):1:-1)
        DxDyDz(:) = DxDyDz(size(DxDyDz,dim=1):1:-1)
#endif
    end subroutine hdf5_structured_dataset_per_process_ReadGeometry_DXDYDZ_R4P


    subroutine hdf5_structured_dataset_per_process_ReadGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !-----------------------------------------------------------------
    !< Read R8P coordinates to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this   !< HDF5 dataset per process handler for structured grids
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
                call this%ReadHyperSlab(                                                                                              &
                        DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/3_HSIZE_T/),                                                                              &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/3_HSIZE_T/),                                                                              &
                        Values          = Origin)
                call this%ReadHyperSlab(                                                                                              &
                        DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/3_HSIZE_T/),                                                                              &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/3_HSIZE_T/),                                                                              &
                        Values          = DxDyDz)
            case (XDMF_GEOMETRY_TYPE_ORIGIN_DXDY)
                ! Origin and DxDyDz size must be 2
                call this%ReadHyperSlab(                                                                                              &
                        DatasetName     = 'Origin_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/2_HSIZE_T/),                                                                              &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/2_HSIZE_T/),                                                                              &
                        Values          = Origin)
                call this%ReadHyperSlab(                                                                                              &
                        DatasetName     = 'DxDyDz_'//Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                        DatasetDims     = (/2_HSIZE_T/),                                                                              &
                        HyperSlabOffset = (/0_HSIZE_T/),                                                                              &
                        HyperSlabSize   = (/2_HSIZE_T/),                                                                              &
                        Values          = DxDyDz)
        end select
        Origin(:) = Origin(size(Origin,dim=1):1:-1)
        DxDyDz(:) = DxDyDz(size(DxDyDz,dim=1):1:-1)
#endif
    end subroutine hdf5_structured_dataset_per_process_ReadGeometry_DXDYDZ_R8P


    subroutine hdf5_structured_dataset_per_process_ReadTopology_I4P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read I4P connectivities to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this     !< HDF5 dataset per process handler for structured grids
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
        call this%ReadHyperSlab(                                                                                   &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalTopologySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalTopologySize/),                                                           &
                Values          = Connectivities)
#endif
    end subroutine hdf5_structured_dataset_per_process_ReadTopology_I4P


    subroutine hdf5_structured_dataset_per_process_ReadTopology_I8P(this, Connectivities, Name)
    !-----------------------------------------------------------------
    !< Read I8P connectivities to a HDF5 file for the dataset per process strategy
    !----------------------------------------------------------------- 
        class(hdf5_structured_dataset_per_process_handler_t), intent(IN) :: this     !< HDF5 dataset per process handler for structured grids
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
        call this%ReadHyperSlab(                                                                                   &
                DatasetName     = Name//'_'//trim(adjustl(str(no_sign=.true.,n=this%MPIEnvironment%get_rank()))),  &
                DatasetDims     = (/LocalTopologySize/),                                                           &
                HyperSlabOffset = (/0_HSIZE_T/),                                                                   &
                HyperSlabSize   = (/LocalTopologySize/),                                                           &
                Values          = Connectivities)
#endif
    end subroutine hdf5_structured_dataset_per_process_ReadTopology_I8P


end module hdf5_structured_dataset_per_process_handler
