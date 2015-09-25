module hdf5_contiguous_hyperslab_handler

use IR_Precision, only : I4P, I8P, R4P, R8P
#ifdef ENABLE_HDF5
use HDF5
#endif
use hdf5_handler
use xh5for_utils

implicit none

private

    type, extends(hdf5_handler_t) :: hdf5_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< HDF5 contiguous hyperslab handler
    !----------------------------------------------------------------- 
    contains
        procedure :: WriteHyperSlab_I4P => hdf5_contiguous_hyperslab_handler_WriteHyperSlab_I4P
        procedure :: WriteHyperSlab_I8P => hdf5_contiguous_hyperslab_handler_WriteHyperSlab_I8P
        procedure :: WriteHyperSlab_R4P => hdf5_contiguous_hyperslab_handler_WriteHyperSlab_R4P
        procedure :: WriteHyperSlab_R8P => hdf5_contiguous_hyperslab_handler_WriteHyperSlab_R8P
        procedure :: WriteGeometry_R4P  => hdf5_contiguous_hyperslab_handler_WriteGeometry_R4P
        procedure :: WriteGeometry_R8P  => hdf5_contiguous_hyperslab_handler_WriteGeometry_R8P
        procedure :: WriteTopology_I4P  => hdf5_contiguous_hyperslab_handler_WriteTopology_I4P
        procedure :: WriteTopology_I8P  => hdf5_contiguous_hyperslab_handler_WriteTopology_I8P
        procedure :: WriteAttribute_I4P => hdf5_contiguous_hyperslab_handler_WriteAttribute_I4P
        procedure :: WriteAttribute_I8P => hdf5_contiguous_hyperslab_handler_WriteAttribute_I8P
        procedure :: WriteAttribute_R4P => hdf5_contiguous_hyperslab_handler_WriteAttribute_R4P
        procedure :: WriteAttribute_R8P => hdf5_contiguous_hyperslab_handler_WriteAttribute_R8P
        generic   :: WriteHyperSlab     => WriteHyperSlab_I4P, &
                                           WriteHyperSlab_I8P, &
                                           WriteHyperSlab_R4P, &
                                           WriteHyperSlab_R8P
    end type hdf5_contiguous_hyperslab_handler_t

public :: hdf5_contiguous_hyperslab_handler_t

contains



    subroutine hdf5_contiguous_hyperslab_handler_WriteHyperSlab_I4P(this, DatasetName, DatasetDims, HyperSlabOffset, HyperSlabSize, Values)
    !-----------------------------------------------------------------
    !< Writes I4P dataset to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        character(len=*),                           intent(IN) :: DatasetName         !< Dataset name
        integer(HSIZE_T),                           intent(IN) :: DatasetDims(:)      !< Dataset dimensions
        integer(HSIZE_T),                           intent(IN) :: HyperSlabOffset(:)  !< Hyperslab offset
        integer(HSIZE_T),                           intent(IN) :: HyperSlabSize(:)    !< Hyperslab size
        integer(I4P),                               intent(IN) :: Values(:)           !< I4P Dataset values
        integer(HID_T)                                         :: filespace           !< HDF5 file Dataspace identifier
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! Create filespace
        call H5Screate_simple_f(rank = 1,                     &
                dims     = DatasetDims,                       &
                space_id = filespace,                         &
                hdferr   = hdferror)
        ! Create the dataset with default properties.
        call H5Pcreate_f(H5P_DATASET_XFER_F, prp_id = plist_id, hdferr=hdferror) 
        ! Set MPIO data transfer mode to COLLECTIVE
        call H5Pset_dxpl_mpio_f(prp_id = plist_id, data_xfer_mode = H5FD_MPIO_COLLECTIVE_F, hdferr = hdferror)
        ! Create Coordinates dataset 
        call H5Dcreate_f(loc_id = this%file_id,             &
                name     = '/'//trim(adjustl(DatasetName)), &
                type_id  = H5T_NATIVE_INTEGER,              &
                space_id = filespace,                       &
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
        ! Write data
        call H5Dwrite_f(dset_id = dset_id,           &
                mem_type_id   = H5T_NATIVE_INTEGER,  &
                buf           = Values,              &
                dims          = HyperSlabSize,       &
                hdferr        = hdferror,            &
                file_space_id = filespace,           &
                mem_space_id  = memspace,            &
                xfer_prp      = plist_id)
        ! Close data space, dataset, property list .
        call H5Sclose_f(space_id = memspace,  hdferr = hdferror) 
        call H5Dclose_f(dset_id  = dset_id,   hdferr = hdferror)
        call H5Pclose_f(prp_id   = plist_id,  hdferr = hdferror)
        call H5Sclose_f(space_id = filespace, hdferr = hdferror)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteHyperSlab_I4P


    subroutine hdf5_contiguous_hyperslab_handler_WriteHyperSlab_I8P(this, DatasetName, DatasetDims, HyperSlabOffset, HyperSlabSize, Values)
    !-----------------------------------------------------------------
    !< Writes I8P dataset to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        character(len=*),                           intent(IN) :: DatasetName         !< Dataset name
        integer(HSIZE_T),                           intent(IN) :: DatasetDims(:)      !< Dataset dimensions
        integer(HSIZE_T),                           intent(IN) :: HyperSlabOffset(:)  !< Hyperslab offset
        integer(HSIZE_T),                           intent(IN) :: HyperSlabSize(:)    !< Hyperslab size
        integer(I8P),                               intent(IN) :: Values(:)           !< I8P Dataset values
        integer(HID_T)                                         :: filespace           !< HDF5 file Dataspace identifier
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! Create filespace
        call H5Screate_simple_f(rank = 1,                     &
                dims     = DatasetDims,                       &
                space_id = filespace,                         &
                hdferr   = hdferror)
        ! Create the dataset with default properties.
        call H5Pcreate_f(H5P_DATASET_XFER_F, prp_id = plist_id, hdferr=hdferror) 
        ! Set MPIO data transfer mode to COLLECTIVE
        call H5Pset_dxpl_mpio_f(prp_id = plist_id, data_xfer_mode = H5FD_MPIO_COLLECTIVE_F, hdferr = hdferror)
        ! Create Coordinates dataset 
        call H5Dcreate_f(loc_id = this%file_id,             &
                name     = '/'//trim(adjustl(DatasetName)), &
                type_id  = H5T_NATIVE_INTEGER,              &
                space_id = filespace,                       &
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
        ! Write data
        ! I8P does not have native type in Fortran HDF5
!        call H5Dwrite_f(dset_id = dset_id,           &
!                mem_type_id   = H5T_NATIVE_INTEGER,  &
!                buf           = Values,              &
!                dims          = HyperSlabSize,       &
!                hdferr        = hdferror,            &
!                file_space_id = filespace,           &
!                mem_space_id  = memspace,            &
!                xfer_prp      = plist_id)
        ! Close data space, dataset, property list .
        call H5Sclose_f(space_id = memspace,  hdferr = hdferror) 
        call H5Dclose_f(dset_id  = dset_id,   hdferr = hdferror)
        call H5Pclose_f(prp_id   = plist_id,  hdferr = hdferror)
        call H5Sclose_f(space_id = filespace, hdferr = hdferror)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteHyperSlab_I8P


    subroutine hdf5_contiguous_hyperslab_handler_WriteHyperSlab_R4P(this, DatasetName, DatasetDims, HyperSlabOffset, HyperSlabSize, Values)
    !-----------------------------------------------------------------
    !< Writes R4P dataset to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        character(len=*),                           intent(IN) :: DatasetName         !< Dataset name
        integer(HSIZE_T),                           intent(IN) :: DatasetDims(:)      !< Dataset dimensions
        integer(HSIZE_T),                           intent(IN) :: HyperSlabOffset(:)  !< Hyperslab offset
        integer(HSIZE_T),                           intent(IN) :: HyperSlabSize(:)    !< Hyperslab size
        real(R4P),                                  intent(IN) :: Values(:)           !< R4P Dataset values
        integer(HID_T)                                         :: filespace           !< HDF5 file Dataspace identifier
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! Create filespace
        call H5Screate_simple_f(rank = 1,                     &
                dims     = DatasetDims,                       &
                space_id = filespace,                         &
                hdferr   = hdferror)
        ! Create the dataset with default properties.
        call H5Pcreate_f(H5P_DATASET_XFER_F, prp_id = plist_id, hdferr=hdferror) 
        ! Set MPIO data transfer mode to COLLECTIVE
        call H5Pset_dxpl_mpio_f(prp_id = plist_id, data_xfer_mode = H5FD_MPIO_COLLECTIVE_F, hdferr = hdferror)
        ! Create Coordinates dataset 
        call H5Dcreate_f(loc_id = this%file_id,             &
                name     = '/'//trim(adjustl(DatasetName)), &
                type_id  = H5T_NATIVE_REAL,                 &
                space_id = filespace,                       &
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
        ! Write data
        call H5Dwrite_f(dset_id = dset_id,        &
                mem_type_id   = H5T_NATIVE_REAL,  &
                buf           = Values,           &
                dims          = HyperSlabSize,    &
                hdferr        = hdferror,         &
                file_space_id = filespace,        &
                mem_space_id  = memspace,         &
                xfer_prp      = plist_id)
        ! Close data space, dataset, property list .
        call H5Sclose_f(space_id = memspace,  hdferr = hdferror) 
        call H5Dclose_f(dset_id  = dset_id,   hdferr = hdferror)
        call H5Pclose_f(prp_id   = plist_id,  hdferr = hdferror)
        call H5Sclose_f(space_id = filespace, hdferr = hdferror)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteHyperSlab_R4P


    subroutine hdf5_contiguous_hyperslab_handler_WriteHyperSlab_R8P(this, DatasetName, DatasetDims, HyperSlabOffset, HyperSlabSize, Values)
    !-----------------------------------------------------------------
    !< Writes R4P dataset to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        character(len=*),                           intent(IN) :: DatasetName         !< Dataset name
        integer(HSIZE_T),                           intent(IN) :: DatasetDims(:)      !< Dataset dimensions
        integer(HSIZE_T),                           intent(IN) :: HyperSlabOffset(:)  !< Hyperslab offset
        integer(HSIZE_T),                           intent(IN) :: HyperSlabSize(:)    !< Hyperslab size
        real(R8P),                                  intent(IN) :: Values(:)           !< R8P Dataset values
        integer(HID_T)                                         :: filespace           !< HDF5 file Dataspace identifier
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        ! Create filespace
        call H5Screate_simple_f(rank = 1,                     &
                dims     = DatasetDims,                       &
                space_id = filespace,                         &
                hdferr   = hdferror)
        ! Create the dataset with default properties.
        call H5Pcreate_f(H5P_DATASET_XFER_F, prp_id = plist_id, hdferr=hdferror) 
        ! Set MPIO data transfer mode to COLLECTIVE
        call H5Pset_dxpl_mpio_f(prp_id = plist_id, data_xfer_mode = H5FD_MPIO_COLLECTIVE_F, hdferr = hdferror)
        ! Create Coordinates dataset 
        call H5Dcreate_f(loc_id = this%file_id,             &
                name     = '/'//trim(adjustl(DatasetName)), &
                type_id  = H5T_NATIVE_DOUBLE,               &
                space_id = filespace,                       &
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
        ! Write data
        call H5Dwrite_f(dset_id = dset_id,          &
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
    end subroutine hdf5_contiguous_hyperslab_handler_WriteHyperSlab_R8P


    subroutine hdf5_contiguous_hyperslab_handler_WriteGeometry_R4P(this, Coordinates)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        real(R4P),                                  intent(IN) :: Coordinates(:)      !< Grid coordinates
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        integer(HID_T)                                         :: filespace           !< HDF5 fiel Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypeFromGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%UniformGridDescriptor%GetNumberOfNodes(),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName='Coordinates',         &
                DatasetDims     = (/spacedim*globalnumberofnodes/), &
                HyperSlabOffset = (/spacedim*nodeoffset/),          &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),  &
                Values          = Coordinates)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteGeometry_R4P



    subroutine hdf5_contiguous_hyperslab_handler_WriteGeometry_R8P(this, Coordinates)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        real(R8P),                                  intent(IN) :: Coordinates(:)      !< Grid coordinates
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        integer(HID_T)                                         :: filespace           !< HDF5 fiel Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypeFromGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%UniformGridDescriptor%GetNumberOfNodes(),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName='Coordinates',         &
                DatasetDims     = (/spacedim*globalnumberofnodes/), &
                HyperSlabOffset = (/spacedim*nodeoffset/),          &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),  &
                Values          = Coordinates)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteGeometry_R8P


    subroutine hdf5_contiguous_hyperslab_handler_WriteTopology_I4P(this, Connectivities)
    !-----------------------------------------------------------------
    !< Writes I4P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                   !< HDF5 contiguous hyperslab handler
        integer(I4P),                               intent(IN) :: Connectivities(:)      !< I4P Grid connectivities
        integer(HSIZE_T)                                       :: nodesperelement        !< Nodes per element
        integer(HSIZE_T)                                       :: globalnumberofelements !< Global number of elements
        integer(HSIZE_T)                                       :: localnumberofelements  !< Local number of elements
        integer(HSIZE_T)                                       :: elementoffset          !< Elements offset for a particular grid
        integer(HID_T)                                         :: filespace              !< HDF5 fiel Dataspace identifier
        integer(HID_T)                                         :: plist_id               !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id                !< HDF5 Dataset identifier 
        integer(HID_T)                                         :: memspace               !< HDF5 memory Dataspace identifier
        integer                                                :: hdferror               !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        nodesperelement = int(GetNumberOfNodesPerElement(this%SpatialGridDescriptor%GetTopologyTypeFromGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofelements = int(this%SpatialGridDescriptor%GetGlobalNumberOfElements(),HSIZE_T)
        localnumberofelements = int(this%UniformGridDescriptor%GetNumberOfElements(),HSIZE_T)
        elementoffset = int(this%SpatialGridDescriptor%GetElementOffsetFromGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName='Connectivities',                &
                DatasetDims     = (/nodesperelement*globalnumberofelements/), &
                HyperSlabOffset = (/nodesperelement*elementoffset/),          &
                HyperSlabSize   = (/nodesperelement*localnumberofelements/),  &
                Values          = Connectivities)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteTopology_I4P


    subroutine hdf5_contiguous_hyperslab_handler_WriteTopology_I8P(this, Connectivities)
    !-----------------------------------------------------------------
    !< Writes I8P connectivities to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                   !< HDF5 contiguous hyperslab handler
        integer(I8P),                               intent(IN) :: Connectivities(:)      !< I8P Grid connectivities
        integer(HSIZE_T)                                       :: nodesperelement        !< Nodes per element
        integer(HSIZE_T)                                       :: globalnumberofelements !< Global number of elements
        integer(HSIZE_T)                                       :: localnumberofelements  !< Local number of elements
        integer(HSIZE_T)                                       :: elementoffset          !< Elements offset for a particular grid
        integer(HID_T)                                         :: filespace              !< HDF5 fiel Dataspace identifier
        integer(HID_T)                                         :: plist_id               !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id                !< HDF5 Dataset identifier 
        integer(HID_T)                                         :: memspace               !< HDF5 memory Dataspace identifier
        integer                                                :: hdferror               !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        nodesperelement = int(GetNumberOfNodesPerElement(this%SpatialGridDescriptor%GetTopologyTypeFromGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofelements = int(this%SpatialGridDescriptor%GetGlobalNumberOfElements(),HSIZE_T)
        localnumberofelements = int(this%UniformGridDescriptor%GetNumberOfElements(),HSIZE_T)
        elementoffset = int(this%SpatialGridDescriptor%GetElementOffsetFromGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName='Connectivities',                &
                DatasetDims     = (/nodesperelement*globalnumberofelements/), &
                HyperSlabOffset = (/nodesperelement*elementoffset/),          &
                HyperSlabSize   = (/nodesperelement*localnumberofelements/),  &
                Values          = Connectivities)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteTopology_I8P


    subroutine hdf5_contiguous_hyperslab_handler_WriteAttribute_I4P(this, Name, Values)
    !-----------------------------------------------------------------
    !< Writes I4P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I4P),                               intent(IN) :: Values(:)           !< I4P Attribute values
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        integer(HID_T)                                         :: filespace           !< HDF5 fiel Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypeFromGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%UniformGridDescriptor%GetNumberOfNodes(),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName = Name,                &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = Values)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteAttribute_I4P


    subroutine hdf5_contiguous_hyperslab_handler_WriteAttribute_I8P(this, Name, Values)
    !-----------------------------------------------------------------
    !< Writes I8P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I8P),                               intent(IN) :: Values(:)           !< I8P Attribute values
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        integer(HID_T)                                         :: filespace           !< HDF5 fiel Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypeFromGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%UniformGridDescriptor%GetNumberOfNodes(),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName = Name,                &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = Values)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteAttribute_I8P


    subroutine hdf5_contiguous_hyperslab_handler_WriteAttribute_R4P(this, Name, Values)
    !-----------------------------------------------------------------
    !< Writes R4P attribute values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        real(R4P),                                  intent(IN) :: Values(:)           !< R4P Attribute values
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        integer(HID_T)                                         :: filespace           !< HDF5 fiel Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypeFromGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%UniformGridDescriptor%GetNumberOfNodes(),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName = Name,                &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = Values)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteAttribute_R4P


    subroutine hdf5_contiguous_hyperslab_handler_WriteAttribute_R8P(this, Name, Values)
    !-----------------------------------------------------------------
    !< Writes R8P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        real(R8P),                                  intent(IN) :: Values(:)           !< R8P Attribute values
        integer(HSIZE_T)                                       :: spacedim            !< Space dimension
        integer(HSIZE_T)                                       :: globalnumberofnodes !< Global number of nodes
        integer(HSIZE_T)                                       :: localnumberofnodes  !< Local number of nodes
        integer(HSIZE_T)                                       :: nodeoffset          !< Node offset for a particular grid
        integer(HID_T)                                         :: filespace           !< HDF5 fiel Dataspace identifier
        integer(HID_T)                                         :: plist_id            !< HDF5 Property list identifier 
        integer(HID_T)                                         :: dset_id             !< HDF5 Dataset identifier 
        integer(HID_T)                                         :: memspace            !< HDF5 memory Dataspace identifier
        integer                                                :: hdferror            !< HDF5 error code
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypeFromGridID(ID=this%MPIEnvironment%get_rank())),HSIZE_T)
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%UniformGridDescriptor%GetNumberOfNodes(),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName = Name,                &
                DatasetDims     = (/globalnumberofnodes/), &
                HyperSlabOffset = (/nodeoffset/),          &
                HyperSlabSize   = (/localnumberofnodes/),  &
                Values          = Values)
#endif
    end subroutine hdf5_contiguous_hyperslab_handler_WriteAttribute_R8P

end module hdf5_contiguous_hyperslab_handler
