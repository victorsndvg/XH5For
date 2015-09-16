module hdf5_contiguous_hyperslab_handler

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
        procedure, public :: WriteTopology  => hdf5_contiguous_hyperslab_WriteTopology
        procedure, public :: WriteGeometry  => hdf5_contiguous_hyperslab_WriteGeometry
!        procedure, public :: WriteAttribute => hdf5_contiguous_hyperslab_WriteAttribute
    end type hdf5_contiguous_hyperslab_handler_t

contains


    subroutine hdf5_contiguous_hyperslab_WriteTopology(this, GridID)
    !-----------------------------------------------------------------
    !< Writes a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                !< HDF5 contiguous hyperslab handler
        integer(I4P)                                           :: GridId              !< Grid ID number
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
        spacedim = int(GetSpaceDimension(this%SpatialGridDescriptor%GetGeometryTypeFromGridID(ID=GridID)),HSIZE_T)
        globalnumberofnodes = int(this%SpatialGridDescriptor%GetGlobalNumberOfNodes(),HSIZE_T)
        localnumberofnodes = int(this%UniformGridDescriptor%GetNumberOfNodes(),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetFromGridID(ID=GridID),HSIZE_T)
        ! Create filespace
        call H5Screate_simple_f(rank = 1,                     &
                dims     = (/spacedim*globalnumberofnodes/),  &
                space_id = filespace,                         &
                hdferr   = hdferror)
        ! Create the dataset with default properties.
        call H5Pcreate_f(H5P_DATASET_XFER_F, prp_id = plist_id, hdferr=hdferror) 
        ! Set MPIO data transfer mode to COLLECTIVE
        call H5Pset_dxpl_mpio_f(prp_id = plist_id, data_xfer_mode = H5FD_MPIO_COLLECTIVE_F, hdferr = hdferror)
        ! Create Coordinates dataset 
        call H5Dcreate_f(loc_id = this%file_id,  &
                name = 'Grid0'//'/Coordinates',  &
                type_id = H5T_NATIVE_DOUBLE,     &
                space_id = filespace,            &
                dset_id = dset_id,               & 
                hdferr = hdferror)
        ! Select hyperslab
        call H5Sselect_hyperslab_f (space_id = filespace,   &
                operator = H5S_SELECT_SET_F,                &
                start    = (/spacedim*nodeoffset/),         &
                count    = (/spacedim*localnumberofnodes/), &
                hdferr   = hdferror)
        ! Create memspace
        call H5Screate_simple_f(rank = 1,                   &
                dims     = (/spacedim*localnumberofnodes/), &
                space_id = memspace,                        &
                hdferr   = hdferror) 
        ! Write data
        call H5Dwrite_f(dset_id = dset_id,                        &
                mem_type_id   = H5T_NATIVE_DOUBLE,                &
                buf           = (/0.1,0.2/), & !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                dims          = (/spacedim*localnumberofnodes/),  &
                hdferr        = hdferror,                         &
                file_space_id = filespace,                        &
                mem_space_id  = memspace,                         &
                xfer_prp      = plist_id)
        ! Close data space, dataset, property list .
        call H5Sclose_f(space_id = memspace,  hdferr = hdferror) 
        call H5Dclose_f(dset_id  = dset_id,   hdferr = hdferror)
        call H5Pclose_f(prp_id   = plist_id,  hdferr = hdferror)
        call H5Sclose_f(space_id = filespace, hdferr = hdferror)
#endif
    end subroutine hdf5_contiguous_hyperslab_WriteTopology


    subroutine hdf5_contiguous_hyperslab_WriteGeometry(this, GridID)
    !-----------------------------------------------------------------
    !< Writes a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_contiguous_hyperslab_handler_t), intent(IN) :: this                   !< HDF5 contiguous hyperslab handler
        integer(I4P)                                           :: GridId                 !< Grid ID number
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
        nodesperelement = int(GetNumberOfNodesPerElement(this%SpatialGridDescriptor%GetTopologyTypeFromGridID(ID=GridID)),HSIZE_T)
        globalnumberofelements = int(this%SpatialGridDescriptor%GetGlobalNumberOfElements(),HSIZE_T)
        localnumberofelements = int(this%UniformGridDescriptor%GetNumberOfElements(),HSIZE_T)
        elementoffset = int(this%SpatialGridDescriptor%GetElementOffsetFromGridID(ID=GridID),HSIZE_T)
        ! Create filespace
        call H5Screate_simple_f(rank = 1,                               &
                dims     = (/nodesperelement*globalnumberofelements/),  &
                space_id = filespace,                                   &
                hdferr   = hdferror)
        ! Create the dataset with default properties.
        call H5Pcreate_f(H5P_DATASET_XFER_F, prp_id = plist_id, hdferr=hdferror) 
        ! Set MPIO data transfer mode to COLLECTIVE
        call H5Pset_dxpl_mpio_f(prp_id = plist_id, data_xfer_mode = H5FD_MPIO_COLLECTIVE_F, hdferr = hdferror)
        ! Create Coordinates dataset 
        call H5Dcreate_f(loc_id = this%file_id,     &
                name = 'Grid0'//'/Connectivities',  &
                type_id = H5T_NATIVE_DOUBLE,        &
                space_id = filespace,               &
                dset_id = dset_id,                  & 
                hdferr = hdferror)
        ! Select hyperslab
        call H5Sselect_hyperslab_f (space_id = filespace,            &
                operator = H5S_SELECT_SET_F,                         &
                start    = (/nodesperelement*elementoffset/),        &
                count    = (/nodesperelement*localnumberofelements/), &
                hdferr   = hdferror)
        ! Create memspace
        call H5Screate_simple_f(rank = 1,                             &
                dims     = (/nodesperelement*localnumberofelements/), &
                space_id = memspace,                                  &
                hdferr   = hdferror) 
        ! Write data
        call H5Dwrite_f(dset_id = dset_id,                                  &
                mem_type_id   = H5T_NATIVE_DOUBLE,                          &
                buf           = (/0.1,0.2/), & !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                dims          = (/nodesperelement*localnumberofelements/),  &
                hdferr        = hdferror,                                   &
                file_space_id = filespace,                                  &
                mem_space_id  = memspace,                                   &
                xfer_prp      = plist_id)
        ! Close data space, dataset, property list .
        call H5Sclose_f(space_id = memspace,  hdferr = hdferror) 
        call H5Dclose_f(dset_id  = dset_id,   hdferr = hdferror)
        call H5Pclose_f(prp_id   = plist_id,  hdferr = hdferror)
        call H5Sclose_f(space_id = filespace, hdferr = hdferror)
#endif
    end subroutine hdf5_contiguous_hyperslab_WriteGeometry


end module hdf5_contiguous_hyperslab_handler
