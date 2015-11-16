module hdf5_unstructured_contiguous_hyperslab_handler

use IR_Precision, only : I4P, I8P, R4P, R8P
#ifdef ENABLE_HDF5
use HDF5
#endif
use hdf5_contiguous_hyperslab_handler
use xh5for_utils

implicit none

private

    type, extends(hdf5_contiguous_hyperslab_handler_t) :: hdf5_unstructured_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< HDF5 contiguous hyperslab handler for Unstructured grids
    !----------------------------------------------------------------- 
    contains
        procedure :: WriteGeometry_R4P  => hdf5_unstructured_contiguous_hyperslab_WriteGeometry_R4P
        procedure :: WriteGeometry_R8P  => hdf5_unstructured_contiguous_hyperslab_WriteGeometry_R8P
        procedure :: ReadGeometry_R4P   => hdf5_unstructured_contiguous_hyperslab_ReadGeometry_R4P
        procedure :: ReadGeometry_R8P   => hdf5_unstructured_contiguous_hyperslab_ReadGeometry_R8P
        procedure :: WriteTopology_I4P  => hdf5_unstructured_contiguous_hyperslab_WriteTopology_I4P
        procedure :: WriteTopology_I8P  => hdf5_unstructured_contiguous_hyperslab_WriteTopology_I8P
        procedure :: ReadTopology_I4P   => hdf5_unstructured_contiguous_hyperslab_ReadTopology_I4P
        procedure :: ReadTopology_I8P   => hdf5_unstructured_contiguous_hyperslab_ReadTopology_I8P
        procedure :: WriteAttribute_I4P => hdf5_unstructured_contiguous_hyperslab_WriteAttribute_I4P
        procedure :: WriteAttribute_I8P => hdf5_unstructured_contiguous_hyperslab_WriteAttribute_I8P
        procedure :: WriteAttribute_R4P => hdf5_unstructured_contiguous_hyperslab_WriteAttribute_R4P
        procedure :: WriteAttribute_R8P => hdf5_unstructured_contiguous_hyperslab_WriteAttribute_R8P
        procedure :: ReadAttribute_I4P  => hdf5_unstructured_contiguous_hyperslab_ReadAttribute_I4P
        procedure :: ReadAttribute_I8P  => hdf5_unstructured_contiguous_hyperslab_ReadAttribute_I8P
        procedure :: ReadAttribute_R4P  => hdf5_unstructured_contiguous_hyperslab_ReadAttribute_R4P
        procedure :: ReadAttribute_R8P  => hdf5_unstructured_contiguous_hyperslab_ReadAttribute_R8P
    end type hdf5_unstructured_contiguous_hyperslab_handler_t

public :: hdf5_unstructured_contiguous_hyperslab_handler_t

contains

    subroutine hdf5_unstructured_contiguous_hyperslab_ReadHyperSlab_R8P(this, DatasetName, DatasetDims, HyperSlabOffset, HyperSlabSize, Values)
    !-----------------------------------------------------------------
    !< read R8P dataset to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this    !< HDF5 contiguous hyperslab handler for Unstructured grids
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
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadHyperSlab_R8P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_R4P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Writes R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R4P),                                  intent(IN) :: Coordinates(:)      !< Grid coordinates
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
        localnumberofnodes = int(this%UniformGridDescriptor%GetNumberOfNodes(),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName=Name,                  &
                DatasetDims     = (/spacedim*globalnumberofnodes/), &
                HyperSlabOffset = (/spacedim*nodeoffset/),          &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),  &
                Values          = Coordinates)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_R4P



    subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_R8P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Writes R8P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R8P),                                  intent(IN) :: Coordinates(:)      !< Grid coordinates
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
        localnumberofnodes = int(this%UniformGridDescriptor%GetNumberOfNodes(),HSIZE_T)
        nodeoffset = int(this%SpatialGridDescriptor%GetNodeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName=Name,                  &
                DatasetDims     = (/spacedim*globalnumberofnodes/), &
                HyperSlabOffset = (/spacedim*nodeoffset/),          &
                HyperSlabSize   = (/spacedim*localnumberofnodes/),  &
                Values          = Coordinates)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteGeometry_R8P


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
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        call this%UniformGridDescriptor%SetTopologySize(int(size(connectivities,dim=1),I8P))
        call this%SpatialGridDescriptor%SetTopologySizePerGridID(int(size(connectivities,dim=1),I8P),ID=this%MPIEnvironment%get_rank())
        GlobalTopologySize = int(this%SpatialGridDescriptor%GetGlobalTopologySize(),HSIZE_T)
        LocalTopologySize = int(this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        TopologySizeOffset = int(this%SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)

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
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        call this%UniformGridDescriptor%SetTopologySize(int(size(connectivities,dim=1),I8P))
        call this%SpatialGridDescriptor%SetTopologySizePerGridID(int(size(connectivities,dim=1),I8P),ID=this%MPIEnvironment%get_rank())
        GlobalTopologySize = int(this%SpatialGridDescriptor%GetGlobalTopologySize(),HSIZE_T)
        LocalTopologySize = int(this%SpatialGridDescriptor%GetTopologySizePerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        TopologySizeOffset = int(this%SpatialGridDescriptor%GetTopologySizeOffsetPerGridID(ID=this%MPIEnvironment%get_rank()),HSIZE_T)
        call this%WriteHyperSlab(DatasetName=Name,                            &
                DatasetDims     = (/GlobalTopologySize/), &
                HyperSlabOffset = (/TopologySizeOffset/), &
                HyperSlabSize   = (/LocalTopologySize/),  &
                Values          = Connectivities)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteTopology_I8P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteAttribute_I4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Writes I4P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I4P),                               intent(IN) :: Type                !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN) :: Center              !< Attribute center at (Node, Cell, etc.)
        integer(I4P),                               intent(IN) :: Values(:)           !< I4P Attribute values
        integer(HSIZE_T)                                       :: GlobalNumberOfData  !< Global number of data
        integer(HSIZE_T)                                       :: LocalNumberOfData   !< Local number of data
        integer(HSIZE_T)                                       :: NumberOfComponents  !< Global number of nodes
        integer(HSIZE_T)                                       :: DataOffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        NumberOfComponents = int(GetNumberOfComponentsFromAttributeType(Type), HSIZE_T)
        call this%CalculateAttributeDimensions(          &
                Center             = Center,             &
                GlobalNumberOfData = GlobalNumberOfData, &
                LocalNumberOfData  = LocalNumberOfData,  &
                DataOffset         = DataOffset)
        call this%WriteHyperSlab(                                             &
                DatasetName     = Name,                                       &
                DatasetDims     = (/GlobalNumberOfData*NumberOfComponents/),  &
                HyperSlabOffset = (/DataOffset*NumberOfComponents/),          &
                HyperSlabSize   = (/LocalNumberOfData*NumberOfComponents/),   &
                Values          = Values)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteAttribute_I4P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteAttribute_I8P(this, Name, type, Center, Values)
    !-----------------------------------------------------------------
    !< Writes I8P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I4P),                               intent(IN) :: Type                !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN) :: Center              !< Attribute center at (Node, Cell, etc.)
        integer(I8P),                               intent(IN) :: Values(:)           !< I8P Attribute values
        integer(HSIZE_T)                                       :: GlobalNumberOfData  !< Global number of data
        integer(HSIZE_T)                                       :: LocalNumberOfData   !< Local number of data
        integer(HSIZE_T)                                       :: NumberOfComponents  !< Global number of nodes
        integer(HSIZE_T)                                       :: DataOffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        NumberOfComponents = int(GetNumberOfComponentsFromAttributeType(Type), HSIZE_T)
        call this%CalculateAttributeDimensions(          &
                Center             = Center,             &
                GlobalNumberOfData = GlobalNumberOfData, &
                LocalNumberOfData  = LocalNumberOfData,  &
                DataOffset         = DataOffset)
        call this%WriteHyperSlab(                                             &
                DatasetName     = Name,                                       &
                DatasetDims     = (/GlobalNumberOfData*NumberOfComponents/),  &
                HyperSlabOffset = (/DataOffset*NumberOfComponents/),          &
                HyperSlabSize   = (/LocalNumberOfData*NumberOfComponents/),   &
                Values          = Values)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteAttribute_I8P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteAttribute_R4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Writes R4P attribute values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I4P),                               intent(IN) :: Type                !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN) :: Center              !< Attribute center at (Node, Cell, etc.)
        real(R4P),                                  intent(IN) :: Values(:)           !< R4P Attribute values
        integer(HSIZE_T)                                       :: GlobalNumberOfData  !< Global number of data
        integer(HSIZE_T)                                       :: LocalNumberOfData   !< Local number of data
        integer(HSIZE_T)                                       :: NumberOfComponents  !< Global number of nodes
        integer(HSIZE_T)                                       :: DataOffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        NumberOfComponents = int(GetNumberOfComponentsFromAttributeType(Type), HSIZE_T)
        call this%CalculateAttributeDimensions(          &
                Center             = Center,             &
                GlobalNumberOfData = GlobalNumberOfData, &
                LocalNumberOfData  = LocalNumberOfData,  &
                DataOffset         = DataOffset)
        call this%WriteHyperSlab(                                             &
                DatasetName     = Name,                                       &
                DatasetDims     = (/GlobalNumberOfData*NumberOfComponents/),  &
                HyperSlabOffset = (/DataOffset*NumberOfComponents/),          &
                HyperSlabSize   = (/LocalNumberOfData*NumberOfComponents/),   &
                Values          = Values)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteAttribute_R4P


    subroutine hdf5_unstructured_contiguous_hyperslab_WriteAttribute_R8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Writes R8P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I4P),                               intent(IN) :: Type                !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN) :: Center              !< Attribute center at (Node, Cell, etc.)
        real(R8P),                                  intent(IN) :: Values(:)           !< R8P Attribute values
        integer(HSIZE_T)                                       :: GlobalNumberOfData  !< Global number of data
        integer(HSIZE_T)                                       :: LocalNumberOfData   !< Local number of data
        integer(HSIZE_T)                                       :: NumberOfComponents  !< Global number of nodes
        integer(HSIZE_T)                                       :: DataOffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        NumberOfComponents = int(GetNumberOfComponentsFromAttributeType(Type), HSIZE_T)
        call this%CalculateAttributeDimensions(          &
                Center             = Center,             &
                GlobalNumberOfData = GlobalNumberOfData, &
                LocalNumberOfData  = LocalNumberOfData,  &
                DataOffset         = DataOffset)
        call this%WriteHyperSlab(                                             &
                DatasetName     = Name,                                       &
                DatasetDims     = (/GlobalNumberOfData*NumberOfComponents/),  &
                HyperSlabOffset = (/DataOffset*NumberOfComponents/),          &
                HyperSlabSize   = (/LocalNumberOfData*NumberOfComponents/),   &
                Values          = Values)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_WriteAttribute_R8P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_R4P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Read R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R4P), allocatable,                     intent(OUT):: Coordinates(:)      !< Grid coordinates
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
                Values          = Coordinates)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_R4P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_R8P(this, Coordinates, Name)
    !-----------------------------------------------------------------
    !< Read R4P coordinates to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        real(R8P), allocatable,                     intent(OUT):: Coordinates(:)      !< Grid coordinates
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
                Values          = Coordinates)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadGeometry_R8P


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
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadTopology_I8P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadAttribute_I4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Writes I4P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I4P),                               intent(IN) :: Type                !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN) :: Center              !< Attribute center at (Node, Cell, etc.)
        integer(I4P), allocatable,                  intent(OUT):: Values(:)           !< I4P Attribute values
        integer(HSIZE_T)                                       :: GlobalNumberOfData  !< Global number of data
        integer(HSIZE_T)                                       :: LocalNumberOfData   !< Local number of data
        integer(HSIZE_T)                                       :: NumberOfComponents  !< Global number of nodes
        integer(HSIZE_T)                                       :: DataOffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        NumberOfComponents = int(GetNumberOfComponentsFromAttributeType(Type), HSIZE_T)
        call this%CalculateAttributeDimensions(          &
                Center             = Center,             &
                GlobalNumberOfData = GlobalNumberOfData, &
                LocalNumberOfData  = LocalNumberOfData,  &
                DataOffset         = DataOffset)
        call this%ReadHyperSlab(                                              &
                DatasetName     = Name,                                       &
                DatasetDims     = (/GlobalNumberOfData*NumberOfComponents/),  &
                HyperSlabOffset = (/DataOffset*NumberOfComponents/),          &
                HyperSlabSize   = (/LocalNumberOfData*NumberOfComponents/),   &
                Values          = Values)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadAttribute_I4P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadAttribute_I8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Writes I4P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I4P),                               intent(IN) :: Type                !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN) :: Center              !< Attribute center at (Node, Cell, etc.)
        integer(I8P), allocatable,                  intent(OUT):: Values(:)           !< I4P Attribute values
        integer(HSIZE_T)                                       :: GlobalNumberOfData  !< Global number of data
        integer(HSIZE_T)                                       :: LocalNumberOfData   !< Local number of data
        integer(HSIZE_T)                                       :: NumberOfComponents  !< Global number of nodes
        integer(HSIZE_T)                                       :: DataOffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        NumberOfComponents = int(GetNumberOfComponentsFromAttributeType(Type), HSIZE_T)
        call this%CalculateAttributeDimensions(          &
                Center             = Center,             &
                GlobalNumberOfData = GlobalNumberOfData, &
                LocalNumberOfData  = LocalNumberOfData,  &
                DataOffset         = DataOffset)
        call this%ReadHyperSlab(                                              &
                DatasetName     = Name,                                       &
                DatasetDims     = (/GlobalNumberOfData*NumberOfComponents/),  &
                HyperSlabOffset = (/DataOffset*NumberOfComponents/),          &
                HyperSlabSize   = (/LocalNumberOfData*NumberOfComponents/),   &
                Values          = Values)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadAttribute_I8P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadAttribute_R4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Writes I4P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I4P),                               intent(IN) :: Type                !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN) :: Center              !< Attribute center at (Node, Cell, etc.)
        real(R4P), allocatable,                     intent(OUT):: Values(:)           !< I4P Attribute values
        integer(HSIZE_T)                                       :: GlobalNumberOfData  !< Global number of data
        integer(HSIZE_T)                                       :: LocalNumberOfData   !< Local number of data
        integer(HSIZE_T)                                       :: NumberOfComponents  !< Global number of nodes
        integer(HSIZE_T)                                       :: DataOffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        NumberOfComponents = int(GetNumberOfComponentsFromAttributeType(Type), HSIZE_T)
        call this%CalculateAttributeDimensions(          &
                Center             = Center,             &
                GlobalNumberOfData = GlobalNumberOfData, &
                LocalNumberOfData  = LocalNumberOfData,  &
                DataOffset         = DataOffset)
        call this%ReadHyperSlab(                                              &
                DatasetName     = Name,                                       &
                DatasetDims     = (/GlobalNumberOfData*NumberOfComponents/),  &
                HyperSlabOffset = (/DataOffset*NumberOfComponents/),          &
                HyperSlabSize   = (/LocalNumberOfData*NumberOfComponents/),   &
                Values          = Values)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadAttribute_R4P


    subroutine hdf5_unstructured_contiguous_hyperslab_ReadAttribute_R8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Writes I4P attriburte values to a HDF5 file for the contiguous HyperSlab strategy
    !----------------------------------------------------------------- 
        class(hdf5_unstructured_contiguous_hyperslab_handler_t), intent(IN) :: this   !< HDF5 contiguous hyperslab handler for Unstructured grids
        character(len=*),                           intent(IN) :: Name                !< Attribute name
        integer(I4P),                               intent(IN) :: Type                !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                               intent(IN) :: Center              !< Attribute center at (Node, Cell, etc.)
        real(R8P), allocatable,                     intent(OUT):: Values(:)           !< I4P Attribute values
        integer(HSIZE_T)                                       :: GlobalNumberOfData  !< Global number of data
        integer(HSIZE_T)                                       :: LocalNumberOfData   !< Local number of data
        integer(HSIZE_T)                                       :: NumberOfComponents  !< Global number of nodes
        integer(HSIZE_T)                                       :: DataOffset          !< Node offset for a particular grid
    !-----------------------------------------------------------------
        !< @Note: Fixed rank 1?
        !< @Note: Fixed dataset name?
        !< @Note: Fixed rank 1?
#ifdef ENABLE_HDF5
        NumberOfComponents = int(GetNumberOfComponentsFromAttributeType(Type), HSIZE_T)
        call this%CalculateAttributeDimensions(          &
                Center             = Center,             &
                GlobalNumberOfData = GlobalNumberOfData, &
                LocalNumberOfData  = LocalNumberOfData,  &
                DataOffset         = DataOffset)
        call this%ReadHyperSlab(                                              &
                DatasetName     = Name,                                       &
                DatasetDims     = (/GlobalNumberOfData*NumberOfComponents/),  &
                HyperSlabOffset = (/DataOffset*NumberOfComponents/),          &
                HyperSlabSize   = (/LocalNumberOfData*NumberOfComponents/),   &
                Values          = Values)
#endif
    end subroutine hdf5_unstructured_contiguous_hyperslab_ReadAttribute_R8P


end module hdf5_unstructured_contiguous_hyperslab_handler
