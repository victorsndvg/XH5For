program xh5for_ch_unstructured_hexahedron

use xh5for
#ifdef ENABLE_MPI
#ifdef MPI_MOD
  use mpi
#else
  include 'mpif.h'
#endif
#endif

implicit none
    !-----------------------------------------------------------------
    !< Variable definition
    !----------------------------------------------------------------- 
    type(xh5for_t)             :: xh5
    real(R4P), dimension(24)   :: geometry = (/0.0, 0.0, 0.0, &
                                               0.0, 0.0, 1.0, &
                                               0.0, 1.0, 1.0, &
                                               0.0, 1.0, 0.0, &
                                               1.0, 0.0, 0.0, &
                                               1.0, 0.0, 1.0, &
                                               1.0, 1.0, 1.0, &
                                               1.0, 1.0, 0.0/)
    integer(I4P), dimension(8) :: topology = (/0, 1, 2, 3, 4, 5, 6, 7/)
    integer(I4P), dimension(8) :: scalartempI4P = (/0, 1, 2, 3, 4, 5, 6, 7/)
    integer(I8P), dimension(8) :: scalartempI8P = (/0, 1, 2, 3, 4, 5, 6, 7/)
    real(R4P),    allocatable  :: out_geometry(:)
    integer(I4P), allocatable  :: out_topology(:)
    integer(I4P), allocatable  :: out_scalartempI4P(:)
    integer(I8P), allocatable  :: out_scalartempI8P(:)
    
    integer                    :: rank = 0
    integer                    :: mpierr
    integer                    :: exitcode = 0


    !-----------------------------------------------------------------
    !< Main program
    !----------------------------------------------------------------- 

#ifdef ENABLE_MPI
    call MPI_INIT(mpierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr);
#endif
    !< Initialize some values depending on the mpi rank
    geometry = geometry+rank
    scalartempI4P = scalartempI4P+rank
    scalartempI8P = scalartempI8P+rank

    !< Write XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB)
    call xh5%Initialize(NumberOfNodes=8, NumberOfElements=1,TopologyType=XDMF_TOPOLOGY_TYPE_HEXAHEDRON, GeometryType=XDMF_GEOMETRY_TYPE_XYZ)
    call xh5%Open(action=XDMF_ACTION_WRITE , fileprefix='xh5for_ch_unstructured_hexahedron')
    call xh5%WriteTopology(Connectivities=topology)
    call xh5%WriteGeometry(XYZ=geometry)
    call xh5%WriteAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=scalartempI4P)
    call xh5%WriteAttribute(Name='Temperature_I8P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=scalartempI8P)
    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB)
    call xh5%Initialize()
    call xh5%Open(action=XDMF_ACTION_READ, fileprefix='xh5for_ch_unstructured_hexahedron')
    call xh5%Parse()
    call xh5%ReadTopology(Connectivities=out_topology)
    call xh5%ReadGeometry(XYZ=out_geometry)
    call xh5%ReadAttribute(Name='Temperature_I4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=out_scalartempI4P)
    call xh5%ReadAttribute(Name='Temperature_I8P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=out_scalartempI8P)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_geometry - geometry)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_topology - topology)==0)) exitcode = -1
    if(.not. (sum(out_scalarTempI4P - scalarTempI4P)==0)) exitcode = -1
!    if(.not. (sum(out_scalarTempI8P - scalarTempI8P)==0)) exitcode = -1 !I8P not supported in HDF5 layer
#else
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif

#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif

    call exit( status=exitcode)
end program xh5for_ch_unstructured_hexahedron
