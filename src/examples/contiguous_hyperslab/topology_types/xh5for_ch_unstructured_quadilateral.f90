program xh5for_ch_unstructured_quadrilateral

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
    real(R4P),    dimension(12):: geometry = (/0.0, 0.0, &
                                               1.0, 0.0, &
                                               1.0, 1.0, &
                                               0.0, 1.0, &
                                               0.0, 2.0, &
                                               1.0, 2.0/)
    integer(I4P), dimension(8) :: topology = (/0, 1, 2, 3, &
                                               2, 3, 4, 5/)
    real(R4P),    dimension(6) :: realtempR4P = (/0, 1, 2, 3, 4, 5/)
    real(R8P),    dimension(6) :: realtempR8P = (/0, 1, 2, 3, 4, 5/)
    real(R4P),    allocatable  :: out_geometry(:)
    integer(I4P), allocatable  :: out_topology(:)
    real(R4P),    allocatable  :: out_realtempR4P(:)
    real(R8P),    allocatable  :: out_realtempR8P(:)

    integer                    :: rank = 0
    integer                    :: mpierr
    integer                    :: exitcode  = 0


    !-----------------------------------------------------------------
    !< Main program
    !----------------------------------------------------------------- 

#ifdef ENABLE_MPI
    call MPI_INIT(mpierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr);
#endif

    !< Initialize some values depending on the mpi rank
    geometry = geometry+rank
    realtempR4P = realtempR4P+rank
    realtempR8P = realtempR8P+rank

    !< Write XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB)
    call xh5%Initialize(NumberOfNodes=6, NumberOfElements=2,TopologyType=XDMF_TOPOLOGY_TYPE_QUADRILATERAL, GeometryType=XDMF_GEOMETRY_TYPE_XY)
    call xh5%Open(action=XDMF_ACTION_WRITE, fileprefix='xh5for_ch_unstructured_quadrilateral')
    call xh5%WriteTopology(Connectivities = topology)
    call xh5%WriteGeometry(XYZ = geometry)
    call xh5%WriteAttribute(Name='Temperature_R4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=realtempR4P)
    call xh5%WriteAttribute(Name='Temperature_R8P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=realtempR8P)
    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB)
    call xh5%Initialize()
    call xh5%Open(action=XDMF_ACTION_READ, fileprefix='xh5for_ch_unstructured_quadrilateral')
    call xh5%Parse()
    call xh5%ReadTopology(Connectivities = out_topology)
    call xh5%ReadGeometry(XYZ = out_geometry)
    call xh5%ReadAttribute(Name='Temperature_R4P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=out_realtempR4P)
    call xh5%ReadAttribute(Name='Temperature_R8P', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=out_realtempR8P)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_geometry - geometry)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_topology - topology)==0)) exitcode = -1
    if(.not. (sum(out_realTempR4P - realTempR4P)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_realTempR8P - realTempR8P)<=epsilon(0._R8P))) exitcode = -1 
#else
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif

#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif

    call exit(exitcode)

end program xh5for_ch_unstructured_quadrilateral
