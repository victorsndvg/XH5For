program example_unstructured_tetrahedron

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
    real(R4P), dimension(15)   :: geometry = (/0.0, 0.0, 0.0, &
                                               1.0, 0.0, 0.0, &
                                               0.0, 1.0, 0.0, &
                                               0.0, 0.0, 1.0, &
                                               1.0, 1.0, 1.0/)
    integer(I4P), dimension(8) :: topology = (/0, 1, 2, 3,&
                                               1, 2, 3, 4/)
    real(R4P),    dimension(15) :: vectorvelocity = (/0,0,0, &
                                                     1,0,0, &
                                                     2,0,0, &
                                                     3,0,0, &
                                                     4,0,0/)
    real(R4P),    allocatable  :: out_geometry(:)
    integer(I4P), allocatable  :: out_topology(:)
    real(R4P),    allocatable  :: out_vectorvelocity(:)

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
    vectorvelocity = vectorvelocity+rank

    !< Write XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB)
    call xh5%Initialize(NumberOfNodes=5, NumberOfElements=2,TopologyType=XDMF_TOPOLOGY_TYPE_TETRAHEDRON, GeometryType=XDMF_GEOMETRY_TYPE_XYZ)
    call xh5%Open(fileprefix='contiguous_hyperslab_tetrahedron')
    call xh5%WriteTopology(Connectivities = topology)
    call xh5%WriteGeometry(Coordinates = geometry)
    call xh5%WriteAttribute(Name='Velocity', Type=XDMF_ATTRIBUTE_TYPE_VECTOR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=vectorvelocity)
    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB)
    call xh5%Initialize()
    call xh5%Open(action=XDMF_ACTION_READ, fileprefix='contiguous_hyperslab_tetrahedron')
    call xh5%Parse()
    call xh5%ReadTopology(Connectivities = out_topology)
    call xh5%ReadGeometry(Coordinates = out_geometry)
    call xh5%ReadAttribute(Name='Velocity', Type=XDMF_ATTRIBUTE_TYPE_VECTOR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=out_vectorvelocity)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_geometry - geometry)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_topology - topology)==0)) exitcode = -1
    if(.not. (sum(out_vectorVelocity - vectorvelocity)<=epsilon(0._R4P))) exitcode = -1
#else
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif

#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif


    call exit(exitcode)
end program example_unstructured_tetrahedron
