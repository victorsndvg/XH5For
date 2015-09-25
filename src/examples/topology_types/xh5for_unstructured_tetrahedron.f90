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
    integer                    :: rank = 0
    integer                    :: mpierr


    !-----------------------------------------------------------------
    !< Main program
    !----------------------------------------------------------------- 

#ifdef ENABLE_MPI
    call MPI_INIT(mpierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr);
#endif

    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB)
    call xh5%Initialize(NumberOfNodes=5, NumberOfElements=2,TopologyType=XDMF_TOPOLOGY_TYPE_TETRAHEDRON, GeometryType=XDMF_GEOMETRY_TYPE_XYZ)
    call xh5%Open(fileprefix='contiguous_hyperslab_tetrahedron')
    call xh5%WriteTopology(Connectivities = topology)
    call xh5%WriteGeometry(Coordinates = geometry + rank)
    call xh5%WriteAttribute(Name='Velocity', Type=XDMF_ATTRIBUTE_TYPE_VECTOR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=vectorvelocity+rank)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif

end program example_unstructured_tetrahedron
