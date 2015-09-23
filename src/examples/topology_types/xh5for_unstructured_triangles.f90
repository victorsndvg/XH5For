program example_unstructured_triangles

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
    real(R4P), dimension(8)    :: trianglegeometry = (/0.0, 0.0, &
                                                       1.0, 0.0, &
                                                       0.0, 1.0, &
                                                       1.0, 1.0/)
    integer(I4P), dimension(6) :: triangletopology = (/0, 1, 2, &
                                                       1, 2, 3/)
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
    call xh5%Initialize(NumberOfNodes=4, NumberOfElements=2,TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY)
    call xh5%Open(fileprefix='contiguous_hyperslab_triangles')
    call xh5%WriteTopology(Connectivities = triangletopology)
    call xh5%WriteGeometry(Coordinates = trianglegeometry + rank)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif

end program example_unstructured_triangles
