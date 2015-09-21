program example_unstructured_triangles

use xh5for
#ifdef MPI_MOD
use mpi
#endif
#ifdef MPI_H
include 'mpif.h'
#endif

implicit none
    !-----------------------------------------------------------------
    !< Variable definition
    !----------------------------------------------------------------- 
    type(xh5for_t)             :: xh5
    real(R4P), dimension(8)    :: triangletopology = (/0.0,0.0, 1.0,0.0, 0.0,1.0, 1.0,1.0/)
    integer(I4P), dimension(6) :: trianglegeometry = (/0,1,2,1,2,3/)
    integer                    :: mpierr


    !-----------------------------------------------------------------
    !< Main program
    !----------------------------------------------------------------- 

#ifdef ENABLE_MPI
    call MPI_INIT(mpierr)
#endif

    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB)
    call xh5%Initialize(NumberOfNodes=4, NumberOfElements=2,TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY)
    call xh5%Open(fileprefix='hyperslab')
    call xh5%WriteTopology(Coordinates = triangletopology)
    call xh5%WriteGeometry(connectivities = trianglegeometry)
    call xh5%Close()

#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif

end program example_unstructured_triangles
