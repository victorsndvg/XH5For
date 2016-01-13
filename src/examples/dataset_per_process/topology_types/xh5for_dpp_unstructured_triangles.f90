program xh5for_dpp_unstructured_triangles

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
    real(R4P), dimension(8)    :: geometry = (/0.0, 0.0, &
                                               1.0, 0.0, &
                                               0.0, 1.0, &
                                               1.0, 1.0/)
    integer(I4P), dimension(6) :: topology = (/0, 1, 2, &
                                               1, 2, 3/)
    integer(I4P), dimension(2) :: cellfield = (/0, 1/)
    real(R4P),    allocatable  :: out_geometry(:)
    integer(I4P), allocatable  :: out_topology(:)
    real(R4P),    allocatable  :: out_cellfield(:)
    integer(I4P), allocatable  :: out_gridfield(:)

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
    geometry = geometry + rank
    cellfield = cellfield + rank

    !< Write XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS)
    call xh5%Initialize(NumberOfNodes=4, NumberOfElements=2,TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, GeometryType=XDMF_GEOMETRY_TYPE_XY)
    call xh5%Open(action=XDMF_ACTION_WRITE, fileprefix='xh5for_dpp_unstructured_triangles')
    call xh5%WriteTopology(Connectivities = topology)
    call xh5%WriteGeometry(XYZ = geometry)
    call xh5%WriteAttribute(Name='GridNumber', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_GRID , Values=(/int(rank,I4P)/))
    call xh5%WriteAttribute(Name='CellField', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=cellfield)
    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS)
    call xh5%Initialize()
    call xh5%Open(action=XDMF_ACTION_READ, fileprefix='xh5for_dpp_unstructured_triangles')
    call xh5%Parse()
    call xh5%ReadTopology(Connectivities = out_topology)
    call xh5%ReadGeometry(XYZ = out_geometry)
    call xh5%ReadAttribute(Name='GridNumber', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_GRID , Values=out_gridfield)
    call xh5%ReadAttribute(Name='CellField', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=out_cellfield)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_geometry - geometry)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_topology - topology)==0)) exitcode = -1
    if(.not. (sum(out_cellfield - cellfield)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_gridfield - (/int(rank,I4P)/))==0)) exitcode = -1
#else
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif

#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif

    call exit(exitcode)

end program xh5for_dpp_unstructured_triangles
