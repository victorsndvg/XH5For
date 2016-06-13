program xh5for_dpp_unstructured_tetrahedron

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

    real(R4P), dimension(5)    :: X = (/0.0, 1.0, 0.0, 0.0, 1.0/)
    real(R4P), dimension(5)    :: Y = (/0.0, 0.0, 1.0, 0.0, 1.0/)
    real(R4P), dimension(5)    :: Z = (/0.0, 0.0, 0.0, 1.0, 1.0/)
    integer(I4P), dimension(8) :: topology = (/0, 1, 2, 3,&
                                               1, 2, 3, 4/)
    real(R4P),    dimension(15) :: vectorvelocity = (/0,0,0, &
                                                      1,0,0, &
                                                      2,0,0, &
                                                      3,0,0, &
                                                      4,0,0/)

    real(R4P),    allocatable  :: out_X(:)
    real(R4P),    allocatable  :: out_Y(:)
    real(R4P),    allocatable  :: out_Z(:)
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
    X=X+rank; Y=Y+rank; Z=Z+rank
    vectorvelocity = vectorvelocity+rank

    !< Write XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS)
    call xh5%Initialize(NumberOfNodes=5, NumberOfElements=2,TopologyType=XDMF_TOPOLOGY_TYPE_TETRAHEDRON, GeometryType=XDMF_GEOMETRY_TYPE_X_Y_Z)
    call xh5%Open(fileprefix='xh5for_dpp_unstructured_tetrahedron')
    call xh5%AppendStep(Value=0.0_R8P)
    call xh5%WriteTopology(Connectivities = topology)
    call xh5%WriteGeometry(X = X, Y = Y, Z = Z)
    call xh5%WriteAttribute(Name='Velocity', Type=XDMF_ATTRIBUTE_TYPE_VECTOR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=vectorvelocity)
    call xh5%Serialize()
    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%SetStrategy(Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS)
    call xh5%Initialize()
    call xh5%Open(action=XDMF_ACTION_READ, fileprefix='xh5for_dpp_unstructured_tetrahedron')
    call xh5%Parse()
    call xh5%AppendStep(Value=0.0_R8P)
    call xh5%ReadTopology(Connectivities = out_topology)
    call xh5%ReadGeometry(X = out_X, Y = out_Y, Z = out_Z)
    call xh5%ReadAttribute(Name='Velocity', Type=XDMF_ATTRIBUTE_TYPE_VECTOR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=out_vectorvelocity)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_X - X)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_Y - Y)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_Z - Z)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_topology - topology)==0)) exitcode = -1
    if(.not. (sum(out_vectorVelocity - vectorvelocity)<=epsilon(0._R4P))) exitcode = -1
#else
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif

#ifdef ENABLE_MPI
    call MPI_FINALIZE(mpierr)
#endif


    call exit(exitcode)
end program xh5for_dpp_unstructured_tetrahedron
