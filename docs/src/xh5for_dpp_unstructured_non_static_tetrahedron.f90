program xh5for_ch_unstructured_tetrahedron

use xh5for
use PENF, only: I4P, I8P, R4P, R8P, str

#if defined(ENABLE_MPI) && defined(MPI_MOD)
  use mpi
#endif
  implicit none
#if defined(ENABLE_MPI) && defined(MPI_H)
  include 'mpif.h'
#endif

    !-----------------------------------------------------------------
    !< Variable definition
    !----------------------------------------------------------------- 
    type(xh5for_t)             :: xh5

    real(R8P), dimension(5)    :: X = (/0.0, 1.0, 0.0, 0.0, 1.0/)
    real(R8P), dimension(5)    :: Y = (/0.0, 0.0, 1.0, 0.0, 1.0/)
    real(R8P), dimension(5)    :: Z = (/0.0, 0.0, 0.0, 1.0, 1.0/)
    integer(I4P), dimension(8) :: topology = (/0, 1, 2, 3,&
                                               1, 2, 3, 4/)
    real(R4P),    dimension(15) :: vectorvelocity = (/0,0,0, &
                                                      1,0,0, &
                                                      2,0,0, &
                                                      3,0,0, &
                                                      4,0,0/)

    real(R8P),    allocatable  :: out_X(:)
    real(R8P),    allocatable  :: out_Y(:)
    real(R8P),    allocatable  :: out_Z(:)
    integer(I4P), allocatable  :: out_topology(:)
    real(R4P),    allocatable  :: out_vectorvelocity(:)

    integer                    :: rank = 0
    integer                    :: mpierr
    integer                    :: exitcode = 0

    real(R8P)                  :: time = 0.0
    integer                    :: num_steps = 10
    integer                    :: i


    !-----------------------------------------------------------------
    !< Main program
    !----------------------------------------------------------------- 

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_INIT(mpierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr);
#endif

    !< Initialize some values depending on the mpi rank
    X=X+rank; Y=Y+rank; Z=Z+rank
    vectorvelocity = vectorvelocity+rank

    !< Write XDMF/HDF5 file
    call xh5%Open(FilePrefix='xh5for_dpp_unstructured_non_static_tetrahedron', Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS, Action=XDMF_ACTION_WRITE)

    do i=1, num_steps
        call xh5%AppendStep(Value=time+i)
        call xh5%SetGrid(NumberOfNodes=5, NumberOfElements=2,TopologyType=XDMF_TOPOLOGY_TYPE_TETRAHEDRON, GeometryType=XDMF_GEOMETRY_TYPE_X_Y_Z)
        call xh5%WriteTopology(Connectivities = topology)
        call xh5%WriteGeometry(X = X, Y = Y, Z = Z)
        call xh5%WriteAttribute(Name='Velocity', Type=XDMF_ATTRIBUTE_TYPE_VECTOR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=vectorvelocity+i)
    enddo

    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%Open(FilePrefix='xh5for_dpp_unstructured_non_static_tetrahedron', Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS, Action=XDMF_ACTION_READ)

    do i=1, xh5%GetNumberOfSteps()
        call xh5%ParseGrid()
        call xh5%ReadTopology(Connectivities = out_topology)
        call xh5%ReadGeometry(X = out_X, Y = out_Y, Z = out_Z)
        call xh5%ReadAttribute(Name='Velocity', Type=XDMF_ATTRIBUTE_TYPE_VECTOR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=out_vectorvelocity)
        call xh5%NextStep()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_X - X)<=epsilon(0._R8P))) exitcode = -1
    if(.not. (sum(out_Y - Y)<=epsilon(0._R8P))) exitcode = -1
    if(.not. (sum(out_Z - Z)<=epsilon(0._R8P))) exitcode = -1
    if(.not. (sum(out_topology - topology)==0)) exitcode = -1
    if(.not. (sum(out_vectorVelocity - (vectorvelocity+i))<=epsilon(0._R4P))) exitcode = -1
#endif
    enddo

    call xh5%Close()
    call xh5%Free()

#ifndef ENABLE_HDF5
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif


#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif


    call exit(exitcode)
end program xh5for_ch_unstructured_tetrahedron
