program xh5for_ch_unstructured_hexahedron

use xh5for

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
    type(xh5for_t)                :: xh5
    real(R4P),        allocatable :: geometry(:)
    integer(I4P),     allocatable :: topology(:)
    real(R4P),        allocatable :: out_geometry(:)
    integer(I4P),     allocatable :: out_topology(:)
    integer(I4P)                  :: num_elems_per_axis = 1
    integer(I4P)                  :: num_nodes
    integer(I4P)                  :: num_elements
    character(len=10)             :: arg
    
    integer                       :: rank = 0
    integer                       :: mpierr
    integer                       :: exitcode = 0
    integer                       :: i, j ,k


    !-----------------------------------------------------------------
    !< Main program
    !----------------------------------------------------------------- 

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_INIT(mpierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr);
#endif

    if(command_argument_count()==1) then
        call get_command_argument(1, arg)
        arg = trim(adjustl(arg))
        read(arg,*)  num_elems_per_axis
    endif

    call generate_hexa_mesh(rank, num_elems_per_axis, geometry, topology, num_nodes, num_elements)
    !< Write XDMF/HDF5 file
    call xh5%Open(FilePrefix='xh5for_ch_unstructured_hexahedron_perf', Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB, Action=XDMF_ACTION_WRITE)
    call xh5%SetGrid(NumberOfNodes=num_nodes, NumberOfElements=num_elements,TopologyType=XDMF_TOPOLOGY_TYPE_HEXAHEDRON, GeometryType=XDMF_GEOMETRY_TYPE_XYZ)
    call xh5%WriteGeometry(XYZ=geometry)
    call xh5%WriteTopology(Connectivities=topology)
    call xh5%Close()
    call xh5%Free()

    !< Read XDMF/HDF5 file
    call xh5%Open(FilePrefix='xh5for_ch_unstructured_hexahedron_perf', Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB, Action=XDMF_ACTION_READ)
    call xh5%ParseGrid()
    call xh5%ReadTopology(Connectivities=out_topology)
    call xh5%ReadGeometry(XYZ=out_geometry)
    call xh5%Close()
    call xh5%Free()

#ifdef ENABLE_HDF5
    !< Check results
    if(.not. (sum(out_geometry - geometry)<=epsilon(0._R4P))) exitcode = -1
    if(.not. (sum(out_topology - topology)==0)) exitcode = -1
#else
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif

    call exit( status=exitcode)


contains

    subroutine generate_hexa_mesh(rank, num_elements_per_line, XYZ, topology, number_nodes, number_elements)
        integer,                   intent(in)    :: rank
        integer,                   intent(in)    :: num_elements_per_line
        real(R4P),    allocatable, intent(inout) :: XYZ(:)
        integer(I4P), allocatable, intent(inout) :: topology(:)
        integer(I4P),              intent(out)   :: number_nodes
        integer(I4P),              intent(out)   :: number_elements
        integer                                  :: dim = 3
        integer                                  :: num_elements_per_surface
        integer                                  :: num_nodes_per_line
        integer                                  :: num_nodes_per_surface
        integer                                  :: num_nodes_per_element = 8 ! hexahedron
        real(R4P),    dimension(3)               :: first_point = (/0.0,0.0,0.0/)
        real(R4P),    dimension(3)               :: last_point = (/1.0,1.0,1.0/)
        real(R4P),    dimension(3)               :: steps
        integer(I4P), dimension(8)               :: first_element_topology

        num_elements_per_surface = num_elements_per_line**2
        num_elements             = num_elements_per_line**3
        num_nodes_per_line       = num_elements_per_line+1
        num_nodes_per_surface    = num_nodes_per_line**2
        num_nodes                = num_nodes_per_line**3
        !< Initialize some values depending on the mpi rank
        first_point = first_point+rank
        last_point = last_point+rank
        steps = (last_point-first_point)/real(num_elements_per_line, R4P)

        first_element_topology = (/0,                                          1,                                       &
                                   num_nodes_per_line+1,                       num_nodes_per_line,                      &
                                   num_nodes_per_surface,                      num_nodes_per_surface+1,                 &
                                   num_nodes_per_surface+num_nodes_per_line+1, num_nodes_per_surface+num_nodes_per_line/)

        if(allocated(XYZ)) deallocate(XYZ)
        if(allocated(topology)) deallocate(topology)
        allocate(XYZ(num_nodes*3))
        allocate(topology(num_elements*num_nodes_per_element))

        do i=0, num_nodes-1
            XYZ((i*dim)+1) = first_point(1)+mod(i/num_nodes_per_surface,num_nodes_per_line)*steps(1)
            XYZ((i*dim)+2) = first_point(2)+mod(i/num_nodes_per_line,num_nodes_per_line)*steps(2)
            XYZ((i*dim)+3) = first_point(3)+mod(i,num_nodes_per_line)*steps(3)
        enddo
        do i=0, num_elements-1
            topology((i*num_nodes_per_element)+1:(i+1)*num_nodes_per_element) = &
                                            first_element_topology+&
                                            mod(i,num_elements_per_line)+&
                                            mod(i/num_elements_per_line,num_elements_per_line)*num_nodes_per_line+&
                                            mod(i/num_elements_per_surface, num_elements_per_surface)*num_nodes_per_surface
        enddo

    end subroutine

end program xh5for_ch_unstructured_hexahedron
