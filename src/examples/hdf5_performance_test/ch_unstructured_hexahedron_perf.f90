program ch_unstructured_hexahedron

use PENF, only: I4P, I8P, R4P, R8P, str

#if defined(ENABLE_MPI) && defined(MPI_MOD)
  use mpi
#endif
#ifdef ENABLE_HDF5
  use hdf5
#endif
  implicit none
#if defined(ENABLE_MPI) && defined(MPI_H)
  include 'mpif.h'
#endif

    !-----------------------------------------------------------------
    !< Variable definition
    !----------------------------------------------------------------- 
    real(R4P),        allocatable :: geometry(:)
    integer(I4P),     allocatable :: topology(:)
    real(R4P),        allocatable :: out_geometry(:)
    integer(I4P),     allocatable :: out_topology(:)
    integer(I4P)                  :: num_elems_per_axis = 1
    integer(I4P)                  :: num_nodes
    integer(I4P)                  :: num_elements
    character(len=10)             :: arg

    integer                       :: comm = 0
    integer                       :: commsize = 1
    integer                       :: info = 0
    integer                       :: root = 0
    
    integer                       :: rank = 0
    integer                       :: mpierr = 0
    integer                       :: exitcode = 0
    integer                       :: i, j ,k

#ifdef ENABLE_HDF5
    integer                                                :: hdferror
    integer(HID_T)                                         :: FileID
    integer(HID_T)                                         :: filespace
    integer(HID_T)                                         :: memspace
    integer(HID_T)                                         :: plist_id
    integer(HID_T)                                         :: dset_id
#endif

    !-----------------------------------------------------------------
    !< Main program
    !----------------------------------------------------------------- 

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    integer                       :: bsize
    character(len=10)             :: bsizestr

    call MPI_INIT(mpierr)
    CALL MPI_COMM_DUP(MPI_COMM_WORLD, comm, mpierr)
    call MPI_Comm_rank(comm, rank, mpierr);
    call MPI_COMM_SIZE(comm, commsize, mpierr)
    info = MPI_INFO_NULL
#endif

    if(command_argument_count()==1) then
        call get_command_argument(1, arg)
        arg = trim(adjustl(arg))
        read(arg,*)  num_elems_per_axis
    endif

    call generate_hexa_mesh(rank, num_elems_per_axis, geometry, topology, num_nodes, num_elements)

    if(rank==0) then
        write(*,fmt='(A)') '==================================================='
        write(*,fmt='(A)') 'TEST INFO: Fixed size per task'
        write(*,fmt='(A)') 'TEST INFO: Number of elements: '//trim(adjustl(str(no_sign=.true., n=num_elements)))
        write(*,fmt='(A)') 'TEST INFO: Number of nodes: '//trim(adjustl(str(no_sign=.true., n=num_nodes)))
        write(*,fmt='(A)') '==================================================='
    endif

#ifdef ENABLE_HDF5
    !-----------------------------------------------------------------
    !< Open HDF5 file
    !----------------------------------------------------------------- 
    call H5open_f(error=hdferror) 
    call H5pcreate_f(H5P_FILE_ACCESS_F, prp_id=plist_id, hdferr=hdferror)
#if defined(ENABLE_MPI) && defined(ENABLE_PARALLEL_HDF5)
    call H5pset_fapl_mpio_f(prp_id = plist_id, comm = comm, info = info, hdferr = hdferror)
#endif
    call H5fcreate_f(name = 'ch_unst_hexa.h5',            &
            access_flags  = H5F_ACC_TRUNC_F,              &
            File_id       = FileID,                       &
            hdferr        = hdferror,                     &
            creation_prp  = H5P_DEFAULT_F,                &
            access_prp    = plist_id)
    call h5pclose_f(prp_id = plist_id, hdferr = hdferror)

    !-----------------------------------------------------------------
    !< Write I4P HyperSlab
    !----------------------------------------------------------------- 
    call H5Screate_simple_f(rank = 1,                                          &
            dims     = (/int(commsize, I8P)*size(topology, dim=1, kind=I8P)/), &
            space_id = filespace,                                              &
            hdferr   = hdferror)
    ! Create the dataset with default properties.
    call H5Pcreate_f(H5P_DATASET_XFER_F, prp_id = plist_id, hdferr=hdferror) 
#if defined(ENABLE_MPI) && defined(ENABLE_PARALLEL_HDF5)
    ! Set MPIO data transfer mode to COLLECTIVE
    call H5Pset_dxpl_mpio_f(prp_id = plist_id, data_xfer_mode = H5FD_MPIO_COLLECTIVE_F, hdferr = hdferror)
#endif
    ! Create dataset 
    call H5Dcreate_f(loc_id = FileID,                   &
            name     = '/Connectivities',               &
            type_id  = H5T_NATIVE_INTEGER,              &
            space_id = filespace,                       &
            dset_id  = dset_id,                         & 
            hdferr   = hdferror)
    ! Select hyperslab
    call H5Sselect_hyperslab_f (space_id = filespace,                      &
            operator = H5S_SELECT_SET_F,                                   &
            start    = (/int(rank, I8P)*size(topology, dim=1, kind=I8P)/), &
            count    = (/size(topology, dim=1, kind=I8P)/),                &
            hdferr   = hdferror)
    ! Create memspace
    call H5Screate_simple_f(rank = 1,                       &
            dims     = (/size(topology, dim=1, kind=I8P)/), &
            space_id = memspace,                            &
            hdferr   = hdferror) 
    ! Write data
    call H5Dwrite_f(dset_id = dset_id,                           &
            mem_type_id   = H5T_NATIVE_INTEGER,                  &
            buf           = topology,                            &
            dims          = (/size(topology, dim=1, kind=I8P)/), &
            hdferr        = hdferror,                            &
            file_space_id = filespace,                           &
            mem_space_id  = memspace,                            &
            xfer_prp      = plist_id)
    ! Close data space, dataset, property list .
    call H5Sclose_f(space_id = memspace,  hdferr = hdferror) 
    call H5Dclose_f(dset_id  = dset_id,   hdferr = hdferror)
    call H5Pclose_f(prp_id   = plist_id,  hdferr = hdferror)
    call H5Sclose_f(space_id = filespace, hdferr = hdferror)

    !-----------------------------------------------------------------
    !< Write R4P HyperSlab
    !----------------------------------------------------------------- 
    call H5Screate_simple_f(rank = 1,                                          &
            dims     = (/int(commsize, I8P)*size(geometry, dim=1, kind=I8P)/), &
            space_id = filespace,                                              &
            hdferr   = hdferror)
    ! Create the dataset with default properties.
    call H5Pcreate_f(H5P_DATASET_XFER_F, prp_id = plist_id, hdferr=hdferror) 
#if defined(ENABLE_MPI) && defined(ENABLE_PARALLEL_HDF5)
    ! Set MPIO data transfer mode to COLLECTIVE
    call H5Pset_dxpl_mpio_f(prp_id = plist_id, data_xfer_mode = H5FD_MPIO_COLLECTIVE_F, hdferr = hdferror)
#endif
    ! Create dataset 
    call H5Dcreate_f(loc_id = FileID,                   &
            name     = '/Coordinates',                  &
            type_id  = H5T_NATIVE_REAL,                 &
            space_id = filespace,                       &
            dset_id  = dset_id,                         & 
            hdferr   = hdferror)
    ! Select hyperslab
    call H5Sselect_hyperslab_f (space_id = filespace,                      &
            operator = H5S_SELECT_SET_F,                                   &
            start    = (/int(rank, I8P)*size(geometry, dim=1, kind=I8P)/), &
            count    = (/size(geometry, dim=1, kind=I8P)/),                &
            hdferr   = hdferror)
    ! Create memspace
    call H5Screate_simple_f(rank = 1,                       &
            dims     = (/size(geometry, dim=1, kind=I8P)/), &
            space_id = memspace,                            &
            hdferr   = hdferror) 
    ! Write data
    call H5Dwrite_f(dset_id = dset_id,                           &
            mem_type_id   = H5T_NATIVE_REAL,                     &
            buf           = geometry,                            &
            dims          = (/size(geometry, dim=1, kind=I8P)/), &
            hdferr        = hdferror,                            &
            file_space_id = filespace,                           &
            mem_space_id  = memspace,                            &
            xfer_prp      = plist_id)
    ! Close data space, dataset, property list .
    call H5Sclose_f(space_id = memspace,  hdferr = hdferror) 
    call H5Dclose_f(dset_id  = dset_id,   hdferr = hdferror)
    call H5Pclose_f(prp_id   = plist_id,  hdferr = hdferror)
    call H5Sclose_f(space_id = filespace, hdferr = hdferror)
    
    call H5Fclose_f(file_id = FileID, hdferr = hdferror)
    call H5close_f(error = hdferror) 
#else
    if(rank==0) write(*,*) 'Warning: HDF5 is not enabled. Please enable HDF5 and recompile to write the HeavyData'
#endif

#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
    call MPI_FINALIZE(mpierr)
#endif

    call exit( status=exitcode)


contains

    subroutine generate_hexa_mesh(rank, num_elements_per_axis, XYZ, topology, number_nodes, number_elements)
        integer,                   intent(in)    :: rank
        integer,                   intent(in)    :: num_elements_per_axis
        real(R4P),    allocatable, intent(inout) :: XYZ(:)
        integer(I4P), allocatable, intent(inout) :: topology(:)
        integer(I4P),              intent(out)   :: number_nodes
        integer(I4P),              intent(out)   :: number_elements
        integer                                  :: dim = 3
        integer                                  :: num_elements_per_surface
        integer                                  :: num_nodes_per_axis
        integer                                  :: num_nodes_per_surface
        integer                                  :: num_nodes_per_element = 8 ! hexahedron
        real(R4P),    dimension(3)               :: first_point = (/0.0,0.0,0.0/)
        real(R4P),    dimension(3)               :: last_point = (/1.0,1.0,1.0/)
        real(R4P),    dimension(3)               :: steps
        integer(I4P), dimension(8)               :: first_element_topology

        num_elements_per_surface = num_elements_per_axis**2
        number_elements          = num_elements_per_axis**3
        num_nodes_per_axis       = num_elements_per_axis+1
        num_nodes_per_surface    = num_nodes_per_axis**2
        number_nodes             = num_nodes_per_axis**3
        !< Initialize some values depending on the mpi rank
        first_point = first_point+rank
        last_point = last_point+rank
        steps = (last_point-first_point)/real(num_elements_per_axis, R4P)

        first_element_topology = (/0,                                          1,                                       &
                                   num_nodes_per_axis+1,                       num_nodes_per_axis,                      &
                                   num_nodes_per_surface,                      num_nodes_per_surface+1,                 &
                                   num_nodes_per_surface+num_nodes_per_axis+1, num_nodes_per_surface+num_nodes_per_axis/)

        if(allocated(XYZ)) deallocate(XYZ)
        if(allocated(topology)) deallocate(topology)
        allocate(XYZ(number_nodes*3))
        allocate(topology(number_elements*num_nodes_per_element))

        do i=0, number_nodes-1
            XYZ((i*dim)+1) = first_point(1)+mod(i/num_nodes_per_surface,num_nodes_per_axis)*steps(1)
            XYZ((i*dim)+2) = first_point(2)+mod(i/num_nodes_per_axis,num_nodes_per_axis)*steps(2)
            XYZ((i*dim)+3) = first_point(3)+mod(i,num_nodes_per_axis)*steps(3)
        enddo
        do i=0, number_elements-1
            topology((i*num_nodes_per_element)+1:(i+1)*num_nodes_per_element) =            &
                    first_element_topology+                                                &
                    mod(i,num_elements_per_axis)+                                          &
                    mod(i/num_elements_per_axis,num_elements_per_axis)*num_nodes_per_axis+ &
                    mod(i/num_elements_per_surface, num_elements_per_surface)*num_nodes_per_surface
        enddo

    end subroutine

end program ch_unstructured_hexahedron
