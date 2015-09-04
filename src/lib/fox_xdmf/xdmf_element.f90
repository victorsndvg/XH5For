module xdmf_element
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF element handling module
!< @TODO enhance and generalice the module.
!< @TODO add comments.
!--------------------------------------------------------------------- -----------------------------------------------------------

implicit none

    type, abstract:: xdmf_element_t
    private
        character(len=:), allocatable :: Tag
        logical, public               :: warn = .true. !< @Note Remove this variable or set to .false.. .true. Only valid under development
    contains
    private
        procedure(element_default_initialization), deferred :: default_initialization
        procedure, public                 :: get_tag => element_get_tag
        procedure, public                 :: set_tag => element_set_tag
        procedure(element_free), deferred :: free
    end type xdmf_element_t

    abstract interface
        subroutine element_default_initialization(this)
            import xdmf_element_t
            class(xdmf_element_t), intent(INOUT) :: this
        end subroutine element_default_initialization

        subroutine element_free(this)
            import xdmf_element_t
            class(xdmf_element_t), intent(INOUT) :: this
        end subroutine element_free
    end interface


contains

    function element_get_tag(this) result(tag)
        class(xdmf_element_t) :: this
        character(len=:), allocatable :: tag
        if(allocated(this%tag)) tag = this%tag
    end function element_get_tag

    subroutine element_set_tag(this, tag) 
        class(xdmf_element_t) :: this
        character(len=*), intent(IN) :: tag
        this%Tag = tag
    end subroutine element_set_tag


end module xdmf_element
