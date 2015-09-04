module xdmf_file
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF file handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use FoX_wxml

implicit none

    type xdmf_file_t
    !-----------------------------------------------------------------
    !< XDMF file handler type
    !----------------------------------------------------------------- 
        character(len=:), allocatable :: filename                     !< File name
        type(xmlf_t)                  :: xml_handler                  !< FoX XML File handler
    !----------------------------------------------------------------- 
    contains
    private
        procedure, public :: openfile             => xdmf_openfile
        procedure, public :: closefile            => xdmf_closefile
        procedure, public :: set_filename         => xdmf_set_filename
        procedure, public :: get_filename         => xdmf_get_filename
    end type xdmf_file_t

    public :: xdmf_file_t

contains

    subroutine xdmf_set_filename(xdmf_file, filename)
    !-----------------------------------------------------------------
    !< Set the filename of xdmf_file type
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
        character(len=*),   intent(IN)    :: filename                 !< File name
    !----------------------------------------------------------------- 
        xdmf_file%filename = filename
    end subroutine xdmf_set_filename


    subroutine xdmf_get_filename(xdmf_file, filename)
    !-----------------------------------------------------------------
    !< Get the filename of xdmf_file type
    !----------------------------------------------------------------- 
        class(xdmf_file_t),             intent(IN)  :: xdmf_file      !< XDMF file handler
        character(len=:), allocatable,  intent(OUT) :: filename       !< File name
    !----------------------------------------------------------------- 
        if(allocated(xdmf_file%filename)) filename = xdmf_file%filename
    end subroutine xdmf_get_filename


    subroutine xdmf_openfile(xdmf_file, IO_error)
    !-----------------------------------------------------------------
    !< Open a XDMF file a returns a FoX **xml_handler**
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
        integer, optional, intent(OUT)    :: IO_error                 !< IO error status
    !-----------------------------------------------------------------
        ! preserve_whitespace: Force the pretty_print=False and 
        !                       minimize_overrun=True
        ! pretty_print       : Add EOL chars and indentation
        ! minimize_overrun   : Add EOL chars
        ! canonical          : Force canonical XML form
        ! replace            : Replace file if exists
        ! addDecl            : Add version and enconding 
        !                      (<?xml version="1.0" encoding="UTF-8"?>)
        ! warning            : Print warning messages on screen
        ! validate           : Validate XML format
        ! namespace          : Allow the use of namespaces
        call xml_OpenFile(filename=xdmf_file%filename, xf=xdmf_file%xml_handler, &
            iostat=IO_error, preserve_whitespace=.false., pretty_print=.true., &
            minimize_overrun=.true., canonical=.false., replace=.true., addDecl=.true., &
            warning=.false., validate=.false., namespace=.true.)

        call xml_DeclareNamespace(xdmf_file%xml_handler, "http://www.w3.org/2001/XInclude", "xi")
        call xml_NewElement(xdmf_file%xml_handler, "Xdmf")
        call xml_AddAttribute(xdmf_file%xml_handler,"Version","2.1")

    end subroutine xdmf_openfile

    subroutine xdmf_closefile(xdmf_file)
    !-----------------------------------------------------------------
    !< Manage the closing of a XDMF file and all the outstanding elements
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
    !-----------------------------------------------------------------
        ! empty : Empty files return warning instead of error
        call xml_Close(xf=xdmf_file%xml_handler, empty=.true.)
    end subroutine xdmf_closefile


!--------------------------------------------------------------------- -----------------------------------------------------------
end module xdmf_file
