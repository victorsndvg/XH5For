module xdmf_file
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF file handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use FoX_wxml
use FoX_dom, only: Node, parseFile

implicit none
private

    type xdmf_file_t
    !-----------------------------------------------------------------
    !< XDMF file handler type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: filename                     !< File name
        type(xmlf_t), public          :: xml_handler                  !< FoX SAX XML File handler
        type(Node),       pointer     :: Root => null()               !< FoX DOM node list pointing to XML root element
    !----------------------------------------------------------------- 
    contains
    private
        procedure, public :: openfile             => xdmf_file_openfile
        procedure, public :: parsefile            => xdmf_file_parsefile
        procedure, public :: closefile            => xdmf_file_closefile
        procedure, public :: set_filename         => xdmf_file_set_filename
        procedure, public :: get_filename         => xdmf_file_get_filename
        procedure, public :: get_xml_handler      => xdmf_file_get_xml_handler
        procedure, public :: get_document_root    => xdmf_file_get_document_root
        procedure, public :: free                 => xdmf_file_free
    end type xdmf_file_t

    public :: xdmf_file_t

contains

    subroutine xdmf_file_set_filename(xdmf_file, filename)
    !-----------------------------------------------------------------
    !< Set the filename of xdmf_file type
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
        character(len=*),   intent(IN)    :: filename                 !< File name
    !----------------------------------------------------------------- 
        xdmf_file%filename = filename
    end subroutine xdmf_file_set_filename


    function xdmf_file_get_filename(xdmf_file) result(filename)
    !-----------------------------------------------------------------
    !< Get the filename of xdmf_file type
    !----------------------------------------------------------------- 
        class(xdmf_file_t),             intent(IN)  :: xdmf_file      !< XDMF file handler
        character(len=:), allocatable               :: filename       !< File name
    !----------------------------------------------------------------- 
        if(allocated(xdmf_file%filename)) then
            filename = xdmf_file%filename
        else
            filename = ''
        endif
    end function xdmf_file_get_filename


    function xdmf_file_get_xml_handler(xdmf_file) result(xml_handler)
    !-----------------------------------------------------------------
    !< Get the filename of xdmf_file type
    !----------------------------------------------------------------- 
        class(xdmf_file_t), target, intent(IN) :: xdmf_file                !< XDMF file handler
        type(xmlf_t), pointer             :: xml_handler              !< Fox XML file handler
    !----------------------------------------------------------------- 
         xml_handler => xdmf_file%xml_handler
    end function xdmf_file_get_xml_handler


    function xdmf_file_get_document_root(xdmf_file) result(root)
    !-----------------------------------------------------------------
    !< Get the filename of xdmf_file type
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
        type(Node), pointer               :: root                     !< Fox DOM Node 
    !----------------------------------------------------------------- 
         root => xdmf_file%Root
    end function xdmf_file_get_document_root


    subroutine xdmf_file_openfile(xdmf_file, write_header, IO_error)
    !-----------------------------------------------------------------
    !< Open a XDMF file a returns a FoX **xml_handler**
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
        logical, optional,  intent(IN)    :: write_header             !< Flag to decide if to print header
        integer, optional,  intent(OUT)   :: IO_error                 !< IO error status
        logical                           :: header_flag              !< Real flag to decide if to print header
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
        header_flag = .true.; if(present(write_header)) header_flag = write_header

        if(header_flag) then
            call xml_OpenFile(filename=xdmf_file%get_filename(), xf=xdmf_file%xml_handler, &
                iostat=IO_error, preserve_whitespace=.false., pretty_print=.true., &
                minimize_overrun=.true., canonical=.false., replace=.true., addDecl=.true., &
                warning=.false., validate=.false., namespace=.true.)

            call xml_DeclareNamespace(xdmf_file%xml_handler, "http://www.w3.org/2001/XInclude", "xi")
            call xml_NewElement(xdmf_file%xml_handler, "Xdmf")
            call xml_AddAttribute(xdmf_file%xml_handler,"Version","2.1")
        else
            call xml_OpenFile(filename=xdmf_file%get_filename(), xf=xdmf_file%xml_handler, &
                iostat=IO_error, preserve_whitespace=.false., pretty_print=.true., &
                minimize_overrun=.true., canonical=.false., replace=.true., addDecl=.false., &
                warning=.false., validate=.false., namespace=.true.)
        endif

    end subroutine xdmf_file_openfile


    subroutine xdmf_file_parsefile(xdmf_file)
    !-----------------------------------------------------------------
    !< Parse a XDMF file with FoX DOM
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
    !-----------------------------------------------------------------
        xdmf_file%root => parseFile(xdmf_file%filename)
    end subroutine xdmf_file_parseFile


    subroutine xdmf_file_closefile(xdmf_file)
    !-----------------------------------------------------------------
    !< Manage the closing of a XDMF file and all the outstanding elements
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
    !-----------------------------------------------------------------
        ! empty : Empty files return warning instead of error
        call xml_Close(xf=xdmf_file%xml_handler, empty=.true.)
    end subroutine xdmf_file_closefile


    subroutine xdmf_file_free(xdmf_file)
    !-----------------------------------------------------------------
    !< Free a XDMF file derived type
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
    !-----------------------------------------------------------------
        if(allocated(xdmf_file%filename)) deallocate(xdmf_file%filename)
        nullify(xdmf_file%Root)
    end subroutine xdmf_file_free


!--------------------------------------------------------------------- -----------------------------------------------------------
end module xdmf_file
