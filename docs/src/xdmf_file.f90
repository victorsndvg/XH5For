module xdmf_file
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF file handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use FoX_wxml
use FoX_dom, only: Node, parseFile
use PENF,    only: I4P

implicit none
private

    type xdmf_file_t
    !-----------------------------------------------------------------
    !< XDMF file handler type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: filename                     !< File name
        type(xmlf_t), public          :: xml_handler                  !< FoX SAX XML File handler
        logical                       :: open       = .false.         !< Flag to check if the file is open yet
        logical                       :: parsed     = .false.         !< Flag to check if the file is already parsed
        logical                       :: serialized = .false.         !< Flag to check if the file is serialized yet
        type(Node),       pointer     :: Root   => null()             !< FoX DOM node list pointing to XML root element
    !----------------------------------------------------------------- 
    contains
    private
        procedure, public :: openfile             => xdmf_file_openfile
        procedure, public :: isopen               => xdmf_file_isopen
        procedure, public :: isserialized         => xdmf_file_isserialized
        procedure, public :: setserialized        => xdmf_file_setserialized
        procedure, public :: parsefile            => xdmf_file_parsefile
        procedure, public :: isparsed             => xdmf_file_isparsed
        procedure, public :: setparsed            => xdmf_file_setparsed
        procedure, public :: closefile            => xdmf_file_closefile
        procedure, public :: set_filename         => xdmf_file_set_filename
        procedure, public :: get_filename         => xdmf_file_get_filename
        procedure, public :: get_xml_handler      => xdmf_file_get_xml_handler
        procedure, public :: get_document_root    => xdmf_file_get_document_root
        procedure, public :: free                 => xdmf_file_free
    end type xdmf_file_t

    public :: xdmf_file_t
    public :: xmlf_t

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
        class(xdmf_file_t),     intent(INOUT) :: xdmf_file            !< XDMF file handler
        logical,      optional, intent(IN)    :: write_header         !< Flag to decide if to print header
        integer(I4P), optional, intent(OUT)   :: IO_error             !< IO error status
        logical                               :: header_flag          !< Real flag to decide if to print header
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
        xdmf_file%open = .true.
    end subroutine xdmf_file_openfile


    subroutine xdmf_file_parsefile(xdmf_file)
    !-----------------------------------------------------------------
    !< Parse a XDMF file with FoX DOM
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
    !-----------------------------------------------------------------
        xdmf_file%root => parseFile(xdmf_file%filename)
        xdmf_file%parsed = .true.
    end subroutine xdmf_file_parseFile


    function xdmf_file_isopen(xdmf_file) result(isopen)
    !-----------------------------------------------------------------
    !< Check if the file is opened yet
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(IN) :: xdmf_file                   !< XDMF file handler
        logical                        :: isopen                      !< Flag to check if file is opened yet
    !-----------------------------------------------------------------
        isopen = xdmf_file%open
    end function xdmf_file_isopen


    subroutine xdmf_file_setserialized(xdmf_file)
    !-----------------------------------------------------------------
    !< Set file as already serialized
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
    !-----------------------------------------------------------------
        xdmf_file%serialized = .true.
    end subroutine xdmf_file_setserialized


    function xdmf_file_isserialized(xdmf_file) result(isserialized)
    !-----------------------------------------------------------------
    !< Check if the file is serialized yet
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(IN) :: xdmf_file                   !< XDMF file handler
        logical                        :: isserialized                !< Flag to check if file is serialized yet
    !-----------------------------------------------------------------
        isserialized = xdmf_file%serialized
    end function xdmf_file_isserialized


    subroutine xdmf_file_setparsed(xdmf_file)
    !-----------------------------------------------------------------
    !< Set file as already parsed
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
    !-----------------------------------------------------------------
        xdmf_file%parsed = .true.
    end subroutine xdmf_file_setparsed


    function xdmf_file_isparsed(xdmf_file) result(isparsed)
    !-----------------------------------------------------------------
    !< Check if the file was already parsed
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(IN) :: xdmf_file                   !< XDMF file handler
        logical                        :: isparsed                    !< Flag to check if file was already parsed
    !-----------------------------------------------------------------
        isparsed = xdmf_file%parsed
    end function xdmf_file_isparsed


    subroutine xdmf_file_closefile(xdmf_file)
    !-----------------------------------------------------------------
    !< Manage the closing of a XDMF file and all the outstanding elements
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
    !-----------------------------------------------------------------
        ! empty : Empty files return warning instead of error
        call xml_Close(xf=xdmf_file%xml_handler, empty=.true.)
        xdmf_file%open   = .false.
        xdmf_file%parsed = .false.
    end subroutine xdmf_file_closefile


    subroutine xdmf_file_free(xdmf_file)
    !-----------------------------------------------------------------
    !< Free a XDMF file derived type
    !----------------------------------------------------------------- 
        class(xdmf_file_t), intent(INOUT) :: xdmf_file                !< XDMF file handler
    !-----------------------------------------------------------------
        if(allocated(xdmf_file%filename)) deallocate(xdmf_file%filename)
        if(xdmf_file%isopen()) call xdmf_file%closefile()
        nullify(xdmf_file%Root)
        xdmf_file%open       = .false.
        xdmf_file%parsed     = .false.
        xdmf_file%serialized = .false.
    end subroutine xdmf_file_free


!--------------------------------------------------------------------- -----------------------------------------------------------
end module xdmf_file
