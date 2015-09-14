module xdmf_character_data
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF character_data handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, R4P, R8P, str
use FoX_wxml,     only: xml_AddCharacters, xmlf_t

implicit none

    type  :: xdmf_character_data_t
    !-----------------------------------------------------------------
    !< XDMF character_data type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: Name
    contains
    private
        procedure         :: character_data_open_ch
        procedure         :: character_data_open_I4P
        procedure         :: character_data_open_I8P
        procedure         :: character_data_open_I4P_1D
        procedure         :: character_data_open_I8P_1D
        procedure         :: character_data_open_I4P_2D
        procedure         :: character_data_open_I8P_2D
        procedure         :: character_data_open_R4P
        procedure         :: character_data_open_R8P
        procedure         :: character_data_open_R4P_1D
        procedure         :: character_data_open_R8P_1D
        procedure         :: character_data_open_R4P_2D
        procedure         :: character_data_open_R8P_2D
        procedure         :: default_initialization => character_data_default_initialization
        procedure         :: free                   => character_data_free
        generic,   public :: open                   => character_data_open_ch,     &
                                                       character_data_open_I4P,    &
                                                       character_data_open_I8P,    &
                                                       character_data_open_I4P_1D, &
                                                       character_data_open_I8P_1D, &
                                                       character_data_open_I4P_2D, &
                                                       character_data_open_I8P_2D, &
                                                       character_data_open_R4P,    &
                                                       character_data_open_R8P,    &
                                                       character_data_open_R4P_1D, &
                                                       character_data_open_R8P_1D, &
                                                       character_data_open_R4P_2D, &
                                                       character_data_open_R8P_2D

    end type xdmf_character_data_t

contains

    subroutine character_data_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF character_data type
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))        deallocate(this%Name)
    end subroutine character_data_free


    subroutine character_data_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF character_data with default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
    !----------------------------------------------------------------- 
        call this%free()
    end subroutine character_data_default_initialization


    subroutine character_data_open_ch(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with string data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        character(len=*),             intent(IN)    :: Data           !< XDMF character_data Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(data)))
    end subroutine character_data_open_ch


    subroutine character_data_open_I4P(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with I4P data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        integer(I4P),                 intent(IN)    :: Data           !< XDMF character_data I4P Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(str(no_sign=.true.,n=Data))))
    end subroutine character_data_open_I4P


    subroutine character_data_open_I8P(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with I8P data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        integer(I8P),                 intent(IN)    :: Data           !< XDMF character_data I8P Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(str(no_sign=.true.,n=Data))))
    end subroutine character_data_open_I8P

    subroutine character_data_open_I4P_1D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with I4P 1D array data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        integer(I4P),                 intent(IN)    :: Data(:)        !< XDMF character_data I4P 1D array Data attribute
        character(len=:), allocatable               :: char_dims      !< Aux String 
        integer(I4P)                                :: i              !< Aux index variable
    !----------------------------------------------------------------- 
        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Data,dim=1)
        allocate(character(len=64*i) :: char_dims)
        write(char_dims, fmt=*)   (trim(adjustl(str(no_sign=.true., n=Data(i))))//' ',i=1, size(Data,dim=1) )
        call xml_AddCharacters(xml_handler, trim(adjustl(char_dims)))
        deallocate(char_dims)
    end subroutine character_data_open_I4P_1D


    subroutine character_data_open_I8P_1D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with I8P 1D array data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        integer(I8P),                 intent(IN)    :: Data(:)        !< XDMF character_data I8P 1D array Data attribute
        character(len=:), allocatable               :: char_dims      !< Aux String 
        integer(I4P)                                :: i              !< Aux index variable
    !----------------------------------------------------------------- 
        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Data,dim=1)
        allocate(character(len=64*i) :: char_dims)
        write(char_dims, fmt=*)   (trim(adjustl(str(no_sign=.true., n=Data(i))))//' ',i=1, size(Data,dim=1) )
        call xml_AddCharacters(xml_handler, trim(adjustl(char_dims)))
        deallocate(char_dims)
    end subroutine character_data_open_I8P_1D


    subroutine character_data_open_I4P_2D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with I4P 2D array data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        integer(I4P),                 intent(IN)    :: Data(:,:)      !< XDMF character_data I4P 2D array Data attribute
        character(len=:), allocatable               :: char_dims      !< Aux String 
        integer(I4P)                                :: i, j           !< Aux index variable
    !----------------------------------------------------------------- 
        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Data,dim=1); j = size(Data,dim=2)
        allocate(character(len=64*i*j) :: char_dims)
        write(char_dims, fmt=*)   ((trim(adjustl(str(no_sign=.true., n=Data(i,j))))//' ',i=1, size(Data,dim=1) ), j=1, size(Data,dim=2))
        call xml_AddCharacters(xml_handler, trim(adjustl(char_dims)))
        deallocate(char_dims)
    end subroutine character_data_open_I4P_2D


    subroutine character_data_open_I8P_2D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with I8P 2D array data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        integer(I8P),                 intent(IN)    :: Data(:,:)      !< XDMF character_data I8P 2D array Data attribute
        character(len=:), allocatable               :: char_dims      !< Aux String 
        integer(I4P)                                :: i, j           !< Aux index variable
    !----------------------------------------------------------------- 
        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Data,dim=1); j = size(Data,dim=2)
        allocate(character(len=64*i*j) :: char_dims)
        write(char_dims, fmt=*)   ((trim(adjustl(str(no_sign=.true., n=Data(i,j))))//' ',i=1, size(Data,dim=1) ), j=1, size(Data,dim=2))
        call xml_AddCharacters(xml_handler, trim(adjustl(char_dims)))
        deallocate(char_dims)
    end subroutine character_data_open_I8P_2D


    subroutine character_data_open_R4P(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with R4P data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        real(R4P),                    intent(IN)    :: Data           !< XDMF character_data R4P Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(str(no_sign=.true.,n=Data))))
    end subroutine character_data_open_R4P


    subroutine character_data_open_R8P(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with R8P data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        real(R8P),                    intent(IN)    :: Data           !< XDMF character_data R8P Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(str(no_sign=.true.,n=Data))))
    end subroutine character_data_open_R8P

    subroutine character_data_open_R4P_1D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with R4P 1D array data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        real(R4P),                    intent(IN)    :: Data(:)        !< XDMF character_data R4P 1D array Data attribute
        character(len=:), allocatable               :: char_dims      !< Aux String 
        integer(I4P)                                :: i              !< Aux index variable
    !----------------------------------------------------------------- 
        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Data,dim=1)
        allocate(character(len=64*i) :: char_dims)
        write(char_dims, fmt=*)   (trim(adjustl(str(no_sign=.true., n=Data(i))))//' ',i=1, size(Data,dim=1) )
        call xml_AddCharacters(xml_handler, trim(adjustl(char_dims)))
        deallocate(char_dims)
    end subroutine character_data_open_R4P_1D


    subroutine character_data_open_R8P_1D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with R8P 1D array data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        real(R8P),                    intent(IN)    :: Data(:)        !< XDMF character_data R8P 1D array Data attribute
        character(len=:), allocatable               :: char_dims      !< Aux String 
        integer(I4P)                                :: i              !< Aux index variable
    !----------------------------------------------------------------- 
        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Data,dim=1)
        allocate(character(len=64*i) :: char_dims)
        write(char_dims, fmt=*)   (trim(adjustl(str(no_sign=.true., n=Data(i))))//' ',i=1, size(Data,dim=1) )
        call xml_AddCharacters(xml_handler, trim(adjustl(char_dims)))
        deallocate(char_dims)
    end subroutine character_data_open_R8P_1D


    subroutine character_data_open_R4P_2D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with R4P 2D array data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        real(R4P),                    intent(IN)    :: Data(:,:)      !< XDMF character_data R4P 2D array Data attribute
        character(len=:), allocatable               :: char_dims      !< Aux String 
        integer(I4P)                                :: i, j           !< Aux index variable
    !----------------------------------------------------------------- 
        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Data,dim=1); j = size(Data,dim=2)
        allocate(character(len=64*i*j) :: char_dims)
        write(char_dims, fmt=*)   ((trim(adjustl(str(no_sign=.true., n=Data(i,j))))//' ',i=1, size(Data,dim=1) ), j=1, size(Data,dim=2))
        call xml_AddCharacters(xml_handler, trim(adjustl(char_dims)))
        deallocate(char_dims)
    end subroutine character_data_open_R4P_2D


    subroutine character_data_open_R8P_2D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< Open a new character_data XDMF element with R8P 2D array data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        real(R8P),                    intent(IN)    :: Data(:,:)      !< XDMF character_data R8P 2D array Data attribute
        character(len=:), allocatable               :: char_dims      !< Aux String 
        integer(I4P)                                :: i, j           !< Aux index variable
    !----------------------------------------------------------------- 
        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Data,dim=1); j = size(Data,dim=2)
        allocate(character(len=64*i*j) :: char_dims)
        write(char_dims, fmt=*)   ((trim(adjustl(str(no_sign=.true., n=Data(i,j))))//' ',i=1, size(Data,dim=1) ), j=1, size(Data,dim=2))
        call xml_AddCharacters(xml_handler, trim(adjustl(char_dims)))
        deallocate(char_dims)
    end subroutine character_data_open_R8P_2D


end module xdmf_character_data
