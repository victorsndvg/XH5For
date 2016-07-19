module xdmf_character_data
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF character_data handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use PENF,         only: I4P, I8P, R4P, R8P, str
use FoX_wxml,     only: xml_AddCharacters, xmlf_t

implicit none
private

    type :: xdmf_character_data_t
    !-----------------------------------------------------------------
    !< XDMF character_data type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: Name
    contains
    private
        procedure         :: xdmf_character_data_write_ch
        procedure         :: xdmf_character_data_write_I4P
        procedure         :: xdmf_character_data_write_I8P
        procedure         :: xdmf_character_data_write_I4P_1D
        procedure         :: xdmf_character_data_write_I8P_1D
        procedure         :: xdmf_character_data_write_I4P_2D
        procedure         :: xdmf_character_data_write_I8P_2D
        procedure         :: xdmf_character_data_write_R4P
        procedure         :: xdmf_character_data_write_R8P
        procedure         :: xdmf_character_data_write_R4P_1D
        procedure         :: xdmf_character_data_write_R8P_1D
        procedure         :: xdmf_character_data_write_R4P_2D
        procedure         :: xdmf_character_data_write_R8P_2D
        generic,   public :: write                  => xdmf_character_data_write_ch,     &
                                                       xdmf_character_data_write_I4P,    &
                                                       xdmf_character_data_write_I8P,    &
                                                       xdmf_character_data_write_I4P_1D, &
                                                       xdmf_character_data_write_I8P_1D, &
                                                       xdmf_character_data_write_I4P_2D, &
                                                       xdmf_character_data_write_I8P_2D, &
                                                       xdmf_character_data_write_R4P,    &
                                                       xdmf_character_data_write_R8P,    &
                                                       xdmf_character_data_write_R4P_1D, &
                                                       xdmf_character_data_write_R8P_1D, &
                                                       xdmf_character_data_write_R4P_2D, &
                                                       xdmf_character_data_write_R8P_2D

    end type xdmf_character_data_t

public :: xdmf_character_data_t

contains

    subroutine xdmf_character_data_write_ch(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with string data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        character(len=*),             intent(IN)    :: Data           !< XDMF character_data Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(data)))
    end subroutine xdmf_character_data_write_ch


    subroutine xdmf_character_data_write_I4P(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with I4P data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        integer(I4P),                 intent(IN)    :: Data           !< XDMF character_data I4P Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(str(no_sign=.true.,n=Data))))
    end subroutine xdmf_character_data_write_I4P


    subroutine xdmf_character_data_write_I8P(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with I8P data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        integer(I8P),                 intent(IN)    :: Data           !< XDMF character_data I8P Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(str(no_sign=.true.,n=Data))))
    end subroutine xdmf_character_data_write_I8P

    subroutine xdmf_character_data_write_I4P_1D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with I4P 1D array data
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
    end subroutine xdmf_character_data_write_I4P_1D


    subroutine xdmf_character_data_write_I8P_1D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with I8P 1D array data
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
    end subroutine xdmf_character_data_write_I8P_1D


    subroutine xdmf_character_data_write_I4P_2D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with I4P 2D array data
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
    end subroutine xdmf_character_data_write_I4P_2D


    subroutine xdmf_character_data_write_I8P_2D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with I8P 2D array data
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
    end subroutine xdmf_character_data_write_I8P_2D


    subroutine xdmf_character_data_write_R4P(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with R4P data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        real(R4P),                    intent(IN)    :: Data           !< XDMF character_data R4P Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(str(no_sign=.true.,n=Data))))
    end subroutine xdmf_character_data_write_R4P


    subroutine xdmf_character_data_write_R8P(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with R8P data
    !----------------------------------------------------------------- 
        class(xdmf_character_data_t), intent(INOUT) :: this           !< XDMF character_data type
        type(xmlf_t),                 intent(INOUT) :: xml_handler    !< FoX XML File handler
        real(R8P),                    intent(IN)    :: Data           !< XDMF character_data R8P Data attribute
    !----------------------------------------------------------------- 
        call xml_AddCharacters(xml_handler, trim(adjustl(str(no_sign=.true.,n=Data))))
    end subroutine xdmf_character_data_write_R8P

    subroutine xdmf_character_data_write_R4P_1D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with R4P 1D array data
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
    end subroutine xdmf_character_data_write_R4P_1D


    subroutine xdmf_character_data_write_R8P_1D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with R8P 1D array data
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
    end subroutine xdmf_character_data_write_R8P_1D


    subroutine xdmf_character_data_write_R4P_2D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with R4P 2D array data
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
    end subroutine xdmf_character_data_write_R4P_2D


    subroutine xdmf_character_data_write_R8P_2D(this, xml_handler, Data)
    !-----------------------------------------------------------------
    !< write a new character_data XDMF element with R8P 2D array data
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
    end subroutine xdmf_character_data_write_R8P_2D


end module xdmf_character_data
