module xdmf_utils

USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT ! Standard output/error logical units.

implicit none

contains

    elemental function Upper_Case(string)
    !-----------------------------------------------------------------
    !< Function for converting lower case characters of a string to 
    !< upper case ones.
    !< @author Stephano Zaghi (https://github.com/szaghi)
    !-----------------------------------------------------------------
    implicit none
    character(len=*), intent(IN):: string     !< String to be converted.
    character(len=len(string))::   Upper_Case !< Converted string.
    integer::                      n1         !< Characters counter.
    !-----------------------------------------------------------------
    Upper_Case = string
    do n1=1,len(string)
        select case(ichar(string(n1:n1)))
            case(97:122)
            Upper_Case(n1:n1)=char(ichar(string(n1:n1))-32) ! Upper case conversion
        endselect
    enddo
    return
    end function Upper_Case


    elemental function is_in_option_list(option_list, option, separator) 
    !-----------------------------------------------------------------
    !< Return True if *option* is a substring of *option_list*
    !< *option_list* is a string composed by options delimited
    !< by separators.
    !< @Note Case sensitive
    !----------------------------------------------------------------- 
        character(len=*),           intent(IN) :: option_list         !< List of allowed options
        character(len=*),           intent(IN) :: option              !< Option to search
        character(len=*), optional, intent(IN) :: separator           !< Separator between options
        logical                                :: is_in_option_list   !< Option founded flag
        character(len=:), allocatable          :: sep                 !< Aux separator variable
    !----------------------------------------------------------------- 
        sep = ' '; if(present(separator)) sep = separator

        is_in_option_list = .false.
        if(INDEX(adjustl(trim(option)), adjustl(trim(sep))) == 0) &
            is_in_option_list = INDEX(option_list, adjustl(trim(option))) > 0
    end function is_in_option_list

    subroutine warning_message(msg)
    !-----------------------------------------------------------------
    !< Writes a warning message to the output unit
    !-----------------------------------------------------------------
        character(len=*),           intent(IN) :: msg                 !< Warning message
    !-----------------------------------------------------------------
        write(unit=stdout, fmt=*) 'WARNING: '//trim(adjustl(msg))
    end subroutine


end module xdmf_utils
