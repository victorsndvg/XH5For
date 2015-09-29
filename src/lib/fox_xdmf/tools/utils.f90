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


    function Count_tokens(s1)
    !-----------------------------------------------------------------
    !< Function for counting tokens of a string 
    !< @author David Frank  dave_frank@hotmail.com (http://home.earthlink.net/~dave_gemini/strings.f90)
    !-----------------------------------------------------------------
        character(len=*), intent(IN) :: s1
        character(len=len(s1))       :: s
        integer                      :: Count_tokens
        integer                      :: i, k
    !-----------------------------------------------------------------
        s = s1                            ! remove possible last char null (in C)
        k = 0  ; if (s /= ' ') k = 1      ! string has at least 1 item
        do i = 1,len_trim(s)-1
           if(s(i:i) /= ' '.and.s(i:i) /= ','.and.s(i+1:i+1) == ' '.or.s(i+1:i+1) == ',') k = k+1
        enddo
        Count_tokens = k
    end function Count_tokens

    function Next_token(s1, pos)
    !-----------------------------------------------------------------
    !< Return the next token given a initial position. The position
    !< is updated to reference the start of the next token
    !-----------------------------------------------------------------
        character(len=*), intent(IN)    :: s1
        integer,          intent(INOUT) :: pos
        character(len=len(s1))          :: s
        character(len=:), allocatable   :: Next_token
        integer                         :: i, k
    !-----------------------------------------------------------------
        s = s1    
        if(pos<=len(s)) then     
            k = len_trim(s)
            do i = pos,len_trim(s)-1
                if(s(i:i) /= ' '.and.s(i:i) /= ','.and.s(i+1:i+1) == ' '.or.s(i+1:i+1) == ',') then
                    k = i+1
                    exit
                endif
            enddo
            Next_token = s(pos:k)
            pos = k
        endif

    end function Next_token


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
