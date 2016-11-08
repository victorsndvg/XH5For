!-----------------------------------------------------------------
! XH5For (XDMF parallel partitioned mesh I/O on top of HDF5)
! Copyright (c) 2015 Santiago Badia, Alberto F. Martín, 
! Javier Principe and Víctor Sande.
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library.
!-----------------------------------------------------------------
module xdmf_utils

USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT ! Standard output/error logical units.
USE PENF, only: I4P, str
USE xdmf_parameters

implicit none
private

public :: Upper_Case
public :: Count_tokens
public :: Next_token
public :: is_in_option_list
public :: warning_message
public :: isSupportedTopologyTypeName
public :: isSupportedTimeTypeName
public :: isSupportedGridTypeName
public :: isSupportedGridCollectionTypeName
public :: isSupportedGridSectionName
public :: isSupportedAttributeTypeNAme
public :: isSupportedDataItemTypeName
public :: isSupportedAttributeCenterName
public :: isSupportedDataItemNumberTypeName
public :: isSupportedDataItemFormatName
public :: isSupportedDataItemPrecision

contains

    function Upper_Case(string)
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


    function Count_tokens(s1, separator)
    !-----------------------------------------------------------------
    !< Function for counting tokens of a string 
    !< Modified to define a custom a separator. If present, separator
    !< is the only allowed separator character
    !< @author David Frank  dave_frank@hotmail.com (http://home.earthlink.net/~dave_gemini/strings.f90)
    !-----------------------------------------------------------------
        character(len=*),           intent(IN) :: s1
        character(len=1), optional, intent(IN) :: separator
        character(len=len(s1))                 :: s
        integer(I4P)                           :: Count_tokens
        integer(I4P)                           :: i, k
    !-----------------------------------------------------------------
        s = s1                            ! remove possible last char null (in C)
        k = 0_I4P  ; if (s /= ' ') k = 1_I4P      ! string has at least 1 item
        if(present(separator) ) then
            do i = 1_I4P,len_trim(s, kind=I4P)-1_I4P
               if(s(i:i) /= separator .and.s(i+1:i+1) == separator) k = k+1
            enddo
        else
            do i = 1_I4P,len_trim(s, kind=I4P)-1_I4P
               if(s(i:i) /= ' '.and.s(i:i) /= ','.and.s(i+1_I4P:i+1_I4P) == ' '.or.s(i+1_I4P:i+1_I4P) == ',') k = k+1_I4P
            enddo
        endif
        Count_tokens = k
    end function Count_tokens


    function Next_token(s1, pos, separator)
    !-----------------------------------------------------------------
    !< Return the next token given a initial position. The position
    !< is updated to reference the start of the next token
    !-----------------------------------------------------------------
        character(len=*),           intent(IN)    :: s1
        integer(I4P),               intent(INOUT) :: pos
        character(len=1), optional, intent(IN)    :: separator
        character(len=len(s1))                    :: s
        character(len=:), allocatable             :: Next_token
        integer(I4P)                              :: i, k
    !-----------------------------------------------------------------
        s = s1    
        if(pos<=len(s, kind=I4P)) then     
            k = len_trim(s, kind=I4P)
            if(present(separator)) then
                do i = pos,len_trim(s, kind=I4P)-1_I4P
                    if(s(i:i) /= separator.and.s(i+1_I4P:i+1_I4P) == separator) then
                        k = i
                        exit
                    endif
                enddo
                if(s(pos:pos) == separator) pos = pos+1_I4P
            else
                do i = pos,len_trim(s)-1
                    if(s(i:i) /= ' '.and.s(i:i) /= ','.and.s(i+1_I4P:i+1_I4P) == ' '.or.s(i+1_I4P:i+1_I4P) == ',') then
                        k = i
                        exit
                    endif
                enddo
                if(s(pos:pos) == ' ' .or. s(pos:pos) == ',' ) pos = pos+1_I4P
            endif
            Next_token = s(pos:k)
            pos = k+1_I4P
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


    function isSupportedTopologyTypeName(TopologyTypeName) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid Topology TopologyType Name
    !----------------------------------------------------------------- 
        character(len=*),       intent(IN) :: TopologyTypeName        !< XDMF Topology TopologyType attribute
        logical                            :: supported               !< Valid TopologyType confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_TOPOLOGYTYPENAMES, option=TopologyTypeName, separator='&') 
        if(.not. supported) call warning_message('Not supported TopologyType Name: "'//TopologyTypeName//'" (Note: Case sensitive)')
    end function isSupportedTopologyTypeName


    function isSupportedTimeTypeName(TimeTypeName) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid Grid GridType Name
    !----------------------------------------------------------------- 
        character(len=*),   intent(IN) :: TimeTypeName                !< XDMF Grid TimeType attribute
        logical                        :: supported                   !< Valid TimeType confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_TIMETYPENAMES, option=TimeTypeName, separator='&') 
        if(.not. supported) call warning_message('Not supported TimeType Name: "'//TimeTypeName//'" (Note: Case sensitive)')
    end function isSupportedTimeTypeName


    function isSupportedGridTypeName(GridTypeName) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid Grid GridType Name
    !----------------------------------------------------------------- 
        character(len=*),       intent(IN) :: GridTypeName            !< XDMF Grid GridType attribute
        logical                            :: supported               !< Valid GridType confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_GRIDTYPENAMES, option=GridTypeName, separator='&') 
        if(.not. supported) call warning_message('Not supported GridType Name: "'//GridTypeName//'" (Note: Case sensitive)')
    end function isSupportedGridTypeName


    function isSupportedGridCollectionTypeName(CollectionTypeName) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid grid CollectionType Name
    !----------------------------------------------------------------- 
        character(len=*),       intent(IN) :: CollectionTypeName      !< XDMF Grid GridType attribute
        logical                            :: supported               !< Valid GridType confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_GRIDCOLLECTIONTYPENAMES, option=CollectionTypeName, separator='&') 
        if(.not. supported) call warning_message('Not supported CollectionType Name: "'//CollectionTypeName//'" (Note: Case sensitive)')
    end function isSupportedGridCollectionTypeName


    function isSupportedGridSectionName(SectionName) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid grid Section Name
    !----------------------------------------------------------------- 
        character(len=*),       intent(IN) :: SectionName             !< XDMF Grid Section attribute
        logical                            :: supported               !< Valid Section confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_GRIDCOLLECTIONSECTIONAMES, option=SectionName, separator='&') 
        if(.not. supported) call warning_message('Not supported Section Name: "'//SectionName//'" (Note: Case sensitive)')
    end function isSupportedGridSectionName


    function isSupportedAttributeTypeNAme(AttributeTypeName) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid Attribute AttributeType Name
    !----------------------------------------------------------------- 
        character(len=*),        intent(IN) :: AttributeTypeName      !< XDMF Attribute AttributeType attribute
        logical                             :: supported              !< Valid AttributeType confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_ATTRIBUTETYPENAMES, option=AttributeTypeName, separator='&') 
        if(.not. supported) call warning_message('Not supported AttributeType Name: "'//AttributeTypeName//'" (Note: Case sensitive)')
    end function isSupportedAttributeTypeName


    function isSupportedAttributeCenterName(CenterName) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid attribute Section Name
    !----------------------------------------------------------------- 
        character(len=*),        intent(IN) :: CenterName             !< XDMF Attribute Center attribute
        logical                             :: supported              !< Valid Center confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_ATTRIBUTECENTERNAMES, option=CenterName, separator='&') 
        if(.not. supported) call warning_message('Not supported AttributeCenter Name: "'//CenterName//'" (Note: Case sensitive)')
    end function isSupportedAttributeCenterName


    function isSupportedDataItemTypeName(ItemTypeNAme) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem ItemType Name
    !----------------------------------------------------------------- 
        character(len=*),       intent(IN) :: ItemTypeName            !< Dataitem ItemType 
        logical                            :: supported               !< Valid ItemType confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_DATAITEMTYPENAMES, option=ItemTypeName, separator='&') 
        if(.not. supported) call warning_message(msg='Not supported ItemType Name: "'//ItemTypeNAme//'" (Note: Case sensitive)')
    end function isSupportedDataItemTypeName


    function isSupportedDataItemNumberTypeName(NumberTypeName) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem NumberType Name
    !----------------------------------------------------------------- 
        character(len=*),       intent(IN) :: NumberTypeNAme          !< Dataitem NumberType 
        logical                            :: supported               !< Valid NumberType confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_DATAITEMNUMBERTYPENAMES, option=NumberTypeName, separator='&') 
        if(.not. supported) call warning_message('Not supported NumberType Name: "'//NumberTypeName//'" (Note: Case sensitive)')
    end function isSupportedDataItemNumberTypeName


    function isSupportedDataItemFormatName(FormatName) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem Format Name
    !----------------------------------------------------------------- 
        character(len=*),       intent(IN) :: FormatName              !< Dataitem Format 
        logical                            :: supported               !< Valid NumberType confirmation flag
    !----------------------------------------------------------------- 
        supported = is_in_option_list(option_list=SUPPORTED_DATAITEMFORMATNAMES, option=FormatName, separator='&') 
        if(.not. supported) call warning_message('Not supported Format Name: "'//FormatName//'" (Note: Case sensitive)')
    end function isSupportedDataItemFormatName


    function isSupportedDataItemPrecision(Precision) result(supported)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem Precision
    !----------------------------------------------------------------- 
        integer(I4P),           intent(IN) :: Precision               !< Dataitem Precision
        logical                            :: supported               !< Valid NumberType confirmation flag
    !----------------------------------------------------------------- 
        supported = MINVAL(ABS(SUPPORTED_DATAITEMPRECISIONS - Precision)) == 0_I4P
        if(.not. supported) call warning_message('Not supported Precision: "'//trim(str(no_sign=.true., n=Precision))//'"')
    end function isSupportedDataItemPrecision

end module xdmf_utils
