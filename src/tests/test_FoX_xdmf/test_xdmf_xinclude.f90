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
program test_xdmf_xinclude

use fox_xdmf
use fox_dom

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_xinclude_t) :: xinclude
    type(Node), pointer :: document_root, element
    type(NodeList), pointer :: element_list
    integer :: i


    call file%set_filename('test_xdmf_xinclude.xmf')
    call file%openfile()
    call xinclude%open(file%xml_handler); call xinclude%close(file%xml_handler)
    call xinclude%open(file%xml_handler, href="http://url"); call xinclude%close(file%xml_handler)
    call xinclude%open(file%xml_handler, href="localfile.xmf"); call xinclude%close(file%xml_handler)
    call file%closefile()

    call file%parsefile()
    document_root => getDocumentElement(file%get_document_root())

    if(hasChildNodes(document_root)) then
        element_list => getElementsByTagname(document_root, 'xi:include')
        do i = 0, getLength(element_list) - 1
            element => item(element_list, i)
            call xinclude%parse(element)
            call xinclude%print()
        enddo
    endif
    call xinclude%free()

end program test_xdmf_xinclude
