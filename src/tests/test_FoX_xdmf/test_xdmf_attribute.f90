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
program test_xdmf_attribute

use fox_xdmf
use fox_dom

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_attribute_t) :: attribute
    type(Node), pointer :: document_root, element
    type(NodeList), pointer :: element_list
    integer :: i


    call file%set_filename('test_xdmf_attribute.xmf')
    call file%openfile()
    call attribute%open(file%xml_handler); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, Name='AttributeName'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, AttributeType='Scalar'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, AttributeType='Vector'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, AttributeType='vector'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, AttributeType='Tensor'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, AttributeType='Tensor6'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, AttributeType='Matrix'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, AttributeType='GlobalID'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, AttributeType='Unknown'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, Center='Node'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, Center='Cell'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, Center='Grid'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, Center='Face'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, Center='Edge'); call attribute%close(file%xml_handler)
    call attribute%open(file%xml_handler, AttributeType='Scalar', Center='Node'); call attribute%close(file%xml_handler)
    call file%closefile()

    call file%parsefile()
    document_root => getDocumentElement(file%get_document_root())

    if(hasChildNodes(document_root)) then
        element_list => getElementsByTagname(document_root, "Attribute")
        do i = 0, getLength(element_list) - 1
            element => item(element_list, i)
            call attribute%parse(element)
            call attribute%print()
        enddo
    endif
    call attribute%free()

end program test_xdmf_attribute
