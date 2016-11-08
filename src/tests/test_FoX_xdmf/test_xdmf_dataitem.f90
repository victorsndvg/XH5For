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
program test_xdmf_dataitem

use fox_xdmf
use fox_dom
use PENF, only: I4P, I8P

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_dataitem_t) :: dataitem
    type(xdmf_domain_t) :: domain
    type(Node), pointer :: document_root, element
    type(NodeList), pointer :: element_list
    integer :: i

    call file%set_filename('test_xdmf_dataitem.xmf')
    call file%openfile()
    call domain%open(file%xml_handler)
    call dataitem%open(file%xml_handler); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Name='Nameattr1'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, ItemType='ItemType1'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, ItemType='Uniform'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, ItemType='Collection'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, ItemType='Tree'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, ItemType='HyperSlab'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, ItemType='Coordinates')
    call dataitem%open(file%xml_handler, ItemType='Function'); call dataitem%close(file%xml_handler)
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Dimensions=2_I4P); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Dimensions=(/1_I4P,3_I4P/)); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Dimensions=(/1_I8P,2_I8P,3_I8P/)); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Dimensions=(/1_I8P,1_I8P,1_I8P,1_I8P,1_I8P,1_I8P/)); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, NumberType='DoublePrecision'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, NumberType='Float'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, NumberType='Int'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, NumberType='Uint'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, NumberType='UInt'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, NumberType='Char'); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, NumberType='Uchar')
    call dataitem%open(file%xml_handler, NumberType='UChar')
    call dataitem%close(file%xml_handler)
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Precision=1)
    call dataitem%open(file%xml_handler, Precision=2)
    call dataitem%open(file%xml_handler, Precision=3)
    call dataitem%open(file%xml_handler, Precision=4)
    call dataitem%open(file%xml_handler, Precision=5)
    call dataitem%open(file%xml_handler, Precision=6)
    call dataitem%open(file%xml_handler, Precision=7)
    call dataitem%open(file%xml_handler, Precision=8)
    call dataitem%close(file%xml_handler)
    call dataitem%close(file%xml_handler)
    call dataitem%close(file%xml_handler)
    call dataitem%close(file%xml_handler)
    call dataitem%close(file%xml_handler)
    call dataitem%close(file%xml_handler)
    call dataitem%close(file%xml_handler)
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, FORMAT='XML')
    call dataitem%open(file%xml_handler, FORMAT='HDF')
    call dataitem%open(file%xml_handler, FORMAT='Binary')
    call dataitem%open(file%xml_handler, FORMAT='Unknown')
    call dataitem%open(file%xml_handler, Name='values', ItemType='Uniform', Dimensions=(/3_I8P/), NumberType='Float', Precision=4, Format='XML')
    call dataitem%close(file%xml_handler)
    call domain%open(file%xml_handler)
    call file%closefile()

    call file%parsefile()
    document_root => getDocumentElement(file%get_document_root())

    if(hasChildNodes(document_root)) then
        element_list => getElementsByTagname(document_root, "DataItem")
        do i = 0, getLength(element_list) - 1
            element => item(element_list, i)
            call dataitem%parse(element)
            call dataitem%print()
        enddo
    endif
    call dataitem%free()

end program test_xdmf_dataitem
