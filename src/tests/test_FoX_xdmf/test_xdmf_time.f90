program test_xdmf_time

use fox_xdmf
use fox_dom
use PENF, only: R4P, R8P

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_time_t) :: time
    type(Node), pointer :: document_root, element
    type(NodeList), pointer :: element_list
    integer :: i


    call file%set_filename('test_xdmf_time.xmf')
    call file%openfile()
    call time%open(file%xml_handler); call time%close(file%xml_handler)
    call time%open(file%xml_handler, TimeType='Single'); call time%close(file%xml_handler)
    call time%open(file%xml_handler, TimeType='HyperSlab'); call time%close(file%xml_handler)
    call time%open(file%xml_handler, TimeType='List'); call time%close(file%xml_handler)
    call time%open(file%xml_handler, TimeType='list'); call time%close(file%xml_handler)
    call time%open(file%xml_handler, TimeType='Range'); call time%close(file%xml_handler)
    call time%open(file%xml_handler, TimeType='Unknown'); call time%close(file%xml_handler)
    call time%open(file%xml_handler, Value=1._R4P); call time%close(file%xml_handler)
    call time%open(file%xml_handler, Value=1._R8P); call time%close(file%xml_handler)
    call time%open(file%xml_handler, TimeType='Single', Value=1._R4P); call time%close(file%xml_handler)
    call file%closefile()

    call file%parsefile()
    document_root => getDocumentElement(file%get_document_root())

    if(hasChildNodes(document_root)) then
        element_list => getElementsByTagname(document_root, 'Time')
        do i = 0, getLength(element_list) - 1
            element => item(element_list, i)
            call time%parse(element)
            call time%print()
        enddo
    endif
    call time%free()

end program test_xdmf_time
