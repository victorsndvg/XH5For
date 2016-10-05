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
