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
