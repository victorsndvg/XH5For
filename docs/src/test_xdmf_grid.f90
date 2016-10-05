program test_xdmf_grid

use fox_xdmf
use fox_dom

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_grid_t) :: grid
    type(Node), pointer :: document_root, element
    type(NodeList), pointer :: element_list
    integer :: i


    call file%set_filename('test_xdmf_grid.xmf')
    call file%openfile()
    call grid%open(file%xml_handler); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, Name='GridName'); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, GridType='Uniform'); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, GridType='Collection'); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, GridType='Tree'); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, GridType='Subset')
    call grid%open(file%xml_handler, GridType='Unknown')
    call grid%open(file%xml_handler, CollectionType='Spatial'); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, CollectionType='Temporal'); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, CollectionType='Unknown')
    call grid%open(file%xml_handler, Section='DataItem'); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, Section='All'); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, Section='Unknown'); call grid%close(file%xml_handler)
    call grid%open(file%xml_handler, Name='GridName', GridType='Collection', CollectionType='Spatial',Section='DataItem'); call grid%close(file%xml_handler)
    call grid%close(file%xml_handler)
    call grid%close(file%xml_handler)
    call grid%close(file%xml_handler)
    call file%closefile()

    call file%parsefile()
    document_root => getDocumentElement(file%get_document_root())

    if(hasChildNodes(document_root)) then
        element_list => getElementsByTagname(document_root, 'Grid')
        do i = 0, getLength(element_list) - 1
            element => item(element_list, i)
            call grid%parse(element)
            call grid%print()
        enddo
    endif
    call grid%free()


end program test_xdmf_grid
