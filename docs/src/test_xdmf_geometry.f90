program test_xdmf_geometry

use fox_xdmf
use fox_dom

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_geometry_t) :: geometry
    type(Node), pointer :: document_root, element
    type(NodeList), pointer :: element_list
    integer :: i


    call file%set_filename('test_xdmf_geometry.xmf')
    call file%openfile()
    call geometry%open(file%xml_handler); call geometry%close(file%xml_handler)
    call geometry%open(file%xml_handler, GeometryType='XYZ'); call geometry%close(file%xml_handler)
    call geometry%open(file%xml_handler, GeometryType='XY'); call geometry%close(file%xml_handler)
    call geometry%open(file%xml_handler, GeometryType='X_Y_Z'); call geometry%close(file%xml_handler)
    call geometry%open(file%xml_handler, GeometryType='VxVyVz'); call geometry%close(file%xml_handler)
    call geometry%open(file%xml_handler, GeometryType='Origin_DxDyDz'); call geometry%close(file%xml_handler)
    call geometry%open(file%xml_handler, GeometryType='Origin_DxDy'); call geometry%close(file%xml_handler)
    call geometry%open(file%xml_handler, GeometryType='Unknown'); call geometry%close(file%xml_handler)
    call file%closefile()


    call file%parsefile()
    document_root => getDocumentElement(file%get_document_root())

    if(hasChildNodes(document_root)) then
        element_list => getElementsByTagname(document_root, 'Geometry')
        do i = 0, getLength(element_list) - 1
            element => item(element_list, i)
            call geometry%parse(element)
            call geometry%print()
        enddo
    endif
    call geometry%free()



end program test_xdmf_geometry
