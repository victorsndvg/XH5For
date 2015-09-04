program test_xdmf_geometry

use fox_xdmf

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_geometry_t) :: geometry


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


end program test_xdmf_geometry
