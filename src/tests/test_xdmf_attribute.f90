program test_xdmf_attribute

use fox_xdmf

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_attribute_t) :: attribute


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


end program test_xdmf_attribute
