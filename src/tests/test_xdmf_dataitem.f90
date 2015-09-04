program test_xdmf_dataitem

use fox_xdmf
use IR_Precision, only: I8P

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_dataitem_t) :: dataitem
    type(xdmf_domain_t) :: domain

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
    call dataitem%open(file%xml_handler, Dimensions=(/2_I8P/)); call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Dimensions=(/1_I8P,3_I8P/)); call dataitem%close(file%xml_handler)
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


end program test_xdmf_dataitem
