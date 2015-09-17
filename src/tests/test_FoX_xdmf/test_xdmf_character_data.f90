program test_xdmf_character_data

use fox_xdmf
use IR_Precision, only: I4P, I8P, R4P, R8P

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_dataitem_t) :: dataitem
    type(xdmf_character_data_t) :: character_data

    call file%set_filename('test_xdmf_character_data.xmf')
    call file%openfile()
    call dataitem%open(file%xml_handler, Name='String data')
    call character_data%open(file%xml_handler, 'String')
    call dataitem%close(file%xml_handler)

    call dataitem%open(file%xml_handler, Name='I4P data')
    call character_data%open(file%xml_handler, 1_I4P)
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Name='I8P data')
    call character_data%open(file%xml_handler, 1_I8P)
    call dataitem%close(file%xml_handler)

    call dataitem%open(file%xml_handler, Name='R4P data')
    call character_data%open(file%xml_handler, 1.0_R4P)
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Name='R8P data')
    call character_data%open(file%xml_handler, 1.0_R8P)
    call dataitem%close(file%xml_handler)

    call dataitem%open(file%xml_handler, Name='I4P 1D data')
    call character_data%open(file%xml_handler, (/1_I4P, 2_I4P/))
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Name='I8P 1D data')
    call character_data%open(file%xml_handler, (/1_I8P, 3_I8P/))
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Name='R4P 1D data')
    call character_data%open(file%xml_handler, (/1.0_R4P, 4.0_R4P/))
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Name='R8P 1D data')
    call character_data%open(file%xml_handler, (/1.0_R8P, 5.0_R8P/))
    call dataitem%close(file%xml_handler)

    call dataitem%open(file%xml_handler, Name='I4P 2D data')
    call character_data%open(file%xml_handler, RESHAPE ((/1_I4P, 2_I4P, 3_I4P, 4_I4P, 5_I4P, 6_I4P/), (/2,3/)))
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Name='I8P 2D data')
    call character_data%open(file%xml_handler, RESHAPE ((/1_I8P, 2_I8P, 3_I8P, 4_I8P, 123456789_I8P, 6_I8P/), (/2,3/)))
    call dataitem%close(file%xml_handler)


    call dataitem%open(file%xml_handler, Name='R4P 2D data')
    call character_data%open(file%xml_handler, RESHAPE ((/1.0_R4P, 2.0_R4P, 3.0_R4P, 4.0_R4P/), (/2,2/)))
    call dataitem%close(file%xml_handler)
    call dataitem%open(file%xml_handler, Name='R8P 2D data')
    call character_data%open(file%xml_handler, RESHAPE ((/1.0_R8P, 2.0_R8P, 3.0_R8P, 4.0_R8P/), (/2,2/)))
    call dataitem%close(file%xml_handler)

    call file%closefile()


end program test_xdmf_character_data
