program test_xdmf_time

use fox_xdmf
use IR_Precision, only: R4P, R8P

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_time_t) :: time


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


end program test_xdmf_time
