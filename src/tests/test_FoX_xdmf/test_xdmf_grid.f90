program test_xdmf_grid

use fox_xdmf

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_grid_t) :: grid


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
    call grid%open(file%xml_handler, Name='GridName', GridType='Uniform', CollectionType='Spatial',Section='DataItem'); call grid%close(file%xml_handler)
    call grid%close(file%xml_handler)
    call grid%close(file%xml_handler)
    call grid%close(file%xml_handler)
    call file%closefile()


end program test_xdmf_grid
