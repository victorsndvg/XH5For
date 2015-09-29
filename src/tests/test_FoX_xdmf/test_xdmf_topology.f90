program test_xdmf_topology

use fox_xdmf
use fox_dom
use IR_Precision, only: I4P, I8P

implicit none

    type(xdmf_file_t) :: file
    type(xdmf_topology_t) :: topology
    type(Node), pointer :: document_root, element
    type(NodeList), pointer :: element_list
    integer :: i

! Name               (no default)
! TopologyType       Polyvertex | Polyline | Polygon |
!                    Triangle | Quadrilateral | Tetrahedron | Pyramid| Wedge | Hexahedron |
!                    Edge_3 | Triagle_6 | Quadrilateral_8 | Tetrahedron_10 | Pyramid_13 |
!                    Wedge_15 | Hexahedron_20 |
!                    Mixed |
!                    2DSMesh | 2DRectMesh | 2DCoRectMesh |
!                    3DSMesh | 3DRectMesh | 3DCoRectMesh
! NodesPerElement    (no default) Only Important for Polyvertex, Polygon and Polyline
! NumberOfElement    (no default)
!     OR
! Dimensions         (no default)
! Order              each cell type has its own default
! BaseOffset         0 | #

    call file%set_filename('test_xdmf_topology.xmf')
    call file%openfile()
    call topology%open(file%xml_handler); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Polyvertex'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Polyline'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Polygon'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Triangle'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Quadrilateral'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Tetrahedron'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Pyramid'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Wedge'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Hexahedron'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Edge_3'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Triangle_6'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Quadrilateral_8'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Tetrahedron_10'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Pyramid_13'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Wedge_15'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Hexahedron_20'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Mixed'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='2DSMesh'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='2DRectMesh'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='2DCoRectMesh'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='3DSMesh'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='3DRectMesh'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='3DCoRectMesh'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Unknown'); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,NodesPerElement=3); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,Dimensions=1_I4P); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,Dimensions=1_I8P); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,Dimensions=(/1_I4P,1_I4P,1_I4P/)); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,Dimensions=(/1_I8P,1_I8P,1_I8P/)); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,Order=1); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,BaseOffset=0); call topology%close(file%xml_handler)
    call topology%open(file%xml_handler,TopologyType='Triangle',NodesPerElement=3,Dimensions=(/3_I8P,9007199254740992_I8P/),Order=1,BaseOffset=0); call topology%close(file%xml_handler)
    call file%closefile()

    call file%parsefile()
    document_root => getDocumentElement(file%get_document_root())

    if(hasChildNodes(document_root)) then
        element_list => getElementsByTagname(document_root, 'Topology')
        do i = 0, getLength(element_list) - 1
            element => item(element_list, i)
            call topology%parse(element)
            call topology%print()
        enddo
    endif
    call topology%free()

end program test_xdmf_topology
