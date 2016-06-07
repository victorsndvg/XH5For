module xdmf_contiguous_hyperslab_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF File handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use fox_xdmf
use xdmf_handler
use xh5for_utils
use xh5for_parameters
use IR_Precision, only: I4P, I8P, R4P, R8P, str

implicit none

private

    type, abstract, extends(xdmf_handler_t) :: xdmf_contiguous_hyperslab_handler_t
    !-----------------------------------------------------------------
    !< XDMF contiguous HyperSlab handler implementation
    !----------------------------------------------------------------- 
    contains
    private
        procedure         :: WriteAttributes              => xdmf_contiguous_hyperslab_handler_WriteAttributes
    end type xdmf_contiguous_hyperslab_handler_t

public :: xdmf_contiguous_hyperslab_handler_t

contains

    subroutine xdmf_contiguous_hyperslab_handler_WriteAttributes(this, GridID)
    !-----------------------------------------------------------------
    !< Writes a XDMF Attribute into a opened file for the contiguous HyperSlab strategy
    !< @NOTE: only nodal attributes
    !< @TODO: add cell, face and grid centered attributes
    !----------------------------------------------------------------- 
        class(xdmf_contiguous_hyperslab_handler_t), intent(INOUT) :: this                   !< XDMF contiguous hyperslab handler
        integer(I4P),                               intent(IN)    :: GridID                 !< Grid ID number
        type(xdmf_attribute_t)                                    :: attribute              !< XDMF Attribute type
        type(xdmf_dataitem_t)                                     :: dataitem               !< XDMF Dataitem type
        type(xdmf_character_data_t)                               :: chardata               !< XDMF Character Data type
        integer(I8P)                                              :: LocalNumberOfData      !< Local number of data
        integer(I8P)                                              :: GlobalNumberOfData     !< Global number of nodes
        integer(I8P)                                              :: DataOffset             !< DataOffset
        integer(I4P)                                              :: NumberOfComponents     !< Number of components given attribute type
        character(len=:), allocatable                             :: XDMFAttributeTypeName  !< String Attibute type identifier
        character(len=:), allocatable                             :: XDMFCenterTypeName     !< String Attribute Center identifier
        integer(I4P)                                              :: DimensionsSize         !< Size of the attribute shape
        integer(I4P)                                              :: indx           
    !-----------------------------------------------------------------
        if(this%MPIEnvironment%is_root()) then
            do indx = 1, this%UniformGridDescriptor%GetNumberOfAttributes()
                call this%CalculateAttributeDimensions(                                           & 
                    GridID = GridID,                                                              &
                    Center = this%UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx), &
                    GlobalNumberOfData = GlobalNumberOfData,                                      &
                    LocalNumberOfData = LocalNumberOfData,                                        &
                    DataOffset = DataOffset)
                NumberOfComponents = GetNumberOfComponentsFromAttributeType( &
                                        this%UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFAttributeTypeName = GetXDMFAttributeTypeName( &
                                        this%UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFCenterTypeName = GetXDMFCenterTypeName( &
                                        this%UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx))
                DimensionsSize = size(this%UniformGridDescriptor%GetAttributeArrayDimensions(AttributeNumber=indx), dim=1)
                call attribute%open(xml_handler = this%SpatialFile%xml_handler,                                   &
                        Name          = this%UniformGridDescriptor%GetAttributeName(AttributeNumber=indx), &
                        AttributeType = XDMFAttributeTypeName,                                             &
                        Center        = XDMFCenterTypeName)
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler,                                                     &
                        Dimensions = (/int(LocalNumberOfData,I8P), int(NumberOfComponents,I8P), int(DimensionsSize,I8P)/),  &
                        ItemType   = 'HyperSlab',                                                                           &
                        Format     = 'HDF')
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler, &
                        Dimensions = (/3_I4P, DimensionsSize/),         &
                        NumberType = 'Int',                             &
                        Format     = 'XML',                             &
                        Precision=4) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = (/DataOffset*int(NumberOfComponents,I8P),1_I8P,LocalNumberOfData*int(NumberOfComponents,I8P)/))
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%open(xml_handler = this%SpatialFile%xml_handler,                                    &
                        Dimensions = (/int(GlobalNumberOfData,I8P)*int(NumberOfComponents,I8P)/),          &
                        NumberType = this%UniformGridDescriptor%GetAttributeDataType(AttributeNumber=indx), &
                        Format     = 'HDF',                                                                &
                        Precision  = this%UniformGridDescriptor%GetAttributePrecision(AttributeNumber=indx)) 
                call chardata%write( xml_handler = this%SpatialFile%xml_handler, &
                        Data = trim(adjustl(this%prefix))//'.h5'//':'//&
                                        this%UniformGridDescriptor%GetAttributeName(AttributeNumber=indx))
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call dataitem%close(xml_handler = this%SpatialFile%xml_handler)
                call attribute%close(xml_handler = this%SpatialFile%xml_handler)
            enddo
        endif                    
    end subroutine xdmf_contiguous_hyperslab_handler_WriteAttributes

end module xdmf_contiguous_hyperslab_handler
