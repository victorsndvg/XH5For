module xdmf_contiguous_hyperslab_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF (contiguous hyperslab strategy) File handling module
!--------------------------------------------------------------------- -----------------------------------------------------------

use fox_xdmf
use xdmf_handler
use xh5for_utils
use xh5for_parameters
use mpi_environment
use uniform_grid_descriptor
use PENF, only: I4P, I8P, R4P, R8P, str

implicit none

#include "assert.i90"

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
        type(mpi_env_t) ,                 pointer                 :: MPIEnvironment         !< MPI environment
        class(uniform_grid_descriptor_t), pointer                 :: UniformGridDescriptor  !< Uniform grid descriptor
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
        MPIEnvironment => this%GetMPIEnvironment()
        assert(associated(MPIEnvironment))
        if(MPIEnvironment%is_root()) then
            UniformGridDescriptor => this%GetUniformGridDescriptor()
            assert(associated(UniformGridDescriptor))
            do indx = 1, UniformGridDescriptor%GetNumberOfAttributes()
                call this%CalculateAttributeDimensions(                                      & 
                    GridID = GridID,                                                         &
                    Center = UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx), &
                    GlobalNumberOfData = GlobalNumberOfData,                                 &
                    LocalNumberOfData = LocalNumberOfData,                                   &
                    DataOffset = DataOffset)
                NumberOfComponents = GetNumberOfComponentsFromAttributeType( &
                                        UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFAttributeTypeName = GetXDMFAttributeTypeName( &
                                        UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFCenterTypeName = GetXDMFCenterTypeName( &
                                        UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx))
                DimensionsSize = size(UniformGridDescriptor%GetAttributeArrayDimensions(AttributeNumber=indx), dim=1)
                call attribute%open(xml_handler = this%GetSpatialFileXMLHandler(),                    &
                        Name          = UniformGridDescriptor%GetAttributeName(AttributeNumber=indx), &
                        AttributeType = XDMFAttributeTypeName,                                        &
                        Center        = XDMFCenterTypeName)
                call dataitem%open(xml_handler = this%GetSpatialFileXMLHandler(),                                           &
                        Dimensions = (/int(LocalNumberOfData,I8P), int(NumberOfComponents,I8P), int(DimensionsSize,I8P)/),  &
                        ItemType   = 'HyperSlab',                                                                           &
                        Format     = 'HDF')
                call dataitem%open(xml_handler = this%GetSpatialFileXMLHandler(), &
                        Dimensions = (/3_I4P, DimensionsSize/),         &
                        NumberType = 'Int',                             &
                        Format     = 'XML',                             &
                        Precision=4) 
                call chardata%write( xml_handler = this%GetSpatialFileXMLHandler(), &
                        Data = (/DataOffset*int(NumberOfComponents,I8P),1_I8P,LocalNumberOfData*int(NumberOfComponents,I8P)/))
                call dataitem%close(xml_handler = this%GetSpatialFileXMLHandler())
                call dataitem%open(xml_handler = this%GetSpatialFileXMLHandler(),                      &
                        Dimensions = (/int(GlobalNumberOfData,I8P)*int(NumberOfComponents,I8P)/),      &
                        NumberType = UniformGridDescriptor%GetAttributeDataType(AttributeNumber=indx), &
                        Format     = 'HDF',                                                            &
                        Precision  = UniformGridDescriptor%GetAttributePrecision(AttributeNumber=indx)) 
                call chardata%write( xml_handler = this%GetSpatialFileXMLHandler(), &
                        Data = this%GetHDF5FileName()//':'//UniformGridDescriptor%GetAttributeName(AttributeNumber=indx))
                call dataitem%close(xml_handler = this%GetSpatialFileXMLHandler())
                call dataitem%close(xml_handler = this%GetSpatialFileXMLHandler())
                call attribute%close(xml_handler = this%GetSpatialFileXMLHandler())
            enddo
        endif                    
    end subroutine xdmf_contiguous_hyperslab_handler_WriteAttributes

end module xdmf_contiguous_hyperslab_handler
