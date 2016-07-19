module xdmf_dataset_per_process_handler
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF (dataset per process strategy) File handling module
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


    type, abstract, extends(xdmf_handler_t) :: xdmf_dataset_per_process_handler_t
    !-----------------------------------------------------------------
    !< XDMF dataset per process handler implementation
    !----------------------------------------------------------------- 
    contains
    private
        procedure         :: WriteAttributes              => xdmf_dataset_per_process_handler_WriteAttributes
    end type xdmf_dataset_per_process_handler_t

public :: xdmf_dataset_per_process_handler_t

contains

    subroutine xdmf_dataset_per_process_handler_WriteAttributes(this, GridID)
    !-----------------------------------------------------------------
    !< Writes a XDMF Attribute into a opened file for the dataset per process strategy
    !< @NOTE: only nodal attributes
    !< @TODO: add cell, face and grid centered attributes
    !----------------------------------------------------------------- 
        class(xdmf_dataset_per_process_handler_t), intent(INOUT) :: this                   !< XDMF dataset per process handler
        integer(I4P),                              intent(IN)    :: GridID                 !< Grid ID number
        type(mpi_env_t),                  pointer                :: MPIEnvironment         !< MPI environment
        class(uniform_grid_descriptor_t), pointer                :: UniformGridDescriptor  !< Uniform grid descriptor
        type(xdmf_attribute_t)                                   :: attribute              !< XDMF Attribute type
        type(xdmf_dataitem_t)                                    :: dataitem               !< XDMF Dataitem type
        type(xdmf_character_data_t)                              :: chardata               !< XDMF Character Data type
        integer(I8P)                                             :: LocalNumberOfData      !< Local number of data
        integer(I8P)                                             :: GlobalNumberOfData     !< Global number of nodes
        integer(I8P)                                             :: DataOffset             !< DataOffset
        integer(I4P)                                             :: NumberOfComponents     !< Number of components given attribute type
        character(len=:), allocatable                            :: XDMFAttributeTypeName  !< String Attibute type identifier
        character(len=:), allocatable                            :: XDMFCenterTypeName     !< String Attribute Center identifier
        integer(I4P)                                             :: DimensionsSize         !< Size of the attribute shape
        integer(I4P)                                             :: indx           
    !-----------------------------------------------------------------
        MPIEnvironment => this%GetMPIEnvironment()
        assert(associated(MPIEnvironment))
        if(MPIEnvironment%is_root()) then
            UniformGridDescriptor => this%GetUniformGridDescriptor()
            assert(associated(UniformGridDescriptor))
            do indx = 1, UniformGridDescriptor%GetNumberOfAttributes()
                call this%CalculateAttributeDimensions(                                           & 
                    GridID = GridID,                                                              &
                    Center = UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx), &
                    GlobalNumberOfData = GlobalNumberOfData,                                      &
                    LocalNumberOfData = LocalNumberOfData,                                        &
                    DataOffset = DataOffset)
                NumberOfComponents = GetNumberOfComponentsFromAttributeType( &
                                        UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFAttributeTypeName = GetXDMFAttributeTypeName( &
                                        UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFCenterTypeName = GetXDMFCenterTypeName( &
                                        UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx))
                DimensionsSize = size(UniformGridDescriptor%GetAttributeArrayDimensions(AttributeNumber=indx), dim=1)
                call attribute%open(xml_handler = this%GetSpatialFileXMLHandler(),                         &
                        Name          = UniformGridDescriptor%GetAttributeName(AttributeNumber=indx), &
                        AttributeType = XDMFAttributeTypeName,                                        &
                        Center        = XDMFCenterTypeName)
                call dataitem%open(xml_handler = this%GetSpatialFileXMLHandler(),                      &
                        Dimensions = (/int(LocalNumberOfData,I8P),int(NumberOfComponents,I8P),1_I8P/), &
                        NumberType = UniformGridDescriptor%GetAttributeDataType(AttributeNumber=indx), &
                        Format     = 'HDF',                                                            &
                        Precision  = UniformGridDescriptor%GetAttributePrecision(AttributeNumber=indx)) 
                call chardata%write( xml_handler = this%GetSpatialFileXMLHandler(), &
                        Data = this%GetHDF5Filename()//':'//&
                                        UniformGridDescriptor%GetAttributeName(AttributeNumber=indx)//&
                                        '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
                call dataitem%close(xml_handler = this%GetSpatialFileXMLHandler())
                call attribute%close(xml_handler = this%GetSpatialFileXMLHandler())
            enddo
        endif                    
    end subroutine xdmf_dataset_per_process_handler_WriteAttributes

end module xdmf_dataset_per_process_handler
