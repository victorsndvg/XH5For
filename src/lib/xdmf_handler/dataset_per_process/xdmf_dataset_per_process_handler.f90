!-----------------------------------------------------------------
! XH5For (XDMF parallel partitioned mesh I/O on top of HDF5)
! Copyright (c) 2015 Santiago Badia, Alberto F. Martín, 
! Javier Principe and Víctor Sande.
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library.
!-----------------------------------------------------------------
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
        type(xmlf_t),                     pointer                :: XMLHandler             !< XDMF file handler
        type(xdmf_attribute_t)                                   :: attribute              !< XDMF Attribute type
        type(xdmf_dataitem_t)                                    :: dataitem               !< XDMF Dataitem type
        type(xdmf_character_data_t)                              :: chardata               !< XDMF Character Data type
        integer(I8P)                                             :: LocalNumberOfData      !< Local number of data
        integer(I8P)                                             :: GlobalNumberOfData     !< Global number of nodes
        integer(I8P)                                             :: DataOffset             !< DataOffset
        integer(I4P)                                             :: NumberOfComponents     !< Number of components given attribute type
        character(len=:), allocatable                            :: XDMFAttributeTypeName  !< String Attibute type identifier
        character(len=:), allocatable                            :: XDMFCenterTypeName     !< String Attribute Center identifier
        integer(I4P)                                             :: indx           
    !-----------------------------------------------------------------
        MPIEnvironment => this%GetMPIEnvironment()
        assert(associated(MPIEnvironment))
        if(MPIEnvironment%is_root()) then
            UniformGridDescriptor => this%GetUniformGridDescriptor()
            XMLHandler            => this%GetSpatialFileXMLHandler()
            assert(associated(UniformGridDescriptor) .and. associated(XMLHandler))
            do indx = 1_I4P, UniformGridDescriptor%GetNumberOfAttributes()
                call this%CalculateAttributeDimensions(                                           & 
                    GridID = GridID,                                                              &
                    Center = UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx),      &
                    GlobalNumberOfData = GlobalNumberOfData,                                      &
                    LocalNumberOfData = LocalNumberOfData,                                        &
                    DataOffset = DataOffset)
                NumberOfComponents = GetNumberOfComponentsFromAttributeType( &
                                        UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFAttributeTypeName = GetXDMFAttributeTypeName( &
                                        UniformGridDescriptor%GetAttributeType(AttributeNumber=indx))
                XDMFCenterTypeName = GetXDMFCenterTypeName( &
                                        UniformGridDescriptor%GetAttributeCenter(AttributeNumber=indx))
                call attribute%open(xml_handler = XMLHandler,                                         &
                        Name          = UniformGridDescriptor%GetAttributeName(AttributeNumber=indx), &
                        AttributeType = XDMFAttributeTypeName,                                        &
                        Center        = XDMFCenterTypeName)
                call dataitem%open(xml_handler = XMLHandler,                                           &
                        Dimensions = (/int(LocalNumberOfData,I8P),int(NumberOfComponents,I8P),1_I8P/), &
                        NumberType = UniformGridDescriptor%GetAttributeDataType(AttributeNumber=indx), &
                        Format     = 'HDF',                                                            &
                        Precision  = UniformGridDescriptor%GetAttributePrecision(AttributeNumber=indx)) 
                call chardata%write( xml_handler = XMLHandler, &
                        Data = this%GetHDF5Filename()//':'//&
                                        UniformGridDescriptor%GetAttributeName(AttributeNumber=indx)//&
                                        '_'//trim(adjustl(str(no_sign=.true.,n=GridID))))
                call dataitem%close(xml_handler = XMLHandler)
                call attribute%close(xml_handler = XMLHandler)
            enddo
        endif                    
    end subroutine xdmf_dataset_per_process_handler_WriteAttributes

end module xdmf_dataset_per_process_handler
