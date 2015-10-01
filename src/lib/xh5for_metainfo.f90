module xh5for_metadata

use IR_Precision, only: I4P
use xh5for_parameters

implicit none
private

    type :: xh5for_metadata_t
    private
        character(len=:), allocatable :: XPath
        integer(I4P)                  :: Type       = XDMF_NO_VALUE
        integer(I4P)                  :: Center     = XDMF_NO_VALUE
        character(len=:), allocatable :: DataType
        integer(I4P)                  :: Precision  = XDMF_NO_VALUE
        integer(I4P)                  :: Dimension  = XDMF_NO_VALUE
    contains
        procedure, public :: SetXPath     => xh5for_metadata_SetXPath
        procedure, public :: SetType      => xh5for_metadata_SetType
        procedure, public :: SetCenter    => xh5for_metadata_SetCenter
        procedure, public :: SetDataType  => xh5for_metadata_SetDataType
        procedure, public :: SetPrecision => xh5for_metadata_SetPrecision
        procedure, public :: SetDimension => xh5for_metadata_SetDimension
        procedure, public :: GetXPath     => xh5for_metadata_GetXPath
        procedure, public :: GetType      => xh5for_metadata_GetType
        procedure, public :: GetCenter    => xh5for_metadata_GetCenter
        procedure, public :: GetDataType  => xh5for_metadata_GetDataType
        procedure, public :: GetPrecision => xh5for_metadata_GetPrecision
        procedure, public :: GetDimension => xh5for_metadata_GetDimension
        procedure, public :: Free         => xh5for_metadata_Free
    end type xh5for_metadata_t

public :: xh5for_metadata_t

contains

    subroutine xh5for_metadata_SetXPath(this, XPath)
    !-----------------------------------------------------------------
    !< Set the XPath
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        character(len=*),         intent(IN)    :: XPath              !< XPath
    !----------------------------------------------------------------- 
        this%XPath = XPath
    end subroutine xh5for_metadata_setXPath


    subroutine xh5for_metadata_SetType(this, Type)
    !-----------------------------------------------------------------
    !< Set the Type
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        integer(I4P),             intent(IN)    :: Type               !< Type
    !----------------------------------------------------------------- 
        this%Type = Type
    end subroutine xh5for_metadata_setType


    subroutine xh5for_metadata_SetCenter(this, Center)
    !-----------------------------------------------------------------
    !< Set the Center
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        integer(I4P),             intent(IN)    :: Center             !< Center
    !----------------------------------------------------------------- 
        this%Center = Center
    end subroutine xh5for_metadata_setCenter


    subroutine xh5for_metadata_SetDataType(this, DataType)
    !-----------------------------------------------------------------
    !< Set the DataType
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        character(len=*),         intent(IN)    :: DataType           !< DataType
    !----------------------------------------------------------------- 
        this%DataType = DataType
    end subroutine xh5for_metadata_setDataType


    subroutine xh5for_metadata_SetPrecision(this, Precision)
    !-----------------------------------------------------------------
    !< Set the Precision
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        integer(I4P),             intent(IN)    :: Precision          !< Precision
    !----------------------------------------------------------------- 
        this%Precision = Precision
    end subroutine xh5for_metadata_setPrecision


    subroutine xh5for_metadata_SetDimension(this, Dimension)
    !-----------------------------------------------------------------
    !< Set the Dimension
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
        integer(I4P),             intent(IN)    :: Dimension          !< Dimension
    !----------------------------------------------------------------- 
        this%Dimension = Dimension
    end subroutine xh5for_metadata_setDimension


    function xh5for_metadata_GetXPath(this)
    !-----------------------------------------------------------------
    !< Return the XPath
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                   !< XH5For metadata type
        character(len=:), allocatable :: xh5for_metadata_GetXPath      !< Returned XPath
    !----------------------------------------------------------------- 
        xh5for_metadata_GetXPath = this%XPath
    end function xh5for_metadata_GetXPath


    function xh5for_metadata_GetType(this)
    !-----------------------------------------------------------------
    !< Return the Type
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                  !< XH5For metadata type
        integer(I4P) :: xh5for_metadata_GetType                       !< Returned Type
    !----------------------------------------------------------------- 
        xh5for_metadata_GetType = this%Type
    end function xh5for_metadata_GetType


    function xh5for_metadata_GetCenter(this)
    !-----------------------------------------------------------------
    !< Return the Center
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                  !< XH5For metadata type
        integer(I4P) :: xh5for_metadata_GetCenter                     !< Returned Center
    !----------------------------------------------------------------- 
        xh5for_metadata_GetCenter = this%Center
    end function xh5for_metadata_GetCenter


    function xh5for_metadata_GetDataType(this)
    !-----------------------------------------------------------------
    !< Return the DataType
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                   !< XH5For metadata type
        character(len=:), allocatable :: xh5for_metadata_GetDataType   !< Returned DataType
    !----------------------------------------------------------------- 
        xh5for_metadata_GetDataType = this%DataType
    end function xh5for_metadata_GetDataType


    function xh5for_metadata_GetPrecision(this)
    !-----------------------------------------------------------------
    !< Return the Precision
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                  !< XH5For metadata type
        integer(I4P) :: xh5for_metadata_GetPrecision                  !< Returned Precision
    !----------------------------------------------------------------- 
        xh5for_metadata_GetPrecision = this%Precision
    end function xh5for_metadata_GetPrecision


    function xh5for_metadata_GetDimension(this)
    !-----------------------------------------------------------------
    !< Return the Dimension
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(IN) :: this                  !< XH5For metadata type
        integer(I4P) :: xh5for_metadata_GetDimension                  !< Returned Dimension
    !----------------------------------------------------------------- 
        xh5for_metadata_GetDimension = this%Dimension
    end function xh5for_metadata_GetDimension


    subroutine xh5for_metadata_Free(this)
    !-----------------------------------------------------------------
    !< Free XH5For Metadata
    !----------------------------------------------------------------- 
        class(xh5for_metadata_t), intent(INOUT) :: this               !< XH5For metadata type
    !----------------------------------------------------------------- 
        if(allocated(this%XPath)) deallocate(this%XPath)
        if(allocated(this%DataType)) deallocate(this%DataType)
        this%Type       = XDMF_NO_VALUE
        this%Center     = XDMF_NO_VALUE
        this%Precision  = XDMF_NO_VALUE
        this%Dimension  = XDMF_NO_VALUE
    end subroutine



end module xh5for_metadata
