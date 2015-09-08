module xdmf_dataitem
!--------------------------------------------------------------------- -----------------------------------------------------------
!< XdmfHdf5Fortran: XDMF parallel partitioned mesh I/O on top of HDF5
!< XDMF DataItem handling module
!--------------------------------------------------------------------- -----------------------------------------------------------
use IR_Precision, only: I4P, I8P, str
use FoX_wxml,     only: xml_NewElement, xml_EndElement, xml_AddAttribute, xmlf_t
use xdmf_utils,   only: Upper_Case, is_in_option_list, warning_message
use xdmf_element, only: xdmf_element_t

implicit none

!---------------------------------------------------------------------
! XDMFDataItem properties (* Default):
!---------------------------------------------------------------------
! Name            (no default)
! ItemType        *Uniform | Collection | tree | HyperSlab | coordinates | Function
! Dimensions      (no default) in KJI Order
! NumberType      *Float | Int | UInt | Char | UChar
! Precision       1 | 2 (Int or UInt only) |4 | 8
! Format          *XML | HDF | Binary
! Endian          *Native | Big | Little (applicable only to Binary format)
! Compression     *Raw|Zlib|BZip2 (applicable only to Binary format and depend on xdmf configuration)
! Seek            *0 (number of bytes to skip, applicable only to Binary format with Raw compression)
!---------------------------------------------------------------------

    type, extends(xdmf_element_t) :: xdmf_dataitem_t
    !-----------------------------------------------------------------
    !< XDMF DataItem type
    !----------------------------------------------------------------- 
    private
        character(len=:), allocatable :: Name
        character(len=:), allocatable :: ItemType
        integer(I8P),     allocatable :: Dimensions(:)
        character(len=:), allocatable :: NumberType
        integer(I4P)                  :: Precision
        character(len=:), allocatable :: Format
!        character(len=:), allocatable :: Endian
!        character(len=:), allocatable :: Compression
!        integer(I8P)                  :: Seek
    contains
    private
        procedure         :: dataitem_open_no_dimensions
        procedure         :: dataitem_open_I4P_dimension
        procedure         :: dataitem_open_I4P_dimensions
        procedure         :: dataitem_open_I8P_dimensions
        procedure         :: is_valid_ItemType      => dataitem_is_valid_ItemType
        procedure         :: is_valid_NumberType    => dataitem_is_valid_NumberType
        procedure         :: is_valid_Precision     => dataitem_is_valid_Precision
        procedure         :: is_valid_Format        => dataitem_is_valid_Format
        procedure         :: default_initialization => dataitem_default_initialization
        procedure         :: free                   => dataitem_free
        generic,   public :: open                   => dataitem_open_no_dimensions,  &
                                                       dataitem_open_I4P_dimension,  &
                                                       dataitem_open_I4P_dimensions, &
                                                       dataitem_open_I8P_dimensions

        procedure, public :: close                  => dataitem_close
    end type xdmf_dataitem_t

    !public :: xdmf_dataitem_t

contains

    function dataitem_is_valid_ItemType(this, ItemType) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem ItemType
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t), intent(IN) :: this                    !< XDMF DataItem type
        character(len=*),       intent(IN) :: ItemType                !< Dataitem ItemType 
        logical                            :: is_valid                !< Valid ItemType confirmation flag
        character(len=:), allocatable      :: allowed_ItemTypes       !< Dataitem ItemTypes list 
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_ItemTypes = 'Uniform&Collection&Tree&HyperSlab&Coordinates&Function'
        is_valid = is_in_option_list(option_list=allowed_ItemTypes, option=ItemType, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message(msg='Wrong ItemType: "'//ItemType//'" (Note: Case sensitive)')
    end function dataitem_is_valid_ItemType


    function dataitem_is_valid_NumberType(this, NumberType, warn) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem NumberType
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t), intent(IN) :: this                    !< XDMF DataItem type
        character(len=*),       intent(IN) :: NumberType              !< Dataitem NumberType 
        logical, optional,      intent(IN) :: warn                    !< Warn if is not valid
        logical                            :: is_valid                !< Valid NumberType confirmation flag
        character(len=:), allocatable      :: allowed_NumberTypes     !< Dataitem NumberTypes list 
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_NumberTypes = 'Float&Int&UInt&Char&UChar'
        is_valid = is_in_option_list(option_list=allowed_NumberTypes, option=NumberType, separator='&') 
        if(.not. is_valid .and. this%warn) call warning_message('Wrong NumberType: "'//NumberType//'" (Note: Case sensitive)')
    end function dataitem_is_valid_NumberType

    function dataitem_is_valid_Format(this, Format) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem NumberType
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t), intent(IN) :: this                    !< XDMF DataItem type
        character(len=*),       intent(IN) :: Format                  !< Dataitem Format
        logical                            :: is_valid                !< Valid NumberType confirmation flag
        character(len=:), allocatable      :: allowed_Formats         !< Dataitem Formats list 
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_Formats = 'XML&HDF'
        is_valid = is_in_option_list(option_list=allowed_Formats, option=Format, separator='&') 
        if(.not. is_valid .and. this%warn) then
            if(INDEX('BINARY', Upper_Case(adjustl(trim(Format)))) > 0) then
                call warning_message('Not supported Format: "'//Format//'"')
            else
                call warning_message('Wrong Format: "'//Format//'" (Note: Case sensitive)')
            endif
        endif
    end function dataitem_is_valid_Format


    function dataitem_is_valid_Precision(this, Precision) result(is_valid)
    !-----------------------------------------------------------------
    !< Return True if is a valid dataitem NumberType
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t), intent(IN) :: this                    !< XDMF DataItem type
        integer(I4P),           intent(IN) :: Precision               !< Dataitem Precision
        logical                            :: is_valid                !< Valid NumberType confirmation flag
        integer(I4P), allocatable          :: allowed_Precisions(:)   !< Dataitem NumberTypes list 
    !----------------------------------------------------------------- 
        ! & is an invalid character in XML
        allowed_Precisions = (/1_I4P,2_I4P,4_I4P,8_I4P/)
        is_valid = MINVAL(ABS(allowed_Precisions - Precision)) == 0_I4P
        if(.not. is_valid .and. this%warn) call warning_message('Wrong Precision: "'//trim(str(no_sign=.true., n=Precision))//'"')
    end function dataitem_is_valid_Precision


    subroutine dataitem_free(this)
    !-----------------------------------------------------------------
    !< Free XDMF dataitem type
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t), intent(INOUT) :: this                 !< XDMF DataItemp type
    !----------------------------------------------------------------- 
        if(allocated(this%Name))        deallocate(this%Name)
        if(allocated(this%ItemType))    deallocate(this%ItemType)
        if(allocated(this%Dimensions))  deallocate(this%Dimensions)
        if(allocated(this%NumberType))  deallocate(this%NumberType)
        if(allocated(this%Format))      deallocate(this%Format)
    end subroutine dataitem_free


    subroutine dataitem_default_initialization(this)
    !-----------------------------------------------------------------
    !< Initialize XDMF dataitem with  default attribute values
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t), intent(INOUT) :: this                 !< XDMF DataItemp type
    !----------------------------------------------------------------- 
        call this%free()
        this%ItemType    = 'Uniform'
        this%NumberType  = 'Float'
        this%Precision   = 4
        this%Format      = 'XML'
    end subroutine dataitem_default_initialization


    subroutine dataitem_open_no_dimensions(this, xml_handler, Name, ItemType, NumberType, Precision, Format)
    !-----------------------------------------------------------------
    !< Open a new dataitem XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t),     intent(INOUT) :: this             !< XDMF DataItemp type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        character(len=*), optional, intent(IN)    :: Name             !< XDMF DataItem Name attribute
        character(len=*), optional, intent(IN)    :: ItemType         !< XDMF DataItem ItemType attribute
        character(len=*), optional, intent(IN)    :: NumberType       !< XDMF DataItem NumberType attribute
        integer(i4P)    , optional, intent(IN)    :: Precision        !< XDMF DataItem Precision attribute
        character(len=*), optional, intent(IN)    :: Format           !< XDMF DataItem Format attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
        integer(I4P)                              :: i                !< Aux index variable
    !----------------------------------------------------------------- 
        call this%set_tag('DataItem')

        call xml_NewElement(xml_handler, 'DataItem')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)
        if(PRESENT(ItemType)) then; if(this%is_valid_ItemType(ItemType))       &
            call xml_AddAttribute(xml_handler, name="ItemType", value=ItemType)
        endif

        if(PRESENT(NumberType)) then; if(this%is_valid_NumberType(NumberType)) &
            call xml_AddAttribute(xml_handler, name="NumberType", value=NumberType)
        endif

        if(PRESENT(Precision)) then; if(this%is_valid_Precision(Precision))    &
            call xml_AddAttribute(xml_handler, name="Precision", value=Precision)
        endif

        if(PRESENT(Format)) then; if(this%is_valid_Format(Format))             &
            call xml_AddAttribute(xml_handler, name="Format", value=Format)
        endif

    end subroutine dataitem_open_no_dimensions


    subroutine dataitem_open_I4P_dimension(this, xml_handler, Dimensions, Name, ItemType, NumberType, Precision, Format)
    !-----------------------------------------------------------------
    !< Open a new dataitem XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t),     intent(INOUT) :: this             !< XDMF DataItemp type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        integer(I4P),               intent(IN)    :: Dimensions       !< XDMF DataItem I4P Dimensions attribute
        character(len=*), optional, intent(IN)    :: Name             !< XDMF DataItem Name attribute
        character(len=*), optional, intent(IN)    :: ItemType         !< XDMF DataItem ItemType attribute
        character(len=*), optional, intent(IN)    :: NumberType       !< XDMF DataItem NumberType attribute
        integer         , optional, intent(IN)    :: Precision        !< XDMF DataItem Precision attribute
        character(len=*), optional, intent(IN)    :: Format           !< XDMF DataItem Format attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
    !----------------------------------------------------------------- 
        call this%set_tag('DataItem')

        call xml_NewElement(xml_handler, 'DataItem')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)
        if(PRESENT(ItemType)) then; if(this%is_valid_ItemType(ItemType))       &
            call xml_AddAttribute(xml_handler, name="ItemType", value=ItemType)
        endif

        char_dims = trim(adjustl(str(no_sign=.true., n=Dimensions)))
        call xml_AddAttribute(xml_handler, name="Dimensions", value=trim(char_dims) )
        deallocate(char_dims)

        if(PRESENT(NumberType)) then; if(this%is_valid_NumberType(NumberType)) &
            call xml_AddAttribute(xml_handler, name="NumberType", value=NumberType)
        endif

        if(PRESENT(Precision)) then; if(this%is_valid_Precision(Precision))    &
            call xml_AddAttribute(xml_handler, name="Precision", value=Precision)
        endif

        if(PRESENT(Format)) then; if(this%is_valid_Format(Format))             &
            call xml_AddAttribute(xml_handler, name="Format", value=Format)
        endif

    end subroutine dataitem_open_I4P_dimension


    subroutine dataitem_open_I8P_dimension(this, xml_handler, Dimensions, Name, ItemType, NumberType, Precision, Format)
    !-----------------------------------------------------------------
    !< Open a new dataitem XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t),     intent(INOUT) :: this             !< XDMF DataItemp type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        integer(I8P),               intent(IN)    :: Dimensions       !< XDMF DataItem I8P Dimensions attribute
        character(len=*), optional, intent(IN)    :: Name             !< XDMF DataItem Name attribute
        character(len=*), optional, intent(IN)    :: ItemType         !< XDMF DataItem ItemType attribute
        character(len=*), optional, intent(IN)    :: NumberType       !< XDMF DataItem NumberType attribute
        integer         , optional, intent(IN)    :: Precision        !< XDMF DataItem Precision attribute
        character(len=*), optional, intent(IN)    :: Format           !< XDMF DataItem Format attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
    !----------------------------------------------------------------- 
        call this%set_tag('DataItem')

        call xml_NewElement(xml_handler, 'DataItem')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)
        if(PRESENT(ItemType)) then; if(this%is_valid_ItemType(ItemType))       &
            call xml_AddAttribute(xml_handler, name="ItemType", value=ItemType)
        endif

        char_dims = trim(adjustl(str(no_sign=.true., n=Dimensions)))
        call xml_AddAttribute(xml_handler, name="Dimensions", value=trim(char_dims) )
        deallocate(char_dims)

        if(PRESENT(NumberType)) then; if(this%is_valid_NumberType(NumberType)) &
            call xml_AddAttribute(xml_handler, name="NumberType", value=NumberType)
        endif

        if(PRESENT(Precision)) then; if(this%is_valid_Precision(Precision))    &
            call xml_AddAttribute(xml_handler, name="Precision", value=Precision)
        endif

        if(PRESENT(Format)) then; if(this%is_valid_Format(Format))             &
            call xml_AddAttribute(xml_handler, name="Format", value=Format)
        endif

    end subroutine dataitem_open_I8P_dimension

    subroutine dataitem_open_I4P_dimensions(this, xml_handler, Dimensions, Name, ItemType, NumberType, Precision, Format)
    !-----------------------------------------------------------------
    !< Open a new dataitem XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t),     intent(INOUT) :: this             !< XDMF DataItemp type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        integer(I4P),               intent(IN)    :: Dimensions(:)    !< XDMF DataItem I4P array Dimensions attribute
        character(len=*), optional, intent(IN)    :: Name             !< XDMF DataItem Name attribute
        character(len=*), optional, intent(IN)    :: ItemType         !< XDMF DataItem ItemType attribute
        character(len=*), optional, intent(IN)    :: NumberType       !< XDMF DataItem NumberType attribute
        integer         , optional, intent(IN)    :: Precision        !< XDMF DataItem Precision attribute
        character(len=*), optional, intent(IN)    :: Format           !< XDMF DataItem Format attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
        integer(I4P)                              :: i                !< Aux index variable
    !----------------------------------------------------------------- 
        call this%set_tag('DataItem')

        call xml_NewElement(xml_handler, 'DataItem')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)
        if(PRESENT(ItemType)) then; if(this%is_valid_ItemType(ItemType))       &
            call xml_AddAttribute(xml_handler, name="ItemType", value=ItemType)
        endif

        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Dimensions,dim=1)
        allocate(character(len=64*i) :: char_dims)
        write(char_dims, fmt=*)   (trim(adjustl(str(no_sign=.true., n=Dimensions(i))))//' ',i=1, size(Dimensions,dim=1) )
        call xml_AddAttribute(xml_handler, name="Dimensions", value=trim(char_dims) )
        deallocate(char_dims)

        if(PRESENT(NumberType)) then; if(this%is_valid_NumberType(NumberType)) &
            call xml_AddAttribute(xml_handler, name="NumberType", value=NumberType)
        endif

        if(PRESENT(Precision)) then; if(this%is_valid_Precision(Precision))    &
            call xml_AddAttribute(xml_handler, name="Precision", value=Precision)
        endif

        if(PRESENT(Format)) then; if(this%is_valid_Format(Format))             &
            call xml_AddAttribute(xml_handler, name="Format", value=Format)
        endif

    end subroutine dataitem_open_I4P_dimensions

    subroutine dataitem_open_I8P_dimensions(this, xml_handler, Dimensions, Name, ItemType, NumberType, Precision, Format)
    !-----------------------------------------------------------------
    !< Open a new dataitem XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t),     intent(INOUT) :: this             !< XDMF DataItemp type
        type(xmlf_t),               intent(INOUT) :: xml_handler      !< FoX XML File handler
        integer(I8P),               intent(IN)    :: Dimensions(:)    !< XDMF DataItem I8P array Dimensions attribute
        character(len=*), optional, intent(IN)    :: Name             !< XDMF DataItem Name attribute
        character(len=*), optional, intent(IN)    :: ItemType         !< XDMF DataItem ItemType attribute
        character(len=*), optional, intent(IN)    :: NumberType       !< XDMF DataItem NumberType attribute
        integer         , optional, intent(IN)    :: Precision        !< XDMF DataItem Precision attribute
        character(len=*), optional, intent(IN)    :: Format           !< XDMF DataItem Format attribute
        character(len=:), allocatable             :: char_dims        !< Aux String for int to string conversion
        integer(I4P)                              :: i                !< Aux index variable
    !----------------------------------------------------------------- 
        call this%set_tag('DataItem')

        call xml_NewElement(xml_handler, 'DataItem')
        if(PRESENT(Name))                                                      &
            call xml_AddAttribute(xml_handler, name="Name", value=Name)
        if(PRESENT(ItemType)) then; if(this%is_valid_ItemType(ItemType))       &
            call xml_AddAttribute(xml_handler, name="ItemType", value=ItemType)
        endif

        if(allocated(char_dims)) deallocate(char_dims)
        i = size(Dimensions,dim=1)
        allocate(character(len=64*i) :: char_dims)
        write(char_dims, fmt=*)   (trim(adjustl(str(no_sign=.true., n=Dimensions(i))))//' ',i=1, size(Dimensions,dim=1) )
        call xml_AddAttribute(xml_handler, name="Dimensions", value=trim(char_dims) )
        deallocate(char_dims)

        if(PRESENT(NumberType)) then; if(this%is_valid_NumberType(NumberType)) &
            call xml_AddAttribute(xml_handler, name="NumberType", value=NumberType)
        endif

        if(PRESENT(Precision)) then; if(this%is_valid_Precision(Precision))    &
            call xml_AddAttribute(xml_handler, name="Precision", value=Precision)
        endif

        if(PRESENT(Format)) then; if(this%is_valid_Format(Format))             &
            call xml_AddAttribute(xml_handler, name="Format", value=Format)
        endif

    end subroutine dataitem_open_I8P_dimensions


    subroutine dataitem_close(this, xml_handler)
    !-----------------------------------------------------------------
    !< Close a new dataitem XDMF element
    !----------------------------------------------------------------- 
        class(xdmf_dataitem_t), intent(IN)    :: this                 !< XDMF DataItemp type
        type(xmlf_t),           intent(INOUT) :: xml_handler          !< FoX XML File handler
    !-----------------------------------------------------------------
        call xml_EndElement(xml_handler, this%get_tag())
    end subroutine dataitem_close


end module xdmf_dataitem
