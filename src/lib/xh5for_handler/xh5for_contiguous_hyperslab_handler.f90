module xh5for_contiguous_hyperslab_handler

use xh5for_handler
use xh5for_parameters
use IR_Precision, only : I4P, I8P, R4P, R8P
use xdmf_contiguous_hyperslab_handler
use hdf5_contiguous_hyperslab_handler

implicit none

private

    type, abstract, extends(xh5for_handler_t) :: xh5for_contiguous_hyperslab_handler_t
    contains
        procedure         :: WriteAttribute_I4P => xh5for_contiguous_hyperslab_handler_WriteAttribute_I4P
        procedure         :: WriteAttribute_I8P => xh5for_contiguous_hyperslab_handler_WriteAttribute_I8P
        procedure         :: WriteAttribute_R4P => xh5for_contiguous_hyperslab_handler_WriteAttribute_R4P
        procedure         :: WriteAttribute_R8P => xh5for_contiguous_hyperslab_handler_WriteAttribute_R8P
        procedure         :: ReadAttribute_I4P  => xh5for_contiguous_hyperslab_handler_ReadAttribute_I4P
        procedure         :: ReadAttribute_I8P  => xh5for_contiguous_hyperslab_handler_ReadAttribute_I8P
        procedure         :: ReadAttribute_R4P  => xh5for_contiguous_hyperslab_handler_ReadAttribute_R4P
        procedure         :: ReadAttribute_R8P  => xh5for_contiguous_hyperslab_handler_ReadAttribute_R8P
        procedure, public :: Free               => xh5for_contiguous_hyperslab_handler_Free
        procedure, public :: Open               => xh5for_contiguous_hyperslab_handler_Open
        procedure, public :: Parse              => xh5for_contiguous_hyperslab_handler_Parse
        procedure, public :: Close              => xh5for_contiguous_hyperslab_handler_Close
    end type xh5for_contiguous_hyperslab_handler_t

public :: xh5for_contiguous_hyperslab_handler_t

contains

    subroutine xh5for_contiguous_hyperslab_handler_Free(this)
    !-----------------------------------------------------------------
    !< XH5FOR Free procedure
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contiguous hyperslab handler
    !----------------------------------------------------------------- 
        if(allocated(this%LightData)) then
            call this%LightData%Free()
            deallocate(this%LightData)
        endif
        if(allocated(this%HeavyData)) then
            call this%HeavyData%Free()
            deallocate(this%HeavyData)
        endif
    end subroutine xh5for_contiguous_hyperslab_handler_Free

    subroutine xh5for_contiguous_hyperslab_handler_Open(this, action, fileprefix)
    !-----------------------------------------------------------------
    !< Open the lightdata and the heavydata files
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this         !< XH5For contiguous hyperslab handler
        integer(I4P),                                 intent(IN)    :: action       !< XH5For Open action (Read or Write)
        character(len=*),                             intent(IN)    :: fileprefix   !< Filename prefix
    !-----------------------------------------------------------------
        call this%HeavyData%OpenFile(action=action, fileprefix=fileprefix)
        call this%LightData%OpenFile(action=action, fileprefix=fileprefix)
    end subroutine xh5for_contiguous_hyperslab_handler_Open


    subroutine xh5for_contiguous_hyperslab_handler_Parse(this)
    !-----------------------------------------------------------------
    !< Parse the lightdata
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this !< XH5For contigous hyperslab handler
    !-----------------------------------------------------------------
        call this%LightData%ParseFile()
    end subroutine xh5for_contiguous_hyperslab_handler_Parse



    subroutine xh5for_contiguous_hyperslab_handler_Close(this, action)
    !-----------------------------------------------------------------
    !< Close the lightdata and the heavydata files
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this   !< XH5For contigous hyperslab handler
        integer(I4P),                                 intent(IN)    :: action !< XH5For Close action (Read or Write)
    !-----------------------------------------------------------------
        call this%HeavyData%CloseFile()
        if(action == XDMF_ACTION_WRITE) then
            !< XDMF deferred writing when hdf5 closes    
            call this%LightData%Serialize()
            call this%LightData%CloseFile()
        endif
    end subroutine xh5for_contiguous_hyperslab_handler_Close


    subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_I4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Write an I4P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        integer(I4P),                                 intent(IN)    :: Values(:)  !< I4P Grid attribute
    !----------------------------------------------------------------- 
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_I4P


    subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_I8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Write an I8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        integer(I8P),                                 intent(IN)    :: Values(:)  !< I8P Grid attribute
    !----------------------------------------------------------------- 
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_I8P


    subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_R4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Write an R4P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        real(R4P),                                    intent(IN)    :: Values(:)  !< R4P Grid attribute
    !----------------------------------------------------------------- 
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_R4P


    subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_R8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Write an R8P geometry for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        real(R8P),                                    intent(IN)    :: Values(:)  !< R8P Grid attribute
    !----------------------------------------------------------------- 
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_WriteAttribute_R8P


    subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_I4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Read an I4P attribute for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        integer(I4P), allocatable,                    intent(OUT)   :: Values(:)  !< I4P Grid attribute
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_I4P


    subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_I8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Read an I8P attribute for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        integer(I8P), allocatable,                    intent(OUT)   :: Values(:)  !< I8P Grid attribute
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_I8P


    subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_R4P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Read an R4P attribute for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        real(R4P), allocatable,                       intent(OUT)   :: Values(:)  !< R4P Grid attribute
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_R4P


    subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_R8P(this, Name, Type, Center, Values)
    !-----------------------------------------------------------------
    !< Read an R8P attribute for the contiguous hyperslab strategy
    !----------------------------------------------------------------- 
        class(xh5for_contiguous_hyperslab_handler_t), intent(INOUT) :: this       !< XH5For contiguous hyperslab handler
        character(len=*),                             intent(IN)    :: Name       !< Attribute name
        integer(I4P),                                 intent(IN)    :: Type       !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),                                 intent(IN)    :: Center     !< Attribute center (Node, Cell, etc.)
        real(R8P), allocatable,                       intent(OUT)   :: Values(:)  !< R4P Grid attribute
    !----------------------------------------------------------------- 
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
    end subroutine xh5for_contiguous_hyperslab_handler_ReadAttribute_R8P

end module xh5for_contiguous_hyperslab_handler
