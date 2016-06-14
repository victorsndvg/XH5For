module steps_handler

use IR_Precision, only: I4P, R4P, R8P, str
USE mpi_environment

implicit none
private

    integer(I4P), parameter :: DEFAULT_STEPS_ARRAY_SIZE = 10
    type string_t
    private
        character(len=:), allocatable :: data
    contains
        procedure, non_overridable, public :: Set  => string_Set
        procedure, non_overridable, public :: Get  => string_Get
        procedure, non_overridable, public :: Free => string_Free
    end type

    type steps_handler_t
    private
        real(R8P),      allocatable :: Values(:)
        type(string_t), allocatable :: Filenames(:)
        type(mpi_env_t), pointer    :: MPIEnvironment => NULL()
        integer(I4P)                :: NumberOfSteps = 0
        integer(I4P)                :: StepsCounter  = 0
    contains
        procedure, non_overridable, public :: Initialize             => steps_handler_Initialize
        procedure, non_overridable         :: ResizeArrays           => steps_handler_ResizeArrays
        procedure, non_overridable         :: Append_R4P             => steps_handler_Append_R4P
        procedure, non_overridable         :: Append_R8P             => steps_handler_Append_R8P
        procedure, non_overridable         :: Append_String          => steps_handler_Append_String
        generic,                    public :: Append                 => Append_R4P, Append_R8P, Append_String
        procedure, non_overridable, public :: BroadCastNumberOfSteps => steps_handler_BroadCastNumberOfSteps
        procedure, non_overridable, public :: Begin                  => steps_handler_Begin
        procedure, non_overridable, public :: Next                   => steps_handler_Next
        procedure, non_overridable, public :: End                    => steps_handler_End
        procedure, non_overridable, public :: GetNumberOfSteps       => steps_handler_GetNumberOfSteps
        procedure, non_overridable, public :: GetCurrentStep         => steps_handler_GetCurrentStep
        procedure, non_overridable, public :: GetCurrentValue        => steps_handler_GetCurrentValue
        procedure, non_overridable, public :: GetCurrentFilename     => steps_handler_GetCurrentFilename
        procedure, non_overridable, public :: SetCurrentFilename     => steps_handler_SetCurrentFilename
        procedure, non_overridable, public :: GetStepValue           => steps_handler_GetStepValue
        procedure, non_overridable, public :: Free                   => steps_handler_Free
    end type

public :: steps_handler_t

contains

    subroutine string_Set(this, Data)
    !-----------------------------------------------------------------
    !< Set the string data value
    !----------------------------------------------------------------- 
        class(string_t),  intent(INOUT) :: this                       !< String derive type
        character(len=*), intent(IN)    :: Data                       !< String to store
    !-----------------------------------------------------------------
        call this%Free()
        this%Data = Data
    end subroutine string_Set


    function string_Get(this) result(Data)
    !-----------------------------------------------------------------
    !< Return the string data value
    !----------------------------------------------------------------- 
        class(string_t),  intent(INOUT) :: this                       !< String derive type
        character(len=:), allocatable   :: Data                       !< String to return
    !-----------------------------------------------------------------
        Data = this%Data
    end function string_Get


    subroutine string_Free(this)
    !-----------------------------------------------------------------
    !< Free string derived type
    !----------------------------------------------------------------- 
        class(string_t),  intent(INOUT) :: this                       !< String derive type
    !-----------------------------------------------------------------
        if(allocated(this%Data)) deallocate(this%Data)
    end subroutine string_Free


    subroutine steps_handler_Initialize(this, MPIEnvironment, NumberOfSteps)
    !-----------------------------------------------------------------
    !< Initilized the steps handler type
    !----------------------------------------------------------------- 
        class(steps_handler_t),  intent(INOUT) :: this                !< Steps Handler
        type(mpi_env_t), target, intent(IN)    :: MPIEnvironment      !< MPI Environment
        integer(I4P), optional,  intent(IN)    :: NumberOfSteps       !< Number of expected steps
    !-----------------------------------------------------------------
        call this%Free()
        this%MPIEnvironment => MPIEnvironment
        if(present(NumberOfSteps)) then
            if(this%MPIEnvironment%is_root()) then
                allocate(this%Values(NumberOfSteps))
                allocate(this%Filenames(NumberOfSteps))
            endif
            this%NumberOfSteps = NumberOfSteps
        endif
    end subroutine steps_handler_Initialize


    function steps_handler_GetNumberOfSteps(this) result(NumberOfSteps)
    !-----------------------------------------------------------------
    !< Return the number of steps
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        integer(I4P)                          :: NumberOfSteps        !< Number of steps
    !----------------------------------------------------------------- 
        NumberOfSteps = this%NumberOfSteps
    end function steps_handler_GetNumberOfSteps


    function steps_handler_GetCurrentStep(this) result(CurrentStep)
    !-----------------------------------------------------------------
    !< Return the current step number
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        integer(I4P)                          :: CurrentStep          !< Current step number
    !----------------------------------------------------------------- 
        CurrentStep = this%StepsCounter
    end function steps_handler_GetCurrentStep


    function steps_handler_GetCurrentValue(this) result(CurrentValue)
    !-----------------------------------------------------------------
    !< Return the current step value
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        real(R8P)                             :: CurrentValue         !< Current step value
    !----------------------------------------------------------------- 
        CurrentValue = 0.0_R8P
        if(this%MPIEnvironment%is_root()) then
            if(allocated(this%Values) .and. this%StepsCounter>0) then
                CurrentValue = this%Values(this%StepsCounter)
            endif
        endif
    end function steps_handler_GetCurrentValue


    function steps_handler_GetCurrentFilename(this) result(CurrentFilename)
    !-----------------------------------------------------------------
    !< Return the current step filename
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        character(len=:), allocatable         :: CurrentFilename      !< Current step filename
    !----------------------------------------------------------------- 
        CurrentFilename=''
        if(this%MPIEnvironment%is_root()) then
            if(this%StepsCounter>0) then
                CurrentFilename = this%Filenames(this%StepsCounter)%Get()
            endif
        endif
    end function steps_handler_GetCurrentFilename


    subroutine steps_handler_SetCurrentFilename(this, Filename)
    !-----------------------------------------------------------------
    !< Return the current step filename
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        character(len=*),       intent(IN)    :: Filename             !< Current step filename
    !----------------------------------------------------------------- 
        if(this%MPIEnvironment%is_root()) then
            if(this%StepsCounter>0) then
                call this%Filenames(this%StepsCounter)%Set(Filename)
            endif
        endif
    end subroutine steps_handler_SetCurrentFilename


    function steps_handler_GetStepValue(this, StepNumber) result(Value)
    !-----------------------------------------------------------------
    !< Return the value given the step number
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        integer(I4P),           intent(IN)    :: StepNumber           !< Number of the step
        real(R8P)                             :: Value                !< Current step number
    !----------------------------------------------------------------- 
        Value = 0.0_R8P
        if(this%MPIEnvironment%is_root()) then
            if(StepNumber>0 .and. StepNumber<=this%NumberOfSteps) then
                Value = this%Values(StepNumber)
            endif
        endif
    end function steps_handler_GetStepValue


    subroutine steps_handler_ResizeArrays(this, GrowthFactor)
    !-----------------------------------------------------------------
    !< Prepare arrays to append new elements
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        real(R8P), optional,    intent(IN)    :: GrowthFactor         !< Growth factor
        real(R8P)                             :: TmpGrowthFactor      !< Temporal Growth factor
        real(R8P),      allocatable           :: TmpValues(:)         !< Temporal Steps array
        type(string_t), allocatable           :: TmpFilenames(:)      !< Temporal Filenames array
        integer(I4P)                          :: CurrentSize          !< Current arrays size
        integer(I4P)                          :: NewSize              !< New arrays size
    !----------------------------------------------------------------- 
        if(this%MPIEnvironment%is_root()) then
            TmpGrowthFactor = 2.0
            CurrentSize = size(this%Values)
            if(.not. allocated(this%Values)) then
                allocate(this%Values(DEFAULT_STEPS_ARRAY_SIZE))
                allocate(this%Filenames(DEFAULT_STEPS_ARRAY_SIZE))
            elseif(this%StepsCounter+1>CurrentSize) then
                if(present(GrowthFactor)) then
                    if(int(GrowthFactor*CurrentSize)>CurrentSize) TmpGrowthFactor = GrowthFactor
                endif
                NewSize = int(CurrentSize*TmpGrowthFactor)
                call move_alloc(from=this%Values, to=TmpValues)
                call move_alloc(from=this%Filenames, to=TmpFilenames)
                allocate(this%Values(NewSize))
                allocate(this%Filenames(NewSize))
                this%Values(:this%NumberOfSteps) = TmpValues(:this%NumberOfSteps)
                this%Filenames(:this%NumberOfSteps) = TmpFilenames(:this%NumberOfSteps)
            endif
        endif
    end subroutine steps_handler_ResizeArrays


    subroutine steps_handler_Append_R8P(this, Value)
    !-----------------------------------------------------------------
    !< Append a new R8P step value
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        real(R8P),              intent(IN)    :: Value                !< Step Value
    !-----------------------------------------------------------------
        call this%ResizeArrays()
        this%StepsCounter = this%StepsCounter+1
        this%NumberOfSteps = max(this%NumberOfSteps, this%StepsCounter)
        if(this%MPIEnvironment%is_root()) this%Values(this%StepsCounter) = Value
    end subroutine steps_handler_Append_R8P


    subroutine steps_handler_Append_R4P(this, Value)
    !-----------------------------------------------------------------
    !< Append a new R4P step value
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        real(R4P),              intent(IN)    :: Value                !< Step Value
    !-----------------------------------------------------------------
        call this%ResizeArrays()
        this%StepsCounter = this%StepsCounter+1
        this%NumberOfSteps = max(this%NumberOfSteps, this%StepsCounter)
        if(this%MPIEnvironment%is_root()) this%Values(this%StepsCounter) = real(Value, kind=R8P)
    end subroutine steps_handler_Append_R4P


    subroutine steps_handler_Append_String(this, Filename)
    !-----------------------------------------------------------------
    !< Append a new R8P step value
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        character(len=*),       intent(IN)    :: Filename             !< Step Filename
    !-----------------------------------------------------------------
        call this%ResizeArrays()
        this%StepsCounter = this%StepsCounter+1
        this%NumberOfSteps = max(this%NumberOfSteps, this%StepsCounter)
        if(this%MPIEnvironment%is_root()) call this%Filenames(this%StepsCounter)%Set(Filename)
    end subroutine steps_handler_Append_String


    subroutine steps_handler_BroadcastNumberOfSteps(this)
    !-----------------------------------------------------------------
    !< Broadcast number of steps after XDMF parsing
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
    !-----------------------------------------------------------------
        call this%MPIEnvironment%mpi_broadcast(this%NumberOfSteps)
    end subroutine steps_handler_BroadcastNumberOfSteps


    subroutine steps_handler_Begin(this)
    !-----------------------------------------------------------------
    !< CurrentStep returns to the first step
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
    !-----------------------------------------------------------------
        this%StepsCounter = 0
    end subroutine steps_handler_Begin


    subroutine steps_handler_Next(this)
    !-----------------------------------------------------------------
    !< Return Current step to the first step
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
    !-----------------------------------------------------------------
        if(this%StepsCounter<this%NumberOfSteps) then
            this%StepsCounter = this%StepsCounter+1
        endif
    end subroutine steps_handler_Next


    subroutine steps_handler_End(this)
    !-----------------------------------------------------------------
    !< Return Current step to the first step
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
    !-----------------------------------------------------------------
        this%StepsCounter = this%NumberOfSteps
    end subroutine steps_handler_End


    subroutine steps_handler_Free(this)
    !-----------------------------------------------------------------
    !< Free the steps handler type
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        integer                               :: i
    !-----------------------------------------------------------------
        this%NumberOfSteps = 0
        this%StepsCounter  = 0
        if(allocated(this%Values)) deallocate(this%Values)
        if(allocated(this%Filenames)) then
            do i=1, size(this%Filenames)
                call this%Filenames(i)%Free()
            enddo
            deallocate(this%Filenames)
        endif
    end subroutine steps_handler_Free

end module steps_handler
