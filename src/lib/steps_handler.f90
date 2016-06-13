module steps_handler

use IR_Precision, only: I4P, R4P, R8P, str

implicit none
private

    integer(I4P), parameter :: DEFAULT_STEPS_ARRAY_SIZE = 10

    type steps_handler_t
    private
        real(R8P),    allocatable :: Values(:)
        integer(I4P)              :: NumberOfSteps = 0
        integer(I4P)              :: StepsCounter  = 0
    contains
        procedure, non_overridable, public :: Initialize          => steps_handler_Initialize
        procedure, non_overridable         :: Append_R4P          => steps_handler_Append_R4P
        procedure, non_overridable         :: Append_R8P          => steps_handler_Append_R8P
        generic,                    public :: Append              => Append_R4P, Append_R8P
        procedure, non_overridable, public :: GetNumberOfSteps    => steps_handler_GetNumberOfSteps
        procedure, non_overridable, public :: GetCurrentStep      => steps_handler_GetCurrentStep
        procedure, non_overridable, public :: GetCurrentValue     => steps_handler_GetCurrentValue
        procedure, non_overridable, public :: GetStepValue        => steps_handler_GetStepValue
        procedure, non_overridable, public :: Free                => steps_handler_Free
    end type

public :: steps_handler_t

contains


    subroutine steps_handler_Initialize(this, NumberOfSteps)
    !-----------------------------------------------------------------
    !< Initilized the steps handler type
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        integer(I4P), optional, intent(IN)    :: NumberOfSteps        !< Number of expected steps
    !-----------------------------------------------------------------
        call this%Free()
        if(present(NumberOfSteps)) then
            allocate(this%Values(NumberOfSteps))
            this%NumberOfSteps = NumberOfSteps
        endif
    end subroutine steps_handler_Initialize


    subroutine steps_handler_Append_R4P(this, Value)
    !-----------------------------------------------------------------
    !< Append a new R4P step value
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        real(R4P),              intent(IN)    :: Value                !< Step Value
    !-----------------------------------------------------------------
        call this%Append_R8P(Value = real(Value, kind=R8P))
    end subroutine steps_handler_Append_R4P


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
        if(allocated(this%Values) .and. this%StepsCounter>0) then
            CurrentValue = this%Values(this%StepsCounter)
        endif
    end function steps_handler_GetCurrentValue


    function steps_handler_GetStepValue(this, StepNumber) result(Value)
    !-----------------------------------------------------------------
    !< Return the value given the step number
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        integer(I4P),           intent(IN)    :: StepNumber           !< Number of the step
        real(R8P)                             :: Value                !< Current step number
    !----------------------------------------------------------------- 
        Value = 0.0_R8P
        if(StepNumber>0 .and. StepNumber<=this%NumberOfSteps) then
            Value = this%Values(StepNumber)
        endif
    end function steps_handler_GetStepValue


    subroutine steps_handler_Append_R8P(this, Value)
    !-----------------------------------------------------------------
    !< Append a new R8P step value
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
        real(R8P),              intent(IN)    :: Value                !< Step Value
        real(R8P), allocatable                :: TmpValues(:)         !< Temporal Steps array
    !-----------------------------------------------------------------
        if(.not. allocated(this%Values)) then
            allocate(this%Values(DEFAULT_STEPS_ARRAY_SIZE))
        elseif(this%StepsCounter+1>size(this%Values)) then
            call move_alloc(from=this%Values, to=TmpValues)
            allocate(this%Values(Size(this%Values)*2))
            this%Values(:this%NumberOfSteps) = TmpValues(:this%NumberOfSteps)
        endif
        this%StepsCounter = this%StepsCounter+1
        this%NumberOfSteps = max(this%NumberOfSteps, this%StepsCounter)
        this%Values(this%StepsCounter) = Value
    end subroutine steps_handler_Append_R8P


    subroutine steps_handler_Free(this)
    !-----------------------------------------------------------------
    !< Free the steps handler type
    !----------------------------------------------------------------- 
        class(steps_handler_t), intent(INOUT) :: this                 !< Steps Handler
    !-----------------------------------------------------------------
        this%NumberOfSteps = 0
        this%StepsCounter  = 0
        if(allocated(this%Values)) deallocate(this%Values)
    end subroutine steps_handler_Free

end module steps_handler
