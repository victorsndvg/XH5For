program steps_handler_test

use steps_handler
use IR_Precision, only: I4P, R4P, R8P

implicit none

    type(steps_handler_t) :: TimeSteps
    integer(I4P)          :: i, NumberOfSteps

    NumberOfSteps = 12_I4P
    call TimeSteps%Initialize(NumberOfSteps=NumberOfSteps)

    print*, 'Initial number of steps:', TimeSteps%GetNumberOfSteps()
    print*, 'Adding steps ... '
    do i=1, NumberOfSteps
        if(mod(i,2)==0) then
            call TimeSteps%Append(Value=real(i, R4P))
            print*, 'Number of step:', TimeSteps%GetCurrentStep(), 'with value:', TimeSteps%GetCurrentValue()
        else
            call TimeSteps%Append(Value=real(i, R8P))
            print*, 'Number of step:', TimeSteps%GetCurrentStep(), 'with value:', TimeSteps%GetStepValue(StepNumber=TimeSteps%GetCurrentStep())
        endif
    enddo

    print*, 'Total number of steps:', TimeSteps%GetNumberOfSteps()
    print*, 'Freeing ... '
    call TimeSteps%Free()

    print*, ''
    print*, 'Initial number of steps:', TimeSteps%GetNumberOfSteps()
    print*, 'Adding steps ... '
    do i=NumberOfSteps, 1, -1
        if(mod(i,2)==0) then
            call TimeSteps%Append(Value=real(i, R4P))
            print*, 'Number of step:', TimeSteps%GetCurrentStep(), 'with value:', TimeSteps%GetCurrentValue()
        else
            call TimeSteps%Append(Value=real(i, R8P))
            print*, 'Number of step:', TimeSteps%GetCurrentStep(), 'with value:', TimeSteps%GetStepValue(StepNumber=TimeSteps%GetCurrentStep())
        endif
    enddo

    print*, 'Total number of steps:', TimeSteps%GetNumberOfSteps()
    print*, 'Freeing ... '
    call TimeSteps%Free()


end program steps_handler_test
