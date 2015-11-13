module xh5for_handler_factory

use IR_Precision, only: I4P
use xh5for_parameters
use xh5for_handler
use xh5for_unstructured_contiguous_hyperslab_handler

implicit none
private

    type xh5for_handler_factory_t
    contains
        procedure, public :: GetHandler => xh5for_handler_factory_GetHandler
    end type xh5for_handler_factory_t

public :: xh5for_handler_factory_t

contains

    subroutine xh5for_handler_factory_GetHandler(this, Strategy, Handler)
        class(xh5for_handler_factory_t),      intent(IN)  :: this
        integer(I4P),                         intent(IN)  :: Strategy
        class(xh5for_handler_t), allocatable, intent(OUT) :: Handler


        select case(Strategy)
            case (XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB)
                allocate(xh5for_unstructured_contiguous_hyperslab_handler_t::Handler)

            case default
                allocate(xh5for_unstructured_contiguous_hyperslab_handler_t::Handler)
        end select 
    end subroutine


end module xh5for_handler_factory
