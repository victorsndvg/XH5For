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
module xh5for_handler

use mpi_environment
use xh5for_utils
use xh5for_parameters
use uniform_grid_descriptor
use spatial_grid_descriptor
use steps_handler
use xdmf_handler
use hdf5_handler
use xh5for_abstract_factory
use xh5for_factory
use PENF, only: I4P, I8P, R4P, R8P, str


implicit none

#include "assert.i90"

private

    integer(I4P), private, parameter :: XH5FOR_STATE_START     = 0
    integer(I4P), private, parameter :: XH5FOR_STATE_OPEN      = 1
    integer(I4P), private, parameter :: XH5FOR_STATE_INIT      = 2
    integer(I4P), private, parameter :: XH5FOR_STATE_GRID_SET  = 3
    integer(I4P), private, parameter :: XH5FOR_STATE_GRID_IO   = 4
    integer(I4P), private, parameter :: XH5FOR_STATE_CLOSE     = 5

    !-----------------------------------------------------------------
    ! XH5FOR State Transition Diagram
    !-----------------------------------------------------------------
    ! - This diagram controls the basic life cycle of the XH5For library
    !----------------------------------------------------------------- 
    !       INIT STATE      |     ACTION      |      FINAL STATE
    !----------------------------------------------------------------- 
    ! START                 | Free            | START
    ! START                 | Open            | OPEN
    ! START                 | Close           | CLOSE
    !----------------------------------------------------------------- 
    ! OPEN                  | Free            | START
    ! OPEN                  | Clean           | OPEN
    ! OPEN                  | Open            | OPEN
    ! OPEN                  | Initialize      | INIT
    ! OPEN                  | SetGrid         | GRID_SET
    ! OPEN                  | ParseGrid       | GRID_SET
    ! OPEN                  | Close           | CLOSE
    !----------------------------------------------------------------- 
    ! INIT                  | Free            | START
    ! INIT                  | Clean           | OPEN
    ! INIT                  | Open            | OPEN
    ! INIT                  | SetGrid         | GRID_SET
    ! INIT                  | ParseGrid       | GRID_SET
    !----------------------------------------------------------------- 
    ! GRID_SET              | Free            | START
    ! GRID_SET              | Clean           | OPEN
    ! GRID_SET              | Open            | OPEN
    ! GRID_SET              | SetGrid         | GRID_SET
    ! GRID_SET              | ParseGrid       | GRID_SET
    ! GRID_SET              | Write*          | GRID_IO
    ! GRID_SET              | Read*           | GRID_IO
    ! GRID_SET              | Close           | CLOSE
    !----------------------------------------------------------------- 
    ! GRID_IO               | Free            | START
    ! GRID_IO               | Clean           | OPEN
    ! GRID_IO               | Open            | OPEN
    ! GRID_IO               | SetGrid         | GRID_SET
    ! GRID_IO               | ParseGrid       | GRID_SET
    ! GRID_IO               | Write*          | GRID_IO
    ! GRID_IO               | Read*           | GRID_IO
    ! GRID_IO               | Close           | CLOSE
    !----------------------------------------------------------------- 
    ! CLOSE                 | Free            | START
    ! CLOSE                 | Clean           | OPEN
    ! CLOSE                 | Open            | OPEN
    ! CLOSE                 | Initialize      | INIT
    !----------------------------------------------------------------- 


    type :: xh5for_t
    private
        integer(I4P)                                  :: Strategy   = XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB
        integer(I4P)                                  :: GridType   = XDMF_GRID_TYPE_UNSTRUCTURED
        integer(I4P)                                  :: State      = XH5FOR_STATE_START
        integer(I4P)                                  :: Action     = XDMF_ACTION_WRITE
        logical                                       :: StaticGrid = .false.
        character(len=:),                 allocatable :: Prefix
        character(len=:),                 allocatable :: Path
        type(mpi_env_t)                               :: MPIEnvironment
        type(steps_handler_t)                         :: StepsHandler
        class(uniform_grid_descriptor_t), allocatable :: UniformGridDescriptor
        class(spatial_grid_descriptor_t), allocatable :: SpatialGridDescriptor
        class(xdmf_handler_t),            allocatable :: LightData
        class(hdf5_handler_t),            allocatable :: HeavyData
    contains
    private
        procedure         :: xh5for_Initialize
        procedure         :: xh5for_Set_Unstructured_Grid_I4P
        procedure         :: xh5for_Set_Unstructured_Grid_I8P
        procedure         :: xh5for_Set_Structured_Grid_I4P
        procedure         :: xh5for_Set_Structured_Grid_I8P
        procedure         :: xh5for_WriteGeometry_XYZ_R4P
        procedure         :: xh5for_WriteGeometry_XYZ_R8P
        procedure         :: xh5for_WriteGeometry_X_Y_Z_R4P
        procedure         :: xh5for_WriteGeometry_X_Y_Z_R8P
        procedure         :: xh5for_WriteGeometry_DXDYDZ_R4P
        procedure         :: xh5for_WriteGeometry_DXDYDZ_R8P
        procedure         :: xh5for_ReadGeometry_XYZ_R4P
        procedure         :: xh5for_ReadGeometry_XYZ_R8P
        procedure         :: xh5for_ReadGeometry_X_Y_Z_R4P
        procedure         :: xh5for_ReadGeometry_X_Y_Z_R8P
        procedure         :: xh5for_ReadGeometry_DXDYDZ_R4P
        procedure         :: xh5for_ReadGeometry_DXDYDZ_R8P
        procedure         :: xh5for_WriteTopology_I4P
        procedure         :: xh5for_WriteTopology_I8P
        procedure         :: xh5for_ReadTopology_I4P
        procedure         :: xh5for_ReadTopology_I8P
        procedure         :: xh5for_WriteAttribute_I4P
        procedure         :: xh5for_WriteAttribute_I8P
        procedure         :: xh5for_WriteAttribute_R4P
        procedure         :: xh5for_WriteAttribute_R8P
        procedure         :: xh5for_ReadAttribute_I4P
        procedure         :: xh5for_ReadAttribute_I8P
        procedure         :: xh5for_ReadAttribute_R4P
        procedure         :: xh5for_ReadAttribute_R8P
        procedure         :: SetStrategy           => xh5for_SetStrategy
        procedure         :: SetGridType           => xh5for_SetGridType
        procedure, public :: AppendStep            => xh5for_AppendStep
        procedure, public :: NextStep              => xh5for_NextStep
        procedure, public :: GetNumberOfSteps      => xh5for_GetNumberOfSteps
        procedure         :: Initialize            => xh5for_Initialize
        generic,   public :: SetGrid               => xh5for_Set_Unstructured_Grid_I4P, &
                                                      xh5for_Set_Unstructured_Grid_I8P, &
                                                      xh5for_Set_Structured_Grid_I4P,   &
                                                      xh5for_Set_Structured_Grid_I8P
        procedure, public :: Free                  => xh5for_Free
        procedure, public :: Clean                 => xh5for_Clean
        procedure, public :: Open                  => xh5for_Open
        procedure         :: CheckOpenHeavyDataFile=> xh5for_CheckOpenHeavyDataFile
        procedure, public :: ParseGrid             => xh5for_ParseGrid
        procedure, public :: Serialize             => xh5for_Serialize
        procedure, public :: Close                 => xh5for_Close
        generic,   public :: WriteTopology         => xh5for_WriteTopology_I4P, &
                                                      xh5for_WriteTopology_I8P
        generic,   public :: ReadTopology          => xh5for_ReadTopology_I4P, &
                                                      xh5for_ReadTopology_I8P
        generic,   public :: WriteGeometry         => xh5for_WriteGeometry_XYZ_R4P,   &
                                                      xh5for_WriteGeometry_XYZ_R8P,   &
                                                      xh5for_WriteGeometry_X_Y_Z_R4P, &
                                                      xh5for_WriteGeometry_X_Y_Z_R8P, &
                                                      xh5for_WriteGeometry_DXDYDZ_R4P,&
                                                      xh5for_WriteGeometry_DXDYDZ_R8P
        generic,   public :: ReadGeometry          => xh5for_ReadGeometry_XYZ_R4P,   &
                                                      xh5for_ReadGeometry_XYZ_R8P,   &
                                                      xh5for_ReadGeometry_X_Y_Z_R4P, &
                                                      xh5for_ReadGeometry_X_Y_Z_R8P, &
                                                      xh5for_ReadGeometry_DXDYDZ_R4P,&
                                                      xh5for_ReadGeometry_DXDYDZ_R8P
        generic,   public :: WriteAttribute        => xh5for_WriteAttribute_I4P, &
                                                      xh5for_WriteAttribute_I8P, &
                                                      xh5for_WriteAttribute_R4P, &
                                                      xh5for_WriteAttribute_R8P
        generic,   public :: ReadAttribute         => xh5for_ReadAttribute_I4P, &
                                                      xh5for_ReadAttribute_I8P, &
                                                      xh5for_ReadAttribute_R4P, &
                                                      xh5for_ReadAttribute_R8P
    end type xh5for_t

    class(xh5for_abstract_factory_t), allocatable, save :: TheFactory

public  :: xh5for_t
private :: uniform_grid_descriptor_t
private :: spatial_grid_descriptor_t
private :: steps_handler_t
private :: xdmf_handler_t
private :: hdf5_handler_t
private :: xh5for_abstract_factory_t
private :: TheXH5ForFactoryCreator

contains

    subroutine xh5for_SetStrategy(this, Strategy)
    !----------------------------------------------------------------- 
    !< Set the strategy of data handling
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT)  :: this
        integer(I4P),    intent(IN)     :: Strategy
    !----------------------------------------------------------------- 
        if(isSupportedStrategy(Strategy)) this%Strategy = Strategy
    end subroutine xh5for_SetStrategy


    subroutine xh5for_SetGridType(this, GridType)
    !----------------------------------------------------------------- 
    !< Set the strategy of data handling
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT)  :: this
        integer(I4P),    intent(IN)     :: GridType
    !----------------------------------------------------------------- 
        if(isSupportedGridType(GridType)) this%GridType = GridType
    end subroutine xh5for_SetGridType


    subroutine xh5for_AppendStep(this, Value)
    !----------------------------------------------------------------- 
    !< Append an step value to the serie and open HeavyData file
    !< for writing
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT)  :: this
        real(R8P),       intent(IN)     :: Value
    !----------------------------------------------------------------- 
        assert(this%Action == XDMF_ACTION_WRITE)
        if(this%State == XH5FOR_STATE_GRID_IO) then
            if(.not. this%StepsHandler%IsStaticStep() .and. .not. this%LightData%IsSpatialFileSerialized()) then
                call this%Serialize()
            endif
            call this%LightData%Clean()
        endif
        call this%StepsHandler%Append(Value=Value)
    end subroutine xh5for_AppendStep


    subroutine xh5for_NextStep(this)
    !----------------------------------------------------------------- 
    !< Go to next step, read and comunicate metadata and open HeavyData 
    !< file for reading
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT)  :: this
    !----------------------------------------------------------------- 
        assert(this%Action == XDMF_ACTION_READ)
        call this%LightData%CloseSpatialFile()
        call this%StepsHandler%Next()
    end subroutine xh5for_NextStep


    function xh5for_GetNumberOfSteps(this) result(NumberOfSteps)
    !----------------------------------------------------------------- 
    !< Return the number of steps in XDMF temporal file
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT)  :: this
        integer(I4P)                    :: NumberOfSteps
    !----------------------------------------------------------------- 
        if(this%State == XH5FOR_STATE_OPEN .and. this%Action == XDMF_ACTION_READ)  call this%Initialize()
        if(this%State == XH5FOR_STATE_INIT .and. this%Action == XDMF_ACTION_READ)  call this%LightData%ParseTemporalFile()
        NumberOfSteps = this%StepsHandler%GetNumberOfSteps()
    end function xh5for_GetNumberOfSteps


    subroutine xh5for_Free(this)
    !----------------------------------------------------------------- 
    !< Free XH5For derived type
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this
    !----------------------------------------------------------------- 
        this%Strategy = XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB
        if(allocated(this%LightData)) then
            call this%LightData%Free()
            deallocate(this%LightData)
        endif
        if(allocated(this%HeavyData)) then
            call this%HeavyData%Free()
            deallocate(this%HeavyData)
        endif
        if(allocated(this%UniformGridDescriptor)) then
            call this%UniformGridDescriptor%Free()
            deallocate(this%UniformGridDescriptor)
        endif
        if(allocated(this%SpatialGridDescriptor)) then
            call this%SpatialGridDescriptor%Free()
            deallocate(this%SpatialGridDescriptor)
        endif
        call this%MPIEnvironment%Free()
        call this%StepsHandler%Free()
        if(allocated(TheFactory)) deallocate(TheFactory)
        this%State = XH5FOR_STATE_START
    end subroutine xh5for_Free


    subroutine xh5for_Clean(this)
    !----------------------------------------------------------------- 
    !< Clean initialized
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this
    !----------------------------------------------------------------- 
        assert(this%State /= XH5FOR_STATE_START)
        ! Build components from factory
        if(allocated(this%UniformGridDescriptor)) then
            call this%UniformGridDescriptor%Free()
            deallocate(this%UniformGridDescriptor)
        endif
        if(allocated(this%SpatialGridDescriptor)) then
            call this%SpatialGridDescriptor%Free()
            deallocate(this%SpatialGridDescriptor)
        endif
        if(allocated(this%LightData)) then
            call this%LightData%Free()
            deallocate(this%LightData)
        endif
        if(allocated(this%HeavyData)) then
            call this%HeavyData%Free()
            deallocate(this%HeavyData)
        endif
        this%State = XH5FOR_STATE_OPEN
    end subroutine xh5for_Clean


    subroutine xh5for_CheckOpenHeavyDataFile(this, GridData)
    !----------------------------------------------------------------- 
    !< Check if the right HeavyDataFile is open. If not, open it
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this
        logical, optional, intent(IN)     :: GridData
        logical                           :: isGridData
    !----------------------------------------------------------------- 
        isGridData = .false.
        if(Present(GridData)) isGridData = GridData
        if(isGridData .and. this%SpatialGridDescriptor%isStaticGrid()) then
            if(this%HeavyData%IsOpen()) then
                if(.not. this%HeavyData%IsStepFileOpen(XDMF_STATIC_STEP)) call this%HeavyData%OpenFile(this%Action, This%Prefix, This%Path, XDMF_STATIC_STEP)
            else
                call this%HeavyData%OpenFile(this%Action, This%Prefix, this%Path, XDMF_STATIC_STEP)
            endif
        else
            if(this%HeavyData%IsOpen()) then
                if(.not. this%HeavyData%IsStepFileOpen(this%StepsHandler%GetCurrentStep())) call this%HeavyData%OpenFile(this%Action, This%Prefix, this%Path)
            else
                call this%HeavyData%OpenFile(this%Action, This%Prefix, this%Path)
            endif
        endif
    end subroutine


    subroutine xh5for_Open(this, FilePrefix, Path, GridType, StaticGrid, Strategy, Action, Comm, Info, Root)
    !-----------------------------------------------------------------
    !< Open a XDMF (Temporal) file, set the MPI environment and also
    !< initialize the steps handler
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        character(len=*),           intent(IN)    :: FilePrefix       !< XDMF filename prefix
        character(len=*), optional, intent(IN)    :: Path             !< Root path
        integer(I4P),     optional, intent(IN)    :: GridType         !< XDMF grid type
        logical,          optional, intent(IN)    :: StaticGrid       !< Static grid flag
        integer(I4P),     optional, intent(IN)    :: Strategy         !< Data IO management strategy
        integer(I4P),     optional, intent(IN)    :: Action           !< XDMF Open file action (Read or Write)
        integer,          optional, intent(IN)    :: Comm             !< MPI communicator
        integer,          optional, intent(IN)    :: Info             !< MPI info
        integer,          optional, intent(IN)    :: Root             !< MPI root procesor
        integer                                   :: error            !< Error variable
    !-----------------------------------------------------------------
        call this%Free()

        ! Assign Fileprefix, path, strategy and action
        this%Prefix = trim(adjustl(FilePrefix))
        this%Path   = '.'
        if(present(Strategy)) call this%SetStrategy(Strategy)
        if(present(GridType)) call this%SetGridType(GridType)
        if(present(StaticGrid)) this%StaticGrid = StaticGrid
        if(present(Action)) this%Action = Action
        if(present(Path))   this%Path = trim(adjustl(Path))

        ! MPI environment initialization
        call This%MPIEnvironment%Initialize(comm = comm, root = root, mpierror = error)
        assert(error == 0)

        ! Create output directory if does not exist
        if(this%Action == XDMF_ACTION_WRITE) then
            if(This%MPIEnvironment%is_root()) then
                error = MkdirFullPath(this%Path)
                assert(error == 0)
            endif
        endif
        call This%MPIEnvironment%Barrier(mpierror = error)
        assert(error == 0)

        ! Steps initialization
        call this%StepsHandler%Initialize(this%MPIEnvironment)
        this%State = XH5FOR_STATE_OPEN
    end subroutine xh5for_Open


    subroutine xh5for_Initialize(this)
    !----------------------------------------------------------------- 
    !< Apply strategy and create all components
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
    !----------------------------------------------------------------- 
        assert(this%State == XH5FOR_STATE_OPEN)
        call this%Clean()
        ! Build components from factory
        call TheXH5ForFactoryCreator%CreateFactory(GridType=this%GridType, Strategy=this%Strategy, AbstractFactory=TheFactory)
        call TheFactory%CreateUniformGridDescriptor(this%UniformGridDescriptor)
        call TheFactory%CreateSpatialGridDescriptor(this%SpatialGridDescriptor)
        call TheFactory%CreateXDMFHandler(this%LightData)
        call TheFactory%CreateHDF5Handler(this%HeavyData)
        call this%SpatialGridDescriptor%Initialize(MPIEnvironment = this%MPIEnvironment, StaticGrid = this%StaticGrid)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor, &
                FilePrefix            = this%Prefix,                &
                Path                  = this%Path,                  &
                Action                = this%Action)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        this%State = XH5FOR_STATE_INIT
    end subroutine xh5for_Initialize


    subroutine xh5for_Set_Unstructured_Grid_I4P(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
    !----------------------------------------------------------------- 
    !< Set mesh metadata and initialize all library components
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer(I4P),      intent(IN)     :: NumberOfNodes            !< Number of nodes of the current grid (I4P)
        integer(I4P),      intent(IN)     :: NumberOfElements         !< Number of elements of the current grid (I4P)
        integer(I4P),      intent(IN)     :: TopologyType             !< Topology type of the current grid
        integer(I4P),      intent(IN)     :: GeometryType             !< Geometry type of the current grid
    !----------------------------------------------------------------- 
        assert(this%State >= XH5FOR_STATE_OPEN .and. this%GridType == XDMF_GRID_TYPE_UNSTRUCTURED)
        if(this%State == XH5FOR_STATE_OPEN) call this%Initialize()
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize(           &
                NumberOfNodes    = int(NumberOfNodes,I8P),    &
                NumberOfElements = int(NumberOfElements,I8P), &
                TopologyType     = TopologyType,              &
                GeometryType     = GeometryType,              &
                GridType         = this%GridType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(            &
                MPIEnvironment   = this%MPIEnvironment,        &
                NumberOfNodes    = int(NumberOfNodes,I8P),     &
                NumberOfElements = int(NumberOfElements,I8P),  &
                TopologyType     = TopologyType,               &
                GeometryType     = GeometryType,               &
                GridType         = this%GridType,              &
                StaticGrid       = this%StaticGrid)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor, &
                FilePrefix            = this%Prefix,                &
                Path                  = this%Path,                  &
                Action                = this%Action)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        this%State = XH5FOR_STATE_GRID_SET
    end subroutine xh5for_Set_Unstructured_Grid_I4P


    subroutine xh5for_Set_Unstructured_Grid_I8P(this, NumberOfNodes, NumberOfElements, TopologyType, GeometryType)
    !----------------------------------------------------------------- 
    !< Set mesh metadata and initialize all library components
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer(I8P),      intent(IN)     :: NumberOfNodes            !< Number of nodes of the current grid (I8P)
        integer(I8P),      intent(IN)     :: NumberOfElements         !< Number of elements of the current grid (I8P)
        integer(I4P),      intent(IN)     :: TopologyType             !< Topology type of the current grid
        integer(I4P),      intent(IN)     :: GeometryType             !< Geometry type of the current grid
    !----------------------------------------------------------------- 
        assert(this%State >= XH5FOR_STATE_OPEN .and. this%GridType == XDMF_GRID_TYPE_UNSTRUCTURED)
        if(this%State == XH5FOR_STATE_OPEN) call this%Initialize()
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize(           &
                NumberOfNodes    = int(NumberOfNodes,I8P),    &
                NumberOfElements = int(NumberOfElements,I8P), &
                TopologyType     = TopologyType,              &
                GeometryType     = GeometryType,              &
                GridType         = this%GridType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(           &
                MPIEnvironment   = this%MPIEnvironment,       &
                NumberOfNodes    = int(NumberOfNodes,I8P),    &
                NumberOfElements = int(NumberOfElements,I8P), &
                TopologyType     = TopologyType,              &
                GeometryType     = GeometryType,              &
                GridType         = this%GridType,             &
                StaticGrid       = this%StaticGrid)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor, &
                FilePrefix            = this%Prefix,                &
                Path                  = this%Path,                  &
                Action                = this%Action)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        this%State = XH5FOR_STATE_GRID_SET
    end subroutine xh5for_Set_Unstructured_Grid_I8P


    subroutine xh5for_Set_Structured_Grid_I4P(this, GridShape)
    !----------------------------------------------------------------- 
    !< Set mesh metadata and initialize all library components
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer(I4P),      intent(IN)     :: GridShape(3)             !< Shape of the grid
        integer(I8P)                      :: NumberOfNodes            !< Number of nodes of the current grid (I4P)
        integer(I8P)                      :: NumberOfElements         !< Number of elements of the current grid (I4P)
        integer(I4P)                      :: TopologyType             !< Topology type of the current grid
        integer(I4P)                      :: GeometryType             !< Geometry type of the current grid
    !----------------------------------------------------------------- 
        assert(this%State >= XH5FOR_STATE_OPEN .and. (this%GridType == XDMF_GRID_TYPE_REGULAR .or. this%GridType == XDMF_GRID_TYPE_RECTILINEAR))
        if(this%State == XH5FOR_STATE_OPEN) call this%Initialize()
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize( &
                XDim     = int(GridShape(1),I8P),   &
                YDim     = int(GridShape(2),I8P),   &
                ZDim     = int(GridShape(3),I8P),   &
                GridType = this%GridType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(          &
                MPIEnvironment = this%MPIEnvironment,        &
                XDim       = int(GridShape(1),I8P),          &
                YDim       = int(GridShape(2),I8P),          &
                ZDim       = int(GridShape(3),I8P),          &
                GridType   = this%GridType,                  &
                StaticGrid = this%StaticGrid)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor, &
                FilePrefix            = this%Prefix,                &
                Path                  = this%Path,                  &
                Action                = this%Action)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        this%State = XH5FOR_STATE_GRID_SET
    end subroutine xh5for_Set_Structured_Grid_I4P


    subroutine xh5for_Set_Structured_Grid_I8P(this, GridShape)
    !----------------------------------------------------------------- 
    !< Set mesh metadata and initialize all library components
    !----------------------------------------------------------------- 
        class(xh5for_t),   intent(INOUT)  :: this                     !< XH5For derived type
        integer(I8P),      intent(IN)     :: GridShape(3)             !< GridShape
        integer(I8P)                      :: NumberOfNodes            !< Number of nodes of the current grid (I4P)
        integer(I8P)                      :: NumberOfElements         !< Number of elements of the current grid (I4P)
        integer(I4P)                      :: TopologyType             !< Topology type of the current grid
        integer(I4P)                      :: GeometryType             !< Geometry type of the current grid
    !----------------------------------------------------------------- 
        assert(this%State >= XH5FOR_STATE_OPEN .and. (this%GridType == XDMF_GRID_TYPE_REGULAR .or. this%GridType == XDMF_GRID_TYPE_RECTILINEAR))
        if(this%State == XH5FOR_STATE_OPEN) call this%Initialize()
        ! Uniform grid descriptor initialization
        call this%UniformGridDescriptor%Initialize( &
                XDim = GridShape(1),                &
                YDim = GridShape(2),                &
                ZDim = GridShape(3),                &
                GridType = this%GridType)
        ! Spatial grid descriptor initialization
        call this%SpatialGridDescriptor%Initialize(            &
                MPIEnvironment   = this%MPIEnvironment,        &
                XDim       = int(GridShape(1),I8P),            &
                YDim       = int(GridShape(2),I8P),            &
                ZDim       = int(GridShape(3),I8P),            &
                GridType   = this%GridType,                    &
                StaticGrid = this%StaticGrid)
        ! Light data initialization
        call this%LightData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor, &
                FilePrefix            = this%Prefix,                &
                Path                  = this%Path,                  &
                Action                = this%Action)
        ! Heavy data initialization
        call this%HeavyData%Initialize(                             &
                MPIEnvironment        = this%MPIEnvironment,        &
                StepsHandler          = this%StepsHandler,          &
                UniformGridDescriptor = this%UniformGridDescriptor, &
                SpatialGridDescriptor = this%SpatialGridDescriptor)
        this%State = XH5FOR_STATE_GRID_SET
    end subroutine xh5for_Set_Structured_Grid_I8P


    subroutine xh5for_Serialize(this)
    !-----------------------------------------------------------------
    !< Serialize a XDMF file
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
    !-----------------------------------------------------------------
        assert(this%State == XH5FOR_STATE_GRID_IO)
        if(this%Action == XDMF_ACTION_WRITE) then
            call this%LightData%SerializeSpatialFile()
            call this%LightData%SerializeTemporalFile()
            if(this%HeavyData%IsOpen()) call this%HeavyData%CloseFile()
        endif
    end subroutine xh5for_Serialize


    subroutine xh5for_ParseGrid(this)
    !-----------------------------------------------------------------
    !< Parse a XDMF file
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
    !-----------------------------------------------------------------
        assert((this%State >= XH5FOR_STATE_OPEN .and. this%State /= XH5FOR_STATE_CLOSE) .and. this%Action == XDMF_ACTION_READ)
        if(this%State == XH5FOR_STATE_OPEN) call this%Initialize()
        call this%LightData%ParseSpatialFile()
        this%State = XH5FOR_STATE_GRID_SET
    end subroutine xh5for_ParseGrid


    subroutine xh5for_Close(this)
    !-----------------------------------------------------------------
    !< Open a XDMF and HDF5 files
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
    !-----------------------------------------------------------------
        if(this%State == XH5FOR_STATE_GRID_IO .and. this%action == XDMF_ACTION_WRITE) then
            if(this%HeavyData%IsOpen()) call this%HeavyData%CloseFile()
            if(.not. this%LightData%IsSpatialFileSerialized()) call this%Serialize()
            call this%LightData%Clean()
        endif
        this%State = XH5FOR_STATE_CLOSE
    end subroutine xh5for_Close


    subroutine xh5for_WriteGeometry_XYZ_R4P(this, XYZ, Name)
    !----------------------------------------------------------------- 
    !< Write R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P),                  intent(IN)    :: XYZ(:)           !< R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = XYZ, Name = Name)
            call this%HeavyData%WriteGeometry(XYZ = XYZ, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = XYZ, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(XYZ = XYZ, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteGeometry_XYZ_R4P


    subroutine xh5for_WriteGeometry_XYZ_R8P(this, XYZ, Name)
    !----------------------------------------------------------------- 
    !< Write R8P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type                        
        real(R8P),                  intent(IN)    :: XYZ(:)           !< R8P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = XYZ, Name = Name)
            call this%HeavyData%WriteGeometry(XYZ = XYZ, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = XYZ, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(XYZ = XYZ, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteGeometry_XYZ_R8P


    subroutine xh5for_WriteGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !----------------------------------------------------------------- 
    !< Write R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P),                  intent(IN)    :: X(:)             !< X R4P grid geometry coordinates
        real(R4P),                  intent(IN)    :: Y(:)             !< Y R4P grid geometry coordinates
        real(R4P),                  intent(IN)    :: Z(:)             !< Z R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = X, Name = Name)
            call this%HeavyData%WriteGeometry(X = X, Y = Y, Z = Z, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = X, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(X = X, Y = Y, Z = Z, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteGeometry_X_Y_Z_R4P


    subroutine xh5for_WriteGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !----------------------------------------------------------------- 
    !< Write R8P X_Y_Z Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P),                  intent(IN)    :: Origin(:)        !< Origin of the grid coordinates
        real(R4P),                  intent(IN)    :: DxDyDz(:)        !< Step to the next point of the grid
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = Origin, Name = Name)
            call this%HeavyData%WriteGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = Origin, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteGeometry_DXDYDZ_R4P


    subroutine xh5for_WriteGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !----------------------------------------------------------------- 
    !< Write R8P DXDYDZ Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R8P),                  intent(IN)    :: Origin(:)        !< Origin of the grid coordinates
        real(R8P),                  intent(IN)    :: DxDyDz(:)        !< Step to the next point of the grid
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = Origin, Name = Name)
            call this%HeavyData%WriteGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = Origin, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteGeometry_DXDYDZ_R8P


    subroutine xh5for_WriteGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !----------------------------------------------------------------- 
    !< Write R8P X_Y_Z Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R8P),                  intent(IN)    :: X(:)             !< X R4P grid geometry coordinates
        real(R8P),                  intent(IN)    :: Y(:)             !< Y R4P grid geometry coordinates
        real(R8P),                  intent(IN)    :: Z(:)             !< Z R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%LightData%SetGeometry(XYZ = X, Name = Name)
            call this%HeavyData%WriteGeometry(X = X, Y = Y, Z = Z, Name = Name)
        else
            call this%LightData%SetGeometry(XYZ = X, Name = 'Coordinates')
            call this%HeavyData%WriteGeometry(X = X, Y = Y, Z = Z, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteGeometry_X_Y_Z_R8P


    subroutine xh5for_ReadGeometry_XYZ_R4P(this, XYZ, Name)
    !----------------------------------------------------------------- 
    !< Read XY[Z] R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P), allocatable,     intent(OUT)   :: XYZ(:)           !< R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(XYZ = XYZ, Name = Name)
            call this%LightData%SetGeometry(XYZ = XYZ, Name = Name)
        else
            call this%HeavyData%ReadGeometry(XYZ = XYZ, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = XYZ, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadGeometry_XYZ_R4P


    subroutine xh5for_ReadGeometry_XYZ_R8P(this, XYZ, Name)
    !----------------------------------------------------------------- 
    !< Read XY[Z] R8P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R8P), allocatable,     intent(OUT)   :: XYZ(:)           !< R8P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(XYZ = XYZ, Name = Name)
            call this%LightData%SetGeometry(XYZ = XYZ, Name = Name)
        else
            call this%HeavyData%ReadGeometry(XYZ = XYZ, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = XYZ, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadGeometry_XYZ_R8P


    subroutine xh5for_ReadGeometry_X_Y_Z_R4P(this, X, Y, Z, Name)
    !----------------------------------------------------------------- 
    !< Read X_Y_Z R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P), allocatable,     intent(OUT)   :: X(:)             !< X R4P grid geometry coordinates
        real(R4P), allocatable,     intent(OUT)   :: Y(:)             !< Y R4P grid geometry coordinates
        real(R4P), allocatable,     intent(OUT)   :: Z(:)             !< Z R4P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_REAd)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(X = X, Y = Y, Z = Z, Name = Name)
            call this%LightData%SetGeometry(XYZ = X, Name = Name)
        else
            call this%HeavyData%ReadGeometry(X = X, Y = Y, Z = Z, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = X, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadGeometry_X_Y_Z_R4P


    subroutine xh5for_ReadGeometry_X_Y_Z_R8P(this, X, Y, Z, Name)
    !----------------------------------------------------------------- 
    !< Read X_Y_Z R8P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R8P), allocatable,     intent(OUT)   :: X(:)             !< X R8P grid geometry coordinates
        real(R8P), allocatable,     intent(OUT)   :: Y(:)             !< Y R8P grid geometry coordinates
        real(R8P), allocatable,     intent(OUT)   :: Z(:)             !< Z R8P grid geometry coordinates
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(X = X, Y = Y, Z = Z, Name = Name)
            call this%LightData%SetGeometry(XYZ = X, Name = Name)
        else
            call this%HeavyData%ReadGeometry(X = X, Y = Y, Z = Z, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = X, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadGeometry_X_Y_Z_R8P


    subroutine xh5for_ReadGeometry_DXDYDZ_R4P(this, Origin, DxDyDz, Name)
    !----------------------------------------------------------------- 
    !< Read DXDYDZ R4P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R4P), allocatable,     intent(OUT)   :: Origin(:)        !< Origin of the grid coordinates
        real(R4P), allocatable,     intent(OUT)   :: DxDyDz(:)        !< Step to the next point of the grid
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = Name)
            call this%LightData%SetGeometry(XYZ = Origin, Name = Name)
        else
            call this%HeavyData%ReadGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = Origin, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadGeometry_DXDYDZ_R4P


    subroutine xh5for_ReadGeometry_DXDYDZ_R8P(this, Origin, DxDyDz, Name)
    !----------------------------------------------------------------- 
    !< Read DXDYDZ R8P Geometry
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this             !< XH5For derived type
        real(R8P), allocatable,     intent(OUT)   :: Origin(:)        !< Origin of the grid coordinates
        real(R8P), allocatable,     intent(OUT)   :: DxDyDz(:)        !< Step to the next point of the grid
        character(len=*), optional, intent(IN)    :: Name             !< Geometry dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%HeavyData%ReadGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = Name)
            call this%LightData%SetGeometry(XYZ = Origin, Name = Name)
        else
            call this%HeavyData%ReadGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = 'Coordinates')
            call this%LightData%SetGeometry(XYZ = Origin, Name = 'Coordinates')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadGeometry_DXDYDZ_R8P


    subroutine xh5for_WriteTopology_I4P(this, Connectivities, Name)
    !----------------------------------------------------------------- 
    !< Write I4P Topology
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this              !< XH5For derived type
        integer(I4P),               intent(IN)    :: Connectivities(:) !< I4P grid topology connectivities
        character(len=*), optional, intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
            call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = 'Connectivities')
            call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = 'Connectivities')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteTopology_I4P


    subroutine xh5for_WriteTopology_I8P(this, Connectivities, Name)
    !----------------------------------------------------------------- 
    !< Write I8P Topology
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this              !< XH5For derived type
        integer(I8P),               intent(IN)    :: Connectivities(:) !< I8P grid topology connectivities
        character(len=*), optional, intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
            call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = 'Connectivities')
            call this%HeavyData%WriteTopology(Connectivities = Connectivities, Name = 'Connectivities')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteTopology_I8P


    subroutine xh5for_ReadTopology_I4P(this, Connectivities, Name)
    !----------------------------------------------------------------- 
    !< Read I4P Topology
    !----------------------------------------------------------------- 
        class(xh5for_t),           intent(INOUT) :: this              !< XH5For derived type
        integer(I4P), allocatable, intent(OUT)   :: Connectivities(:) !< I4P grid topology connectivities
        character(len=*),optional, intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
!        if(this%SpatialGridDescriptor%isStaticGrid()) call this%LightData%ParseSpatialFile(FirstStep=.true.)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = Name)
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = 'Connectivities')
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = 'Connectivities')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadTopology_I4P


    subroutine xh5for_ReadTopology_I8P(this, Connectivities, Name)
    !----------------------------------------------------------------- 
    !< Read I8P Topology
    !----------------------------------------------------------------- 
        class(xh5for_t),            intent(INOUT) :: this              !< XH5For derived type
        integer(I8P), allocatable,  intent(OUT)   :: Connectivities(:) !< I8P grid topology connectivities
        character(len=*), optional, intent(IN)    :: Name              !< Topology dataset name
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile(GridData=.true.)
        if(present(Name)) then
            call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = Name)
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = Name)
        else
            call this%HeavyData%ReadTopology(Connectivities = Connectivities, Name = 'Connectivities')
            call this%LightData%SetTopology(Connectivities = Connectivities, Name = 'Connectivities')
        endif
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadTopology_I8P


    subroutine xh5for_WriteAttribute_I4P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Write I4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
        character(len=*),intent(IN)    :: Name                        !< Attribute name
        integer(I4P),    intent(IN)    :: Type                        !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),    intent(IN)    :: Center                      !< Attribute centered at (Node, Cell, etc.)
        integer(I4P),    intent(IN)    :: Values(:)                   !< I4P grid attribute values
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%CheckOpenHeavyDataFile()
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteAttribute_I4P


    subroutine xh5for_WriteAttribute_I8P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Write I4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
        character(len=*),intent(IN)    :: Name                        !< Attribute name
        integer(I4P),    intent(IN)    :: Type                        !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),    intent(IN)    :: Center                      !< Attribute centered at (Node, Cell, etc.)
        integer(I8P),    intent(IN)    :: Values(:)                   !< I8P grid attribute values
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%CheckOpenHeavyDataFile()
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteAttribute_I8P


    subroutine xh5for_WriteAttribute_R4P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Write I4P Attributed
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
        character(len=*),intent(IN)    :: Name                        !< Attribute name
        integer(I4P),    intent(IN)    :: Type                        !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),    intent(IN)    :: Center                      !< Attribute centered at (Node, Cell, etc.)
        real(R4P),       intent(IN)    :: Values(:)                   !< R4P grid attribute values
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%CheckOpenHeavyDataFile()
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteAttribute_R4P


    subroutine xh5for_WriteAttribute_R8P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Write I4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t), intent(INOUT) :: this                        !< XH5For derived type
        character(len=*),intent(IN)    :: Name                        !< Attribute name
        integer(I4P),    intent(IN)    :: Type                        !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),    intent(IN)    :: Center                      !< Attribute centered at (Node, Cell, etc.)
        real(R8P),       intent(IN)    :: Values(:)                   !< R8P grid attribute values
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_WRITE)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call this%CheckOpenHeavyDataFile()
        call this%HeavyData%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_WriteAttribute_R8P


    subroutine xh5for_ReadAttribute_I4P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Read I4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t),           intent(INOUT) :: this              !< XH5For derived type
        character(len=*),          intent(IN)    :: Name              !< Attribute name
        integer(I4P),              intent(IN)    :: Type              !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),              intent(IN)    :: Center            !< Attribute centered at (Node, Cell, etc.)
        integer(I4P), allocatable, intent(OUT)   :: Values(:)         !< I4P grid attribute values
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile()
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadAttribute_I4P


    subroutine xh5for_ReadAttribute_I8P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Read I8P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t),           intent(INOUT) :: this              !< XH5For derived type
        character(len=*),          intent(IN)    :: Name              !< Attribute name
        integer(I4P),              intent(IN)    :: Type              !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),              intent(IN)    :: Center            !< Attribute centered at (Node, Cell, etc.)
        integer(I8P), allocatable, intent(OUT)   :: Values(:)         !< I8P grid attribute values
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile()
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadAttribute_I8P


    subroutine xh5for_ReadAttribute_R4P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Read R4P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t),        intent(INOUT) :: this                 !< XH5For derived type
        character(len=*),       intent(IN)    :: Name                 !< Attribute name
        integer(I4P),           intent(IN)    :: Type                 !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),           intent(IN)    :: Center               !< Attribute centered at (Node, Cell, etc.)
        real(R4P), allocatable, intent(OUT)   :: Values(:)            !< R4P grid attribute values
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile()
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadAttribute_R4P


    subroutine xh5for_ReadAttribute_R8P(this, Name, Type, Center, Values)
    !----------------------------------------------------------------- 
    !< Read R8P Attribute
    !----------------------------------------------------------------- 
        class(xh5for_t),        intent(INOUT) :: this                 !< XH5For derived type
        character(len=*),       intent(IN)    :: Name                 !< Attribute name
        integer(I4P),           intent(IN)    :: Type                 !< Attribute type (Scalar, Vector, etc.)
        integer(I4P),           intent(IN)    :: Center               !< Attribute centered at (Node, Cell, etc.)
        real(R8P), allocatable, intent(OUT)   :: Values(:)            !< R8P grid attribute values
    !-----------------------------------------------------------------
        assert((this%State == XH5FOR_STATE_GRID_SET .or. this%State == XH5FOR_STATE_GRID_IO) .and. this%Action == XDMF_ACTION_READ)
        call this%CheckOpenHeavyDataFile()
        call this%HeavyData%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call this%LightData%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        this%State = XH5FOR_STATE_GRID_IO
    end subroutine xh5for_ReadAttribute_R8P

end module xh5for_handler

module xh5for

use xh5for_handler, only: xh5for_t
use xh5for_parameters

end module  xh5for
