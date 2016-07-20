# XH5For
The Fortran way to easy write parallel partitioned XDMF/HDF5 meshes

[![Build Status](https://travis-ci.org/victorsndvg/XH5For.svg?branch=master)](https://travis-ci.org/victorsndvg/XH5For)
[![codecov](https://codecov.io/gh/victorsndvg/XH5For/branch/master/graph/badge.svg)](https://codecov.io/gh/victorsndvg/XH5For)

## License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)

## XH5For: HDF5 Handler 

*HDF5 Handler* software subsystem is the one in charge of manage the HDF5 library in order to perform Collective/Independant IO. 

*HDF5 Handler* doesn't store any data, it uses **SpatialGridDescriptor** in order to calculate HDF5 metadata.
Only structured grids need write or read a topology, in other cases topology is implicit

It relies on [HDF5](https://www.hdfgroup.org/HDF5).

Two strategies are currently implemented:
  - Contiguous Hyperslabs
  - One dataset per process

## XH5For: HDF5 Handler Basic usage

### Unstructured grids:

Write:

```fortran
        call hdf5_handler%Initialize(MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor)
        call hdf5_handler%OpenFile(Action, Prefix)
        call hdf5_handler%WriteGeometry(XYZ = XYZ, Name = Name)
        call hdf5_handler%WriteTopology(Connectivities = Connectivities, Name = Name)
        call hdf5_handler%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call hdf5_handler%CloseFile()
```

Read:

```fortran
        call hdf5_handler%Initialize(MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor)
        call hdf5_handler%OpenFile(Action, Prefix)
        call hdf5_handler%ReadGeometry(XYZ = XYZ, Name = Name)
        call hdf5_handler%ReadTopology(Connectivities = Connectivities, Name = Name)
        call hdf5_handler%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call hdf5_handler%CloseFile()
```

### Rectilinear grids:

Write:

```fortran
        call hdf5_handler%Initialize(MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor)
        call hdf5_handler%OpenFile(Action, Prefix)
        call hdf5_handler%WriteGeometry(X = X, Y = Y, Z = Z, Name = Name)
        call hdf5_handler%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call hdf5_handler%CloseFile()
```

Read:

```fortran
        call hdf5_handler%Initialize(MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor)
        call hdf5_handler%OpenFile(Action, Prefix)
        call hdf5_handler%ReadGeometry(X = X, Y = Y, Z = Z, Name = Name)
        call hdf5_handler%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call hdf5_handler%CloseFile()
```

### Regular grids:

Write:

```fortran
        call hdf5_handler%Initialize(MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor)
        call hdf5_handler%OpenFile(Action, Prefix)
        call hdf5_handler%WriteGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = Name)
        call hdf5_handler%WriteAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call hdf5_handler%CloseFile()
```

Read:

```fortran
        call hdf5_handler%Initialize(MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor)
        call hdf5_handler%OpenFile(Action, Prefix)
        call hdf5_handler%ReadGeometry(Origin = Origin, DxDyDz = DxDyDz, Name = Name)
        call hdf5_handler%ReadAttribute(Name = Name, Type = Type, Center = Center, Values = Values)
        call hdf5_handler%CloseFile()
```


## XH5For: HDF5 Handler state transition diagram

```fortran
    !-----------------------------------------------------------------
    ! HDF5_HANDLER State Transition Diagram
    !-----------------------------------------------------------------
    ! - This diagram controls the basic life cycle of the HDF5 file.
    ! - Only a public procedure (IsOpen) is needed to check if the
    !   handler is in the right state to perform I/O operations.
    ! - Only the next hierarchy layer needs to ensure this status via
    !   ReadHyperSlabs/WriteHyperSlabs/ReadDataset/WriteData/WriteMetadata
    !   procedures
    !----------------------------------------------------------------- 
    !       INIT STATE      |     ACTION      |      FINAL STATE
    !----------------------------------------------------------------- 
    ! START                 | Free            | START
    ! START                 | Initialize      | INIT
    !----------------------------------------------------------------- 
    ! INIT                  | Free            | START
    ! INIT                  | Initialize      | INIT
    ! INIT                  | OpenFile        | OPEN
    !----------------------------------------------------------------- 
    ! OPEN                  | Free            | START
    ! OPEN                  | Initialize      | INIT
    ! OPEN                  | OpenFile        | OPEN
    ! OPEN                  | CloseFile       | CLOSE
    !----------------------------------------------------------------- 
    ! CLOSE                 | Free            | START
    ! CLOSE                 | Initialize      | INIT
    ! CLOSE                 | OpenFile        | OPEN
    ! CLOSE                 | CloseFile       | CLOSE
    !----------------------------------------------------------------- 
```

