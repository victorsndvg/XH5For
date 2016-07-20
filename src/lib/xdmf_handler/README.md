# XH5For
The Fortran way to easy write parallel partitioned XDMF/HDF5 meshes

[![Build Status](https://travis-ci.org/victorsndvg/XH5For.svg?branch=master)](https://travis-ci.org/victorsndvg/XH5For)
[![codecov](https://codecov.io/gh/victorsndvg/XH5For/branch/master/graph/badge.svg)](https://codecov.io/gh/victorsndvg/XH5For)

## License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)

## XH5For: XDMF Handler 

*XDMF Handler* software subsystem is the one in charge of XML file serialization and parsing. 

*XDMF Handler* stores (via **UniformGridDescriptor** .and. **SpatialGridDescriptor**) all data needed to serialize and parse XDFM files. Only *Geometry* and *Topology* metadatada is stored while parsing.

It relies on [FoX](https://github.com/andreww/fox) (Fortran XML library) to serialize/parse XDMF files.

[XDMF Model and Format](http://www.xdmf.org/index.php/XDMF_Model_and_Format)

## XH5For: XDMF Handler Basic usage

Serialization:

```fortran
        call xdmf_handler%Initialize(MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor, FilePrefix, Action)
        call xdmf_handler%SetGeometry(XYZ = XYZ, Name = Name)
        call xdmf_handler%SetTopology(Connectivities = Connectivities, Name = Name)
        call xdmf_handler%AppendAttribute(Name = Name, Type = Type, Center = Center, Attribute = Values)
        call xdmf_handler%SerializeSpatialFile()
        call xdmf_handler%SerializeTemporalFile()
        call xdmf_handler%Free()
```

Parsing:

```fortran
        call xdmf_handler%Initialize(MPIEnvironment, StepsHandler, UniformGridDescriptor, SpatialGridDescriptor, FilePrefix, Action)
        call xdmf_handler%ParseTemporalFile()
        call xdmf_handler%ParseSpatialFile()
        call xdmf_handler%Free()
```

## XH5For: XDMF Handler state transition diagram

```fortran
    !-----------------------------------------------------------------
    ! XDMF_HANDLER State Transition Diagram
    !-----------------------------------------------------------------
    ! - This diagram controls the basic life cycle of the XDMF_HANDLER
    ! - After initialization, calls to almost all procedures are allowed
    ! - The state diagram is really simple. Most of the checks also
    !   realies in the allocation status of the file prefix and in the 
    !   Action status. Writing/Reading operations are mutually exclusive
    !----------------------------------------------------------------- 
    !       INIT STATE      |     ACTION      |      FINAL STATE
    !----------------------------------------------------------------- 
    ! START                 | Free            | START
    ! START                 | Initialize      | INIT
    !----------------------------------------------------------------- 
    ! INIT                  | Free            | START
    ! INIT                  | Initialize      | INIT
```

