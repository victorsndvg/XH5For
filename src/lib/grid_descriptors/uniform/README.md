# XH5For
The Fortran way to easy write parallel partitioned XDMF/HDF5 meshes

[![Build Status](https://travis-ci.org/victorsndvg/XH5For.svg?branch=master)](https://travis-ci.org/victorsndvg/XH5For)
[![codecov](https://codecov.io/gh/victorsndvg/XH5For/branch/master/graph/badge.svg)](https://codecov.io/gh/victorsndvg/XH5For)

## License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)

## XH5For: Uniform Grid Descriptor

*Uniform Grid Descriptor* software subsystem is the one in charge of storing metadata related with the local view of the grid.

*XDMF Handler* uses the *Uniform Grid Descriptor* while serializing and parsing an XDMF file.

## XH5For: Uniform Grid Descriptor Basic usage

Unstructured grid:

```fortran
        call Uniform_Grid_Descriptor%Initialize(NumberOfNodes, NumberOfElements, TopologyType, GeometryType, GridType)
        call Uniform_Grid_Descriptor%SetGeometryMetadata(Name, Precision, ArrayDimensions)
        call Uniform_Grid_Descriptor%SetTopologyMetadata(Name, Precision, ArrayDimensions)
        call Uniform_Grid_Descriptor%AppendAttributeMetadata(Name, Type, DataType, Center, Precision, ArrayDimensions)
        call Uniform_Grid_Descriptor%Free()
```

Structured grid:

```fortran
        call Uniform_Grid_Descriptor%Initialize(Xdim, YDim, ZDim, GridType)
        call Uniform_Grid_Descriptor%SetGeometryMetadata(Name, Precision, ArrayDimensions)
        call Uniform_Grid_Descriptor%SetTopologyMetadata(Name, Precision, ArrayDimensions)
        call Uniform_Grid_Descriptor%AppendAttributeMetadata(Name, Type, DataType, Center, Precision, ArrayDimensions)
        call Uniform_Grid_Descriptor%Free()
```

