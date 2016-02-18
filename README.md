# XH5For
The Fortran way to easy write partitioned XDMF/HDF5 meshes

[![Build Status](https://travis-ci.org/victorsndvg/XH5For.svg?branch=master)](https://travis-ci.org/victorsndvg/XH5For)
[![codecov.io](https://codecov.io/github/victorsndvg/XH5For/coverage.svg?branch=master)](https://codecov.io/github/victorsndvg/XH5For?branch=master)

##License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)                 

## What is XH5For?
Is a library to read and write partitioned FEM meshes taking advantage of the collective/independent IO provided by the HDF5 library. 

## Some concepts
* [XDMF Model and Format](http://www.xdmf.org/index.php/XDMF_Model_and_Format) is a standarized way to describe common scientific data. It separates the **Light data** *(XML description of the data model)* and the **Heavy data** *(raw values)* usually saved in HDF5 or binary Files.
* [HDF5](https://www.hdfgroup.org/HDF5) is a model and data format designed for efficient I/O and work with big amounts of data.

## Some considerations
* **XH5For** use [XDMF Model and Format](http://www.xdmf.org/index.php/XDMF_Model_and_Format) to describe the **Light data** and [HDF5](https://www.hdfgroup.org/HDF5) for the **Heavy data**.
* HDF5 API need to be linked to read/write the **Heavy data**.
* The first approach to the HDF5 IO will be based on HyperSlabs in order to reduce metadata.
* As starting point XML syntax will be based on XDMF v2.x.
* The handling of XML files is developed on top of [FoX](https://github.com/andreww/fox) (Fortran XML library) for reading/writing XDMF files. Particularly, *FoX_wxml* library is used for writing XDMF files and *FoX_dom* will be used for parsing.

## Under development strategies:
  * Contiguous hyperslab
  * One dataset per process strategy)

### Exporters:
  * Mesh:
    * [x] Structured
    * [x] Unstructured
  * Element:
    * Linear
      * [x] Triangle
      * [x] Quadrangle
      * [x] Tretrahedron
      * [x] Hexahedron
      * [x] Mixed
      * [ ] ...
  * Field:
    * Center on: 
        * [x] Node
        * [x] Cell
        * [x] Grid
        * [ ] Edge
        * [ ] Face
    * Type:
        * [x] Scalar
        * [x] Vector
        * [ ] Tensor
        * [ ] Matrix
        * [ ] Tensor6
        * [ ] GlobalID
        * [ ] NoType
  * XDMF tags ([FoX wxml](https://github.com/andreww/fox)  wrapper):
    * [x] Xdmf
    * [x] Domain
    * [x] Grid
    * [x] Topology
    * [x] Geometry
    * [x] Attribute
    * [x] DataItem
    * [x] Character data
  * HDF5:
    * [ ] Manage Files
    * [x] Groups
    * [x] DataSpaces
    * [x] DataSets
    * [x] HyperSlabs

### Importers:
  * Mesh:
    * [x] Structured
    * [x] Unstructured
  * Element:
    * Linear
      * [x] Triangle
      * [x] Quadrangle
      * [x] Tretrahedron
      * [x] Hexahedron
      * [x] Mixed
      * [ ] ...
  * Field:
    * Center on: 
        * [x] Node
        * [x] Cell
        * [x] Grid
        * [ ] Edge
        * [ ] Face
    * Type:
        * [x] Scalar
        * [x] Vector
        * [ ] Tensor
        * [ ] Matrix
        * [ ] Tensor6
        * [ ] GlobalID
        * [ ] NoType
  * XDMF tags parsing ([FoX dom](https://github.com/andreww/fox)  wrapper):
    * [x] Xdmf
    * [x] Domain
    * [x] Grid
    * [x] Topology
    * [x] Geometry
    * [x] Attribute
    * [x] DataItem
    * [x] Character data
  * HDF5:
    * [ ] Manage Files
    * [x] Groups
    * [x] DataSpaces
    * [x] DataSets
    * [x] HyperSlabs

## How to collaborate
If you want to actively collaborate in the project, please feel free to fork and submit pull requests.
Any help or suggestions would be greatly appreciated :)
