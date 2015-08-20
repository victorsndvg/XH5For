# XdmfHdf5Fortran
The Fortran way to easy write partitioned XDMF/HDF5 meshes with parallel IO

## What is XdmfHdf5Fortran?
XdmfHdf5Fortran is a library to read and write partitioned FEM meshes taking advantage of the collective/independent IO provided by the HDF5 library. 

## Some concepts
* [XDMF Model and Format](http://www.xdmf.org/index.php/XDMF_Model_and_Format) is a standarized way to describe common scientific data. It separates the **Light data** *(XML description of the data model)* and the **Heavy data** *(raw values)* usually saved in HDF5 or binary Files.
* [HDF5](https://www.hdfgroup.org/HDF5) is a model and data format designed for efficient I/O and work with big amounts of data.

## Some considerations
* XdmfHdf5Fortran will use [XDMF Model and Format](http://www.xdmf.org/index.php/XDMF_Model_and_Format) to describe the **Light data** and [HDF5](https://www.hdfgroup.org/HDF5) for the **Heavy data**.
* HDF5 API need to be linked to write the **Heavy data**.
* The first approach to the HDF5 IO will be based on HyperSlabs in order to reduce metadata.
* As starting point XML syntax will be based on XDMF v2.x.
* As starting point the library will use an basic XML SAX parser for reading/writing XDMF files based on the previous work of [Francisco Pena](http://sourceforge.net/u/franpena/profile).

## Main features

### Exporters:
  * Mesh:
    * [ ] Structured
    * [ ] Unstructured
  * Element:
    * Linear
      * [ ] Triangle
      * [ ] Quadrangle
      * [ ] Tretrahedron
      * [ ] Hexahedron
      * [ ] Mixed
  * Field:
    * Center on: not defined yet...
    * Number of components: not defined yet...
  * XDMF:
    * [ ] Temporal grid
    * [ ] Spatial grid
    * [ ] Grid
      * [ ] Topology
      * [ ] Geometry
      * [ ] Attribute
    * [ ] DataItem
      * [ ] HyperSlab
  * HDF5:
    * [ ] Manage Files
    * [ ] Groups
    * [ ] DataSpaces
    * [ ] DataSets
    * [ ] HyperSlabs

### Importers
  * (Future work...)

## How to collaborate
If you want to actively collaborate in the project, please feel free to fork and submit pull requests.
Any help or suggestions would be greatly appreciated :)
