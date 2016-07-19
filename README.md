# XH5For
The Fortran way to easy write parallel partitioned XDMF/HDF5 meshes

[![Build Status](https://travis-ci.org/victorsndvg/XH5For.svg?branch=master)](https://travis-ci.org/victorsndvg/XH5For)
[![codecov.io](https://codecov.io/github/victorsndvg/XH5For/coverage.svg?branch=master)](https://codecov.io/github/victorsndvg/XH5For?branch=master)

##License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)                 

## What is XH5For?
Is a library to read and write parallel partitioned FEM meshes taking advantage of the Collective/Independent IO provided by the HDF5 library. 
**XH5For** is not a general-purpose XDMF library, it only reads XDMF files written by itself.

### Some concepts
* [XDMF Model and Format](http://www.xdmf.org/index.php/XDMF_Model_and_Format) is a standarized way to describe common scientific data. It separates the **Light data** *(XML description of the data model)* and the **Heavy data** *(raw values)* usually saved in HDF5 or binary Files.
* [HDF5](https://www.hdfgroup.org/HDF5) is a model and data format designed for efficient Collective/Independent I/O and work with big amounts of data.

##How to get XH5For

```git clone --recursive https://github.com/victorsndvg/XH5For.git ```

## Compilation

**XH5For** compile with GNU Fortran compiler 5.1 (and newer versions) and Intel Fortran compiler 15.0.1 (and newer versions).

**XH5For** uses [CMake](https://cmake.org/) as a portable compilation system. 

The easiest way to compile **XH5For** under Linux is:

```
$ cd XH5For
$ mkdir build
$ cd build
$ cmake [cmake-settings] ../
$ make
```

*To compile XH5For under Windows use de equivalent commands*

### XH5For CMake basic settings

CMake compilation system settings allows us to manage compilation system behaviour. 
Look at the [CMake command line-interface](https://cmake.org/cmake/help/v3.2/manual/cmake.1.html) to know how to pass CMake settings.
There are several ways to [configure XH5For compilation system with CMake](https://cmake.org/runningcmake/)

The most important XH5For settings are:

  * ```XH5For_ENABLE_HDF5```: *ON* if HDF5 library is automagically found in your system (default *OFF*)
  * ```XH5For_ENABLE_MPI```: *ON* if MPI library is automagically found in your system (default *OFF*)
  * ```XH5For_ENABLE_TESTS```: (default *OFF*)
  * ```XH5For_ENABLE_EXAMPLES```: (default *OFF*)

##Getting started with XH5For

The code below writes an hexahedron per MPI task:

```fortran
program xh5for_hexa_per_task

    use xh5for
    use mpi

implicit none

    type(xh5for_t)        :: xh5
    real, dimension(24)   :: geometry = (/0.0, 0.0, 0.0, &
                                          0.0, 0.0, 1.0, &
                                          0.0, 1.0, 1.0, &
                                          0.0, 1.0, 0.0, &
                                          1.0, 0.0, 0.0, &
                                          1.0, 0.0, 1.0, &
                                          1.0, 1.0, 1.0, &
                                          1.0, 1.0, 0.0/)
    integer, dimension(8) :: topology = (/0, 1, 2, 3, 4, 5, 6, 7/)
    integer, dimension(8) :: temperature = (/0, 1, 2, 3, 4, 5, 6, 7/)
    integer               :: mpierror

    call MPI_INIT(mpierror)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr)
    geometry = geometry+rank

    call xh5%Open(FilePrefix='xh5for_unstructured_hexahedron', Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB, Action=XDMF_ACTION_WRITE)
    call xh5%SetGrid(NumberOfNodes=8, NumberOfElements=1,TopologyType=XDMF_TOPOLOGY_TYPE_HEXAHEDRON, GeometryType=XDMF_GEOMETRY_TYPE_XYZ)
    call xh5%WriteTopology(Connectivities=topology)
    call xh5%WriteGeometry(XYZ=geometry)
    call xh5%WriteAttribute(Name='Temperature', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=temperature)
    call xh5%Close()
    call xh5%Free()

    call MPI_FINLIZE(mpierror)

end program xh5for_hexa_per_task
```

## Some considerations

  * **XH5For** use [XDMF Model and Format](http://www.xdmf.org/index.php/XDMF_Model_and_Format) to describe the **Light data** and [HDF5](https://www.hdfgroup.org/HDF5) for the **Heavy data**.
  * HDF5 API must be linked to read/write the **Heavy data**.
  * Paralell HDF5 API must to be linked to take advange of the Collective IO.
  * Paralell HDF5 API only works if an MPI library is also linked.
  * XML syntax is be based on XDMF v2.x.
  * The handling of XML files is developed on top of [FoX](https://github.com/andreww/fox) (Fortran XML library) for reading/writing XDMF files. Particularly, *FoX_wxml* library is used for writing XDMF files and *FoX_dom* will be used for parsing.

## Zen of XH5For:

  * Use *HDF5* high level library based on *MPI_IO*
  * Open in the right mode
  * Number of XDMF/HDF5 files must be independent of the number of processes
  * Reduce simultaneous open files to the minimun
  * Prefer collective IO
  * Do not open/close files too frequently
  * Avoid small accesses
  * Try *MPI_IO* hints
  * Write/Read as infrequently as possible
  * Reduce precision if possible

[Best practices for parallel IO and MPI-IO hints](http://www.idris.fr/media/docs/docu/idris/idris_patc_hints_proj.pdf)

## Development roadmap:

  * XDMF:
      * Series:
        * [x] Static grid
        * [x] Non-Static grid
      * Grid:
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
  * HDF5:
    * [x] Groups
    * [x] DataSets
    * [x] HyperSlabs
    * Strategies:
        * [x] Contiguous hyperslab
        * [x] One dataset per process
        * [ ] Chunking


## How to collaborate

If you want to actively collaborate in the project, please feel free to fork and submit pull requests.
Any help or suggestions would be greatly appreciated :)
