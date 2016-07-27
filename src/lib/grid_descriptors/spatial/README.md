# XH5For
The Fortran way to easy write parallel partitioned XDMF/HDF5 meshes

[![Build Status](https://travis-ci.org/victorsndvg/XH5For.svg?branch=master)](https://travis-ci.org/victorsndvg/XH5For)
[![codecov](https://codecov.io/gh/victorsndvg/XH5For/branch/master/graph/badge.svg)](https://codecov.io/gh/victorsndvg/XH5For)

## License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)

## XH5For: Spatial Grid Descriptor

*Spatial Grid Descriptor* software subsystem is the one in charge of storing metadata related with the global view of the grid. 

*Spatial Grid Descriptor* performs communications (all_gather and broadcast only) through the *MPI Environment* object in order to build the view of the global grid in all involved MPI tasks.

*Spatial Grid Descriptor* is used by the *XDMF Handler* while parsing and by *HDF5 Handler* to calculate attributes dimensions, offsets, etc.

## XH5For: Spatial Grid Descriptor UML class diagram:
Automatically generated with [ForUML](http://research.te.psu.ac.th/aziz/foruml.htm).

![XH5For Spatial Grid Descriptor UML class diagram](https://github.com/victorsndvg/XH5For/tree/master/media/XH5For_spatial_grid_descriptor_UML.svg "XH5For Spatial Grid Descriptor UML class diagram")


