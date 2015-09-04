    Actually this layer allows to manage several types of element tags
    and its attributes.
    
    This layer is built on top of Fox_wxml who manages the creation
    of well-formed XML files. Validation in uncomplete.
    
    This interface validate key-value pairs with defined options
    (@Note that XDMF is a case sensitive format), but not the
    coherence across different option. Final user/developer
    must be who is in charge of this coherence.
    
    Some of the tags actually supported are:
      * [x] Xdmf
      * [x] Domain
      * [x] Grid
      * [x] Topology
      * [x] Geometry
      * [x] DataItem
    
    The hierarchy structure of the code can be modified in the
    future due to design decision and/or integration problems
