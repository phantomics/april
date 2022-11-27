<!-- TITLE/ -->

## Varray

<!-- /TITLE -->

### Virtual array processing classes

This package contains classes for creating and computing the contents of virtual arrays. These are arrays defined by sets of parameters and relationships to other virtual arrays and/or non-virtual arrays that are written explicitly into memory. These classes facilitate the lazy evaluation of array processing instructions; instead of writing computed array elements into memory (accumulating data), a tree of virtual array objects may be created representing the potential computation of an array at a later time (accumulating operations). The `(render)` method in this library is used to reify a virtual array, writing it into memory.