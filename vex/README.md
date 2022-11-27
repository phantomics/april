<!-- TITLE/ -->

## Vex

<!-- /TITLE -->

### Vector language definition framework

This package contains tools for the implementation of vector language compilers within Common Lisp following the pattern of APL and its descendants. Its current application is implementing the April APL compiler, providing the basic functions and macros needed to construct the language. Vex makes it possible to write a centralized specification that defines all functions, operators and system utilities comprising a vector language. In the case of April this specification can be found in the `spec.lisp` file in the package's root directory.