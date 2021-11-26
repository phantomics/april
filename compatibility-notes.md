<!-- TITLE/ -->

# Compatibility Notes

<!-- /TITLE -->

April has been tested with SBCL, CCL, ECL, ABCL and LispWorks. Its compatibility with these implementations is summarized thusly.

## SBCL and CCL

At this time, SBCL and CCL are 100% compatible with April, and all array operations tested with these implementations work. No special configuration options are needed for full functionality.

## ECL

ECL has two issues working with April: first, ECL's implementation of `(acos)` works differently than other implementations; compare the output `(acos 2)` in ECL and others. This affects the performance of the `[○ circular]` function and causes the failure of one of April's main tests.

Second, ECL has issues with `(rational)` and `(rationalize)`. Evaluating `(rationalize 2.3)` will have a much different outcome in ECL compared to other implementations. This will cause issues when taking the greatest common divisor or lowest common multiple of floats, as with `1∨1.5`. It results in the failure of two tests from [the numeric function demo suite](./demo/dfns/numeric).

ECL occasionally hangs when running complex functions. If this happens, sending an interrupt (C-c in Emacs+Slime) and selecting the `CONTINUE` option usually causes it to resume running the function.

ECL benefits from a larger heap size than the default when using complex functions and/or large arrays. A heap size of 2GB was sufficient to run all of April's demo tests.

## ABCL

ABCL has the same problem with `(rational)` and `(rationalize)` that ECL does. They appear to use the same algorithm for these functions.

Additionally, ABCL is incapable of compiling functions beyond a certain size; this is a limitation of the JVM that ABCL's developers have no control over. As a result, some particularly large APL functions will not compile. Notably, some of the large functions present in [the tree function demo suite](./demo/dfns/tree) will not compiler under ABCL and this set of demos is this disabled under ABCL.

It's best to run ABCL with a larger heap size than the default if you expect to run large functions. A heap size of 5GB was used when running April's demo tests.

## LispWorks

There is a problem either with LispWorks or the parse-number library that causes some irregularities when parsing numbers using big-E notation. This causes one of the main tests to fail. Try `(parse-number:parse-number "1e10" :float-format 'double-float)` and compare the result in LispWorks with other CL implementations. 

LispWorks also tends to hang when running complex APL functions. This can be seen when running the demo tests. Sending an interrupt and selecting the "CONTINUE" option often allows it to continue. LispWorks seems to hang much more often than ECL does and it can be difficult for it to finish the demo test suites.