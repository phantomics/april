<!-- TITLE/ -->

# Compatibility Notes

<!-- /TITLE -->

April has been tested with SBCL, CCL, ECL, ABCL, Clasp, Allegro CL and LispWorks. Its compatibility with these implementations is summarized below.

April's compatibility is tested by running the main test set, which is done by evaluating `(april (test))` and further by running the demo test sets, done by first loading the demo workspaces with `(load-demos)` and then running the tests with `(run-demo-tests)` within in the `april` package.

## SBCL and CCL

At this time, SBCL and CCL are 100% compatible with April, passing all of April's main tests and demo tests. No special configuration options are needed for full functionality.

## ECL

ECL passes all tests but special accomodations have been made for it to work.

ECL has two issues working with April: first, ECL's implementation of `(acos)` works differently than other implementations; compare the output `(acos 2)` in ECL and others. This affects the performance of the `[○ circular]` function and causes the failure of one of April's main tests.

Second, ECL has issues with `(rationalize)`. Evaluating `(rationalize 2.3)` will have a much different outcome in ECL compared to other implementations; it appears at least for this case that `(rationalize)` is simply passing its input to `(rational)`. This will cause issues when taking the greatest common divisor or lowest common multiple of floats, as with `1∨1.5`. It results in the failure of two tests from [the numeric function demo suite](./libraries/dfns/numeric).

For the time being, the problems with `(acos)` and `(rationalize)` are mitigated by specialized functions within April that override ECL's default implementations of these functions.

ECL occasionally hangs when running complex functions. If this happens, sending an interrupt (C-c in Emacs+Slime) and selecting the `CONTINUE` option usually causes it to resume running the function.

ECL benefits from a larger heap size than the default when using complex functions and/or large arrays. A heap size of 2GB was sufficient to run all of April's demo tests.

## ABCL

ABCL passes all tests but special accomodations have been made for it to work.

ABCL has the same problem with `(rationalize)` that ECL does; the function appears to be just a passthrough to `(rational)`.

Additionally, ABCL is incapable of compiling functions beyond a certain size; this is a limitation of the JVM that ABCL's developers have no control over. As a result, some particularly large APL functions will not compile. Notably, some of the large functions present in [the tree function demo suite](./libraries/dfns/tree) will not compiler under ABCL and this set of demos is this disabled under ABCL.

Like ECL, ABCL's problems with `(rationalize)` have been mitigated for the time being by a custom implementation of `(rationalize)` ported from SBCL into April for use with ABCL.

It's best to run ABCL with a larger heap size than the default if you expect to run large functions. A heap size of 5GB was used when running April's demo tests.

## Clasp

Clasp and April are nearing full compatibility; currently there are problems loading large APL files like those found in the `libraries/` subdirectory caused by Clasp's limit on the number of returned values from a function. The `random-state` library only gained compatibility with Clasp as of [this commit](https://github.com/Shinmera/random-state/commit/3e31e21ffde13555f73880e490e1f368d8cdbd58), so random number generation will only work with that or more recent commits of `random-state`. All of April's main tests pass in Clasp as long as it and `random-state` are up to date, but the  Stay tuned for more updates.

## Allegro CL

Allegro CL fails 2 main tests because of a problem with its `(acosh)` function. As a result, for example `¯6 ○ ¯2` produces a divide-by-zero error.

## LispWorks

LispWorks has a problem with double-precision floating point math that causes one of the main tests to fail. Compare `(expt 10.0d0 10)` in LispWorks and other CLs. This problem also appears to cause the failure of three tests from [the numeric function demo suite](./libraries/dfns/numeric).

LispWorks also tends to hang when running complex APL functions. This can be seen when running the demo tests. Sending an interrupt and selecting the "CONTINUE" option often allows it to continue. LispWorks seems to hang much more often than ECL does and it can be difficult for it to finish the demo test suites.