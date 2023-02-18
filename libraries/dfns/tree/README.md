<!-- TITLE/ -->

## Dyalog Tree Processing Dfns

<!-- /TITLE -->

These are Dyalog's dfns used for processing tree structures. Their original source is [Dyalog's tree processing dfns page](http://dfns.dyalog.com/n_Trees.htm). They are included in April's demo test suite, so you can run tests of these functions by first evaluating `(april:load-libs)` to load the functions and then `(april:run-tests)` to run the tests.

Note that some of these functions cannot be compiled under ABCL; the JVM imposes a limit on the size of compiled methods that ABCL cannot mitigate, and some of the large functions in this library exceed that limit.