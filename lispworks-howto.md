<!-- TITLE/ -->

# Compatibility Notes

<!-- /TITLE -->

## SBCL and CCL

At this time, SBCL and CCL are 100% compatible with April, all array operations tested with these implementations work.

## ECL

ECL has two issues working with April: first, ECL's implementation of `(acos)` works differently than other implementations; compare the output `(acos 2)` in ECL and others. This affects the performance of the `[○ circular]` function and causes the failure of one of April's main tests.

Second, ECL has issues with `(rational)` and `(rationalize)`. Evaluating `(rationalize 2.3)` will have a much different outcome in ECL compared to other implementations. This will cause issues when taking the greatest common divisor or lowest common multiple of floats, as with `1∨1.5`. It results in the failure of two tests from [the numeric function demo suite](./demo/dfns/numeric).

## ABCL

ABCL has the same problem with `(rational)` and `(rationalize)` that ECL does.

Additionally, ABCL is incapable of compiling functions beyond a certain size; this is a limitation of the JVM that ABCL's developers have no control over. As a result, some particularly large APL functions will not compile. Notably, some of the large functions present in [the tree function demo suite](./demo/dfns/tree) will not compiler under ABCL and this set of demos is this disabled under ABCL.

## LispWorks




## Customizing Your Configuration

On many systems, LispWorks's default settings do not allow the use of the UTF-8 characters needed for APL. You can fix this by adding these lines to your `~/.lispworks` file.

```lisp
(pushnew :utf-8 system:*specific-valid-file-encodings*)
(lw:set-default-character-element-type 'cl:character)
```

If you don't have a `~/.lispworks` file, create it in your home directory and enter the above lines.

Thanks to Rainer Joswig for this tip.