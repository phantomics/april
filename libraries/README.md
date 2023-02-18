<!-- TITLE/ -->

## April Libraries

<!-- /TITLE -->

This directory contains software packages implementing standard libraries for APL. For now, most of them have been ported from Dyalog's MIT-licensed collection of dfns.

### Using libraries in an application

You can include functions and operators from these libraries in an application using April with April's `⎕XWF` and `⎕XWO` syntax forms, used to load external functions and operators respectively.

The first step is typically to create an ASDF system depending on the library/libraries you wish to use, like so:

```lisp
(asdf:defsystem #:my-system
  :description "A system using April and its array processing function library."
  :author "Me"
  :license "..."
  :serial t
  :depends-on ("april" "april-lib.dfns.array")
  :components (...files listed here...)
```

This is an example of a system using the array processing library, which contains some of the most broadly useful functions. Let's say that you want to use the `display` and `disp` functions from that library so you can print neatly formatted representations of arrays using box-drawing characters. The next step is to invoke the following code:

```apl
display disp ← 'ARRAY-LIB-SPACE' ⎕XWF 'display' 'disp'
```

This code can either be placed in an `.apl` file or in an `(april)` macro like so:

```lisp
(april (with (:space my-workspace)) "display disp ← 'ARRAY-LIB-SPACE' ⎕XWF 'display' 'disp'")
```

What this code does is create a pair of functions, `display` and `disp` in your workspace aliasing the functions `display` and `disp` in the workspace called `array-lib-space` that holds the array processing library functions. With this done you can use the libraries in April code:

```lisp
* (april (with (:space my-workspace)) "⎕←disp ⍳4 5 ⋄ 'Drawing complete!'")
┌───┬───┬───┬───┬───┐
│1 1│1 2│1 3│1 4│1 5│
├───┼───┼───┼───┼───┤
│2 1│2 2│2 3│2 4│2 5│
├───┼───┼───┼───┼───┤
│3 1│3 2│3 3│3 4│3 5│
├───┼───┼───┼───┼───┤
│4 1│4 2│4 3│4 4│4 5│
└───┴───┴───┴───┴───┘
"Drawing complete!"
```

And that's all there is to it. These examples assume that you've created a workspace called `my-workspace` to use within your application.