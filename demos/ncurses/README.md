<!-- TITLE/ -->

## Ncurses Application

<!-- /TITLE -->

This is an ncurses demo using April. It gives a new perspective on the classic Conway's Game of Life function, leveraging the `[⌺ stencil]` operator, UTF-8 box drawing glyphs and terminal color codes to manifest the cell matrix in an interesting way.

### Installation

To install the demo, evaluate `(ql:quickload 'april-demo.ncurses)`. If this doesn't work, make sure that the top-level `/april` directory is present or linked within your `~/quicklisp/local-projects` directory.

### Running the demo

For this demo to work, it must be run from the command line rather than through Emacs. The Common Lisp implementation you use must have QuickLisp loaded in its init file, which is provided for automatically when you install QuickLisp via the `quicklisp.lisp` file from [the QuickLisp site](https://www.quicklisp.org) and evaluate `(quicklisp::add-to-init-file)` after installation is complete. Your terminal emulator must support 256-color foregrounds and backgrounds for text - this means that xterm has to be explicitely told to support 256 colors, because by default it supports 8. This can be done by prepending "TERM=xterm-256color" to the sbcl command.

Because this demo depends on external libraries, you'll need to install those via Quicklisp. The simplest way to do this is to evaluate `(april::install-demos)`.

Provided that the above criteria are satisfied, all you need to do is load the `loader.lisp` file in this directory from the command line. Here's how it's done with SBCL:

```
sbcl --load loader.lisp
```

The program will present a cellular playfield with boxes drawn around the cells and background shading indicating the current and past presence of cells. You can control and quit the program using simple key commands described at the bottom of the screen.