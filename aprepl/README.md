<!-- TITLE/ -->

## ApREPL

<!-- /TITLE -->

### Emacs exension for April interaction

This is an extension to Emacs implementing an April REPL. With additional Emacs extensions and April libraries, a wide range of interactions between the two systems may be possible.

The easiest way to use ApREPL is to add it to your Emacs init file. The simplest way to do this is to evaluate `(april::add-aprepl-load-to-init-file)`. This will add an expression loading the `aprepl.el` that will run whenever Emacs starts. It assumes that your Emacs init file is located at the path `~/.emacs`. If this is not the case, you can pass an argument to the function to choose a different path. For example, if your Emacs init file is located at `~/.emacs.d/init.el` you would enter `(april::add-aprepl-load-to-init-file "~/.emacs.d/init.el")`.