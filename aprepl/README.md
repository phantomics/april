<!-- TITLE/ -->

## ApREPL

<!-- /TITLE -->

### Emacs exension for April interaction

This is an extension to Emacs implementing an April REPL. With additional Emacs extensions and April libraries, a wide range of interactions between the two systems may be possible.

The easiest way to use ApREPL is to add it to your Emacs init file. The simplest way to do this is to evaluate `(april::add-aprepl-load-to-init-file)`. This will add an expression loading the `aprepl.el` that will run whenever Emacs starts. It assumes that your Emacs init file is located at the one of the paths `~/.emacs.el`, `~/.emacs` or `~/.emacs.d/init.el` (checked in that order).

If your Emacs init file is not located at one of the three above paths, you can pass an argument to the function to choose a different path. For example, if your Emacs init file is located at `~/.emacs-config-dir/init.el` you would enter `(april::add-aprepl-load-to-init-file "~/.emacs-config-dir/init.el")`. If no argument is given and no Emacs init file is found, the function will default to creating a new init file at the path `~/.emacs` containing the code that loads the ApREPL script.