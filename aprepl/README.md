<!-- TITLE/ -->

## ApREPL

<!-- /TITLE -->

### Emacs exension for April interaction

This is an extension to Emacs implementing an April REPL. With additional Emacs extensions and April libraries, a wide range of interactions between the two systems may be possible.

The easiest way to use ApREPL is to load it whenever you start Emacs, and the simplest way to do that is to evaluate `(april::add-aprepl-load-to-emacs-init-file)`. This will add an expression to your Emacs init file loading the `aprepl.el` file that will run whenever Emacs starts. It assumes that the init file is located at the one of the paths `~/.emacs.el`, `~/.emacs` or `~/.emacs.d/init.el` (checked in that order).

If your Emacs init file is not located at one of the three above paths, you can pass an argument to the function to choose a different path. For example, if your Emacs init file is located at `~/.emacs-config-dir/init.el` you would enter `(april::add-aprepl-load-to-emacs-init-file "~/.emacs-config-dir/init.el")`. If no argument is given and no Emacs init file is found, the function will default to creating a new init file at the path `~/.emacs` containing the code that loads the ApREPL script.

Once the extension has been added to your init file, you can start ApREPL in Emacs by typing `M-x aprepl`.

### REPL commands

As well as evaluating APL expressions, you can interact with ApREPL using its REPL commands. Each of these commands starts with a `)` character as per APL convention. These commands are different from APL functions in that they are handled directly by the REPL, not the underlying April compiler. Many of these commands cause interactions with the compiler, but they are all handled by ApREPL within Emacs before anything happens involving April.

All commands are case-insensitive, so `)OFF` does the same thing as `)off`.

|Command|Name                |Description|
|----------|--------------------|-----------|
|`)OFF`    |End Session         |Shut down ApREPL and close its buffer.|
|`)MAKEWS` |Create workspace    |Create a workspace; for example `)makews newspace` creates a workspace called `newspace`. The workspace name is case-insensitive.|
|`)SETWS`  |Set workspace       |Sets the workspace in which ApREPL will evaluate expressions.|