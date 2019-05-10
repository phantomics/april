<!-- TITLE/ -->

# Using April with LispWorks

<!-- /TITLE -->

## Customizing Your Configuration

On many systems, LispWorks's default settings do not allow the use of the UTF-8 characters needed for APL. You can fix this by adding these lines to your `~/.lispworks` file.

```lisp
(pushnew :utf-8 system:*specific-valid-file-encodings*)
(lw:set-default-character-element-type 'cl:character)
```

If you don't have a `~/.lispworks` file, create it in your home directory and enter the above lines.

Thanks to Rainer Joswig for this tip.