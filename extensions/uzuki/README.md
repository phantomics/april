<!-- TITLE/ -->

## Uzuki
### An April Japanese Kanji Extension

<!-- /TITLE -->

This extension to April provides Japanese aliases of its characters. It assigns kanji aliases for all function and operator characters, as well as allowing for the use of double-width enclosing characters like `（）` and `｛｝` in place of their single-width counterparts.

Uzuki (the old Japanese name for the month of April) is mostly just a curiosity at this time, but its design may point the way to an alternative lexical system for array programming in ideographic languages. Japanese users already have means to enter kanji on a computer, and thus they may enjoy a lower barrier to entry to the use of April through the use of this extension.

### Using Uzuki

If you'd like to try it, all you need to do is evaluate:

```lisp
* (asdf:load-system 'april-xt.uzuki)
```

Then the Japanese character aliases will be activated and usable:

```lisp
* (april-f "３　４形指９")
1 2 3 4
5 6 7 8
9 1 2 3
#2A((1 2 3 4) (5 6 7 8) (9 1 2 3))
```