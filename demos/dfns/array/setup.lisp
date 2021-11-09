;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Dfns.Array -*-
;;;; setup.lisp

(in-package #:april-demo.dfns.array)

(april-create-workspace array-demo-space)

(april (with (:space array-demo-space) ;; sample text for the wrapnote function tests
             (:store-val (note-sample "
Paragraphs  of  text  in  notes  such as these, may be wrapped to be more easily
readable  in an edit window of a particular size. Such wrapping might be advant-
ageous for example, on a \"phablet\", where edit windows are much narrower.

\"Flowing\" text paragraphs are identified as having lines that are left and right
justified.  That is, they extend from the _first_ to the _last_ column, possibly
with  some  extra  padding between words. The value of _last_ is taken to be the
width of the first non-blank line following the title sequence.

To avoid mutilating pictures,  rows containing box-drawing characters are exempt
from wrapping:

┌──────────────────────────────────────────────────────────────────────────────┐
│ This picture will not be wrapped, as each row contains at least one of the   │
│ special box-drawing characters. ┌────────────────────────────────────────────┘
└─────────────────────────────────┘

Notice  in  the example below, that [wrapnote] recombines words split by hyphen-
ation.
")
                         (note-sample-output "
Paragraphs  of  text  in notes
such  as these, may be wrapped
to  be more easily readable in
an edit window of a particular
size.  Such  wrapping might be
advantageous for example, on a
\"phablet\",  where edit windows
are much narrower.

\"Flowing\"  text paragraphs are
identified   as  having  lines
that   are   left   and  right
justified.   That   is,   they
extend from the _first_ to the
_last_  column,  possibly with
some   extra  padding  between
words.  The value of _last_ is
taken  to  be the width of the
first non-blank line following
the title sequence.

To  avoid mutilating pictures,
rows   containing  box-drawing
characters   are  exempt  from
wrapping:

┌──────────────────────────────────────────────────────────────────────────────┐
│ This picture will not be wrapped, as each row contains at least one of the   │
│ special box-drawing characters. ┌────────────────────────────────────────────┘
└─────────────────────────────────┘

Notice  in  the example below,
that   [wrapnote]   recombines
words split by hyphenation.
"))) "")
