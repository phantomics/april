;;;; package.lisp

(defpackage #:april-xt.uzuki
  (:use #:cl #:april #:april.idiom-extension-tools)
  (:shadowing-import-from :april #:this-idiom #:⍺ #:⍶ #:⍺⍺ #:⍵ #:⍹ #:⍵⍵
                          #:*value-composable-lexical-operators*))
