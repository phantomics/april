;;;; april-ext.jkanji.asd

(asdf:defsystem #:april-ext.jkanji
  :description "An extension to April aliasing the lexicon with Japanese kanji."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :version "1.0"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
               (:file "jkanji")))
