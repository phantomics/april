;;;; april-demo.ncurses.asd

(asdf:defsystem #:april-demo.ncurses
  :description "Terminal Dogma: A terminal interaction system leveraging April APL"
  :author "Andrew Sengul"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april" "lparallel" "croatoan")
  :components ((:file "package")
               (:file "setup")
               (:file "ncurses")))
