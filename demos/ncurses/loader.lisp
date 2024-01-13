;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Ncurses -*-
;;;; loader.lisp

(asdf:load-system 'april-demo.ncurses)

(april-demo.ncurses::main)

(cl-user::quit)
