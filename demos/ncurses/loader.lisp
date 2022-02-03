;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Ncurses -*-
;; loader.lisp

;; load croatoan and swank
(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload 'swank))

(asdf:load-system 'april-demo.ncurses)

(in-package #:april-demo.ncurses)

;; Main entry point, to be called from the terminal thread, it will
;; initialize the screen and enter the event loop
(defun main ()
  (croatoan:with-screen (screen
                         ;; Set input blocking to 100 ms. This _must_
                         ;; be set for swank to work, otherwise
                         ;; get-event will block and croatoan only
                         ;; polls the job queue when a key is pressed.
                         :input-blocking 100
                         ;; Do not override the swank debugger hook,
                         ;; as we want to enter the slime debugger in
                         ;; emacs when a error occurs.
                         :bind-debugger-hook nil
                         ;; hide the cursor
                         :cursor-visible nil)
    (initialize-screen screen)))

;; Initialize swank, setting dont-close will prevent the server from
;; shutting down in case slime disconnects. The default port is 4005,
;; one can specify a different one with :port.
(swank:create-server :dont-close t)

;; Initialize screen and enter the event loop
(main)
