;;;; install-seed.lisp
;;; A discrete program to install Seed. If dependencies aren't met, it fails and gives instructions to remedy.

(asdf:load-system 'uiop)
(defpackage #:seed.ui-model.html
  (:export #:qualify-build)
  (:use #:cl))
(load "./seed.ui-model.html/qualify-build.lisp")
(use-package 'seed.ui-model.html)

(if (not (probe-file #P"~/quicklisp"))
    (princ (format nil "~%~%Seed installation failed.~%~%I couldn't find a /quicklisp directory in your home directory; please install Quicklisp. See https://www.quicklisp.org/beta/ for instructions on setting up Quicklisp.~%"))
    (let ((success-message (format nil "~%~%Congratulations, Seed is installed and ready to use. ~%~%If you would like to automatically load Seed whenever you load your Common Lisp REPL, please add the following line to your Common Lisp implementation's init file:~%~%(asdf:load-system 'seed)~%~%For example, for SBCL this file is usually located at ~~/.sbclrc.~%~%If you would like Seed to automatically open its Web interface whenever you load your REPL, you should add both of these lines to your init file:~%~%(asdf:load-system 'seed)~%(seed:contact-open)~%~%If you wish to close the Web connection, you can do so by entering (seed:contact-close), and reopen it by entering (seed:contact-open) again.~%~%")))
      (qualify-build
	((if (not (probe-file #P"~/quicklisp/local-projects/seed"))
	     ;; (multiple-value-bind (1st 2nd error-code)
	     ;; 	 (uiop:run-program (format nil "ln -s \"~a\" ~~/quicklisp/local-projects/seed"
	     ;; 				   (namestring (probe-file "./")))
	     ;; 			   :ignore-error-status t)
	     ;;   (declare (ignore 1st 2nd))
	     ;;   (if (not (= 0 error-code))
	     ;; 	   (princ (format nil "~%~%Seed installation failed.~%~%I couldn't create a link to this directory in the ~~/quicklisp/local-projects directory. This is needed in order to fetch Seed's dependencies using Quicklisp. Please check the permissions of your ~~/quicklisp and ~~/quicklisp/local-projects folders.~%~%"))
	     ;; 	   (progn (ql:quickload 'seed)
	     ;; 		  (princ success-message))))
	     
	     (princ (format nil "~%~%Seed installation failed.~%~%I couldn't find Seed's directory in the ~~/quicklisp/local-projects directory. This is needed in order to fetch Seed's dependencies using Quicklisp. Please either copy the Seed directory into ~~/quicklisp/local-projects or create a symlink called ~~/quicklisp/local-projects/seed from that directory to the directory where Seed is located.~%~%"))
	     (progn (ql:quickload 'seed)
		    (princ success-message))))
	("Seed installation failed."))))

(exit)
