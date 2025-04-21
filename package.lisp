;; package.lisp
(defpackage #:lisp-crud
  (:use #:cl #:woo) ; Add packages to :use
  (:export #:start-server)
  (:export #:stop-server)) ; Export main function if needed

