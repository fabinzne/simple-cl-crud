(defpackage #:lisp-crud.di
  (:use #:cl 
        #:lisp-crud.domain.usecases 
        #:lisp-crud.adapters.postgres)
  (:export #:make-domain-container))