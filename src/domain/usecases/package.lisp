(defpackage #:lisp-crud.domain.usecases
  (:use #:cl)
  (:import-from #:lisp-crud.domain.entities
                #:user
                #:make-user)
  (:import-from #:lisp-crud.domain.ports
                #:user-repository
                #:save-user
                #:delete-user
                #:update-user)
  (:export #:create-user
           #:delete-user-usecase
           #:update-user-usecase))