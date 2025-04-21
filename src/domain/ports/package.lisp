(defpackage #:lisp-crud.domain.ports
  (:use #:cl)
  (:export #:user-repository
           #:save-user
           #:delete-user
           #:update-user
           #:find-by-id
           #:find-by-email
           #:find-all-users))