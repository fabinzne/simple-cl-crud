(in-package #:lisp-crud.domain.usecases)

(defun delete-user (repository user)
  (lisp-crud.domain.ports:delete-user repository user))