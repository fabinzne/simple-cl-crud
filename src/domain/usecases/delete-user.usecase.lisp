(in-package #:lisp-crud.domain.usecases)

(defun delete-user-usecase (repository id)
  (log:info "Deleting user with id: ~A" id)
  (lisp-crud.domain.ports:delete-user repository id))