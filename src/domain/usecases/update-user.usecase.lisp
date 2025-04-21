(in-package #:lisp-crud.domain.usecases)

(defun update-user-usecase (repository id name email)
  (log:info "Updating user -> id: ~A, name: ~A, email: ~A" id name email)
  (let* ((user (make-user id name email)))
    (lisp-crud.domain.ports:update-user repository user)
    user))