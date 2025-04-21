(in-package #:lisp-crud.domain.usecases)

(defun create-user (repository name email)
  (log:info "Creating user -> name: ~A, email: ~A" name email)
  (let* ((id (uuid:make-v4-uuid))
          (user (make-user id name email)))
    ;; Call save-user on the repository instance
    (lisp-crud.domain.ports:save-user repository user)
    user))