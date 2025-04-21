(in-package #:lisp-crud.di)

(defun make-domain-container ()
  (let ((repo (make-instance 'lisp-crud.adapters.postgres:postgres-user-repository)))
    (list
      :create-user (lambda (name email)  ; Already has repo injected!
                    (lisp-crud.domain.usecases:create-user repo name email))
      :delete-user (lambda (id)
                    (lisp-crud.domain.usecases:delete-user-usecase repo id))
      :update-user (lambda (id name email)  
                    (log:info "Received params -> ~A ~A ~A" id name email)
                    (lisp-crud.domain.usecases:update-user-usecase repo id name email)))))
