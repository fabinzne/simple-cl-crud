(in-package #:lisp-crud.di)

(defun make-domain-container ()
  (let ((repo (make-instance 'lisp-crud.adapters.postgres:postgres-user-repository)))
    (list
      :create-user (lambda (name email)  ; Already has repo injected!
                    (lisp-crud.domain.usecases:create-user repo name email))
      :delete-user (lambda (user)
                    (lisp-crud.domain.usecases:delete-user repo user)))))
