(in-package #:lisp-crud.adapters.postgres)

;; Conversion functions
(defun db-record->entity (db-record)
  "Convert database record to domain entity"
  (when db-record
    (make-instance 'lisp-crud.domain.entities:user
                    :id    (getf db-record :id)
                    :name  (getf db-record :name)
                    :email (getf db-record :email))))

(defun entity->db-record (user)
  (list :id    (format nil "~A" (lisp-crud.domain.entities:user-id user))
        :name  (lisp-crud.domain.entities:user-name user)
        :email (lisp-crud.domain.entities:user-email user)))

(defun entity->insert-params (user)
  (list :id    (format nil "~A" (lisp-crud.domain.entities:user-id user))
        :name  (lisp-crud.domain.entities:user-name user)
        :email (lisp-crud.domain.entities:user-email user)))
