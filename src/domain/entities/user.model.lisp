(in-package #:lisp-crud.domain.entities)

(defclass user ()
  ((id :initarg :id :reader user-id)
    (name :initarg :name :reader user-name)
    (email :initarg :email :reader user-email)))

(defun make-user (id name email)
  (make-instance 'user :id id :name name :email email))