(in-package #:lisp-crud.domain.ports)

(defclass user-repository () ())

(defgeneric save-user (repository user))
(defgeneric delete-user (repository id))
(defgeneric update-user (repository user))
(defgeneric find-by-id (repository id))
(defgeneric find-by-email (repository email))
(defgeneric find-all-users (repository))