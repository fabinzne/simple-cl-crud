(in-package #:lisp-crud.adapters.postgres)

(defclass postgres-user-repository (lisp-crud.domain.ports:user-repository)
  ())

(defmethod save-user ((repo postgres-user-repository) user)
  (log:info "Saving user: ~A ~A ~A"
            (lisp-crud.domain.entities:user-id user)
            (lisp-crud.domain.entities:user-name user)
            (lisp-crud.domain.entities:user-email user))
  (let ((params (entity->insert-params user)))
    (log:info "Params to insert ~A ~A ~A"
              (getf params :id) (getf params :name) (getf params :email))
    (postmodern:query
      (:insert-into 'users :set 'id (getf params :id)
                            'name (getf params :name)
                            'email (getf params :email)))
  user))

(defmethod delete-user ((repo postgres-user-repository) id)
  (postmodern:query (:delete-from 'users :where (:= 'id id))))

(defmethod update-user ((repo postgres-user-repository) user)
  (log:info "Updating user: ~A ~A ~A"
            (lisp-crud.domain.entities:user-id user)
            (lisp-crud.domain.entities:user-name user)
            (lisp-crud.domain.entities:user-email user))
  (let ((params (entity->db-record user)))
    (log:info "Params to UPDATE ~A ~A ~A"
              (getf params :id) (getf params :name) (getf params :email))
    (postmodern:query 
      (:update 'users :set 'name (getf params :name)
                            'email (getf params :email)
                      :where (:= 'id (getf params :id))))
  user))

(defmethod find-by-id ((repo postgres-user-repository) id)
  (postmodern:with-connection (conn)
    (let ((result (postmodern:query conn "SELECT id, name, email FROM entities WHERE id = $1"
                                  id)))
      (if result
          (destructuring-bind (id name email) (first result)
            (make-user id name email))
          nil))))

(defmethod find-all-users ((repo postgres-user-repository))
  (postmodern:with-connection (conn)
    (let ((results (postmodern:query conn "SELECT id, name, email FROM entities")))
      (mapcar #'(lambda (row)
                  (destructuring-bind (id name email) row
                    (make-user id name email)))
              results))))