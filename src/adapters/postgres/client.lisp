(in-package #:lisp-crud.adapters.postgres)

;; Remove all previous connection variables and macros
;; Keep only configuration variables
(defvar *db-name* nil)
(defvar *db-user* nil)
(defvar *db-pass* nil)
(defvar *db-host* nil)
(defvar *db-port* 5432)

(defun configure (name user pass host &key (port 5432))
  (setf *db-name* name
        *db-user* user
        *db-pass* pass
        *db-host* host
        *db-port* port))

(defun connect ()
  "Establish a global top-level connection"
  (handler-case
    (progn
      (postmodern:connect-toplevel *db-name* *db-user* *db-pass* *db-host* 
                               :port *db-port*)
      (log:info "Top-level connection established: ~A" postmodern:*database*))
    (postmodern:database-connection-error (e)
      (log:info "Failed to connect to data base: ~A" e))
    (error (e)
      (log:info "Failed to connect to data base: ~A" e))))

(defun disconnect ()
  "Disconnect the global connection"
  (when (postmodern:connected-p postmodern:*database*)
    (postmodern:disconnect-toplevel)
    (log:info "Disconnected top-level connection")))
