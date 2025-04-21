;;;; lisp-crud.lisp

(in-package #:lisp-crud)

;; Ensure necessary packages are loaded (handled by ASDF now)
(use-package :lisp-crud.di)
(use-package :lisp-crud.adapters.postgres)
(use-package :lisp-crud.utilities)
(use-package :lisp-crud.adapters.entrypoints.http)

(defvar *app* (make-instance 'ningle:app))
(defvar *server* nil)
(defvar *usecases* nil)
(log:config :info)

;; Correct implementation of with-html macro
(defmacro with-html ((stream-sym) &body body)
  `(cl-who:with-html-output-to-string (,stream-sym)
                                      ,@body))

;; Counter for visitor tracking
(let ((counter 0))
  (setf (ningle:route *app* "/hello")
        (lambda (params)
          (declare (ignore params))
          (with-html (s)
            (:html
              (:head (:title "Hello World"))
              (:body
                (:h1 "Greetings!")
                (:p "You are visitor number " (cl-who:str (incf counter)))))))))

;; Root route
(setf (ningle:route *app* "/")
      (lambda (params)
        (declare (ignore params))
        "Root page is working! Try <a href='/hello'>the hello page</a> or <a href='/api/users'>the users API</a>"))

;; Register all API endpoints
(defun register-api-endpoints (app usecases)
  "Register all API endpoints with the application"
  (log:info "Registering API endpoints")
  
  ;; Mount the user controller endpoints under /api prefix
  (lisp-crud.adapters.entrypoints.http:register-entrypoints app usecases)
  
  ;; Return the configured app
  app)

(defun start-server (&optional (port 8080))
  (log:info "Loading environment variables")  
  (lisp-crud.utilities:load-env)

  ;; Configure database connection
  (log:info "Configuring postgres")
  (lisp-crud.adapters.postgres:configure
    (or (lisp-crud.utilities:getenv "DB_NAME") "lisp-crud")
    (or (lisp-crud.utilities:getenv "DB_USER") "user")
    (or (lisp-crud.utilities:getenv "DB_PASS") "password")
    (or (lisp-crud.utilities:getenv "DB_HOST") "localhost")
    :port (parse-integer (or (lisp-crud.utilities:getenv "DB_PORT") "5432")))

  ;; Establish connection
  (log:info "Establishing connection with postgres")
  (lisp-crud.adapters.postgres:connect)

  ;; Initialize the server
  (initialize port))

;; Move initialize outside of start-server
(defun initialize (port)
  ;; Initialize the dependency injection container
  (log:info "Initializing dependency injection container")
  (setf *usecases* (lisp-crud.di:make-domain-container))

  ;; Register all API endpoints
  (log:info "Setting up API endpoints")
  (register-api-endpoints *app* *usecases*)

  (log:info "Starting Ningle server on port ~A...~%" port)
  (log:info "Visit http://localhost:~A/ to see the landing page~%" port)
  (log:info "Visit http://localhost:~A/api/users to access the users API~%" port)

  ;; Stop the server if it's already running
  (when *server*
    (log:info "~&Stopping previous server instance~%")
    (clack:stop *server*))
  
  ;; Start a new server
  (setf *server* 
        (clack:clackup *app* :server :woo :port port))
  
  (log:info "~&Server started successfully~%"))

(defun stop-server ()
  (when *server*
    (log:info "~&Stopping server...~%")
    (clack:stop *server*)
    (setf *server* nil)
    (log:info "~&Server stopped~%")))
