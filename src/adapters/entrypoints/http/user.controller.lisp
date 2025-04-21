(in-package #:lisp-crud.adapters.entrypoints.http)

(defvar *app* (make-instance 'ningle:app))

(defun parse-json-body (request)
  "Parse the JSON request body to a Lisp hash table."
  (let ((body-params (lack.request:request-body-parameters request)))
    (cond
      ((hash-table-p body-params) body-params)
      ((and (listp body-params) (consp (car body-params)))
        ;; Convert the alist to hash table
        (alexandria:alist-hash-table body-params :test #'equal))
      ((stringp body-params) (cl-json:decode-json-from-string body-params))
      (t (error "Unknown body type: ~a" body-params)))))

(defun json-response (data &optional (status 200))
  "Create a JSON HTTP response."
  (log:info "Converting json ~A" (json:encode-json-to-string data))
  (list status
        '(:content-type "application/json")
        (list (json:encode-json-to-string data))))

(defun register-user-endpoints (app usecases)
  "Register all user-related endpoints on the given app."
  
  ;; POST /api/users - Create a new user
  (setf (ningle:route app "/api/users" :method :POST)
        (lambda (params)
          (declare (ignore params))
          (handler-case
              (let* ((body (parse-json-body ningle:*request*))
                      (name (gethash "name" body))
                      (email (gethash "email" body))
                      (create-user (getf usecases :create-user)))
                (log:info "Received body: ~A name: ~A and email: ~A" body name email)
                (let ((user (funcall create-user name email)))
                  (json-response
                    (alexandria:plist-hash-table
                      `("message" "User created successfully"
                        "user" ,(alexandria:plist-hash-table `("id" ,(lisp-crud.domain.entities:user-id user)
                        "name" ,(lisp-crud.domain.entities:user-name user)
                        "email" ,(lisp-crud.domain.entities:user-email user))))
                    :test #'equal)
                  201)))
            (error (e)
              (log:error "Error creating user: ~a" e)
              (json-response
                (alexandria:plist-hash-table
                `("error" ,(format nil "Failed to create user: ~a" e))
                :test #'equal)
                500)))))
        
  (setf (ningle:route app "/api/users/:id" :method :DELETE)
    (lambda (params)
      (handler-case
        (progn
          (let ((id (cdr (assoc :id params)))
            (delete-user (getf usecases :delete-user)))
          (log:info "Receid id: ~A" id)
          (let ((result (funcall delete-user id)))
            (json-response
              (alexandria:plist-hash-table
                `("message" "User deleted successfully"))
                200))
            ))
        (error (e)
          (log:error "Error deleting user: ~A" e)
          (json-response
            (alexandria:plist-hash-table
              `("error" ,(format nil "Failed to delete user: ~A" e))
              :test #'equal
              500))))))
              
  (setf (ningle:route app "/api/users/:id" :method :PUT)
    (lambda (params)
      (handler-case
        (progn
          (let* ((id (cdr (assoc :id params)))
                  (body (parse-json-body ningle:*request*))
                  (name (gethash "name" body))
                  (email (gethash "email" body))
                  (update-user (getf usecases :update-user)))
          (log:info "Received request id: ~A name: ~A email: ~A" id name email)
          (let ((user (funcall update-user id name email)))
            (json-response
              (alexandria:plist-hash-table
                `("message" "User updated succesfully"
                  "user"    ,(alexandria:plist-hash-table 
                  `("id"    ,(lisp-crud.domain.entities:user-id user)
                    "name"  ,(lisp-crud.domain.entities:user-name user)
                    "email" ,(lisp-crud.domain.entities:user-email user))))
                :test #'equal)
              200))))
        (error (e)
          (log:error "Error updating user ~A" e)
          (json-response
            (alexandria:plist-hash-table
            `("error" ,(format nil "Failed to update user: ~A" e))
            :test #'equal)
            500))))))


(defun user-controller ()
  "Return the user controller instance."
  *app*)

(defun register-entrypoints (app usecases)
  "Register all endpoints on the given app."
  (register-user-endpoints app usecases)
  app)