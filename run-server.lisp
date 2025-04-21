;; run-server.lisp with hot reload
(load "~/quicklisp/setup.lisp")

(ql:quickload :lisp-crud)
(ql:quickload '(:log4cl :bordeaux-threads :cl-fad :woo :postmodern :uuid :ningle :cl-who :clack))
(use-package :cl)

(defvar *server-thread* nil)
(defvar *file-watcher-thread* nil)
(defvar *last-reload-time* (get-universal-time))
(defvar *watch-directories* '("src/" "templates/" "static/"))  ;; Update these paths to match your project structure
(defvar *file-timestamps* (make-hash-table :test 'equal))

(defun collect-files-to-watch ()
  "Collect all Lisp files in watched directories"
  (let ((files-to-watch nil))
    (dolist (dir *watch-directories*)
      (when (cl-fad:directory-exists-p dir)
        (cl-fad:walk-directory 
          dir
          (lambda (file)
            (when (member (pathname-type file) '("lisp" "asd" "cl") :test #'string=)
              (push file files-to-watch)))
          :directories nil)))
    files-to-watch))

(defun initialize-file-timestamps ()
  "Initialize the hash table with current file timestamps"
  (clrhash *file-timestamps*)
  (dolist (file (collect-files-to-watch))
    (setf (gethash (namestring file) *file-timestamps*)
          (file-write-date file))))

(defun files-changed-p ()
  "Check if any files have changed since last check"
  (let ((changed-files nil))
    (dolist (file (collect-files-to-watch))
      (let* ((file-path (namestring file))
              (current-date (file-write-date file))
             (stored-date (gethash file-path *file-timestamps*)))
        (when (or (null stored-date) (> current-date stored-date))
          (push file-path changed-files)
          (setf (gethash file-path *file-timestamps*) current-date))))
    changed-files))

(defun reload-system ()
  (log:info "Reloading system due to file changes...")
  (when *server-thread*
    (log:info "Stopping current server...")
    (ignore-errors
      (lisp-crud:stop-server)
      (when (and *server-thread* (sb-thread:thread-alive-p *server-thread*))
        (sb-thread:terminate-thread *server-thread*))))
  
  (log:info "Reloading code...")
  (setf *server-thread* 
        (sb-thread:make-thread 
          (lambda ()
            (handler-case
                (let ((port 8080))
                  (lisp-crud:start-server port)
                  (log:info "Server restarted successfully"))
              (error (e)
                (log:error "Error restarting server: ~A" e))))
          :name "server-thread"))
  (setf *last-reload-time* (get-universal-time)))

(defun start-file-watcher ()
  (log:info "Starting file watcher for hot reload...")
  (initialize-file-timestamps)
  
  ;; Create watcher thread
  (setf *file-watcher-thread*
        (sb-thread:make-thread
          (lambda ()
            (loop
              (let ((changed-files (files-changed-p)))
                (when changed-files
                  (log:info "Detected changes in files: ~A" changed-files)
                  (reload-system)))
              ;; Check every second
              (sleep 1)))
          :name "file-watcher-thread")))

;; Start the server and file watcher
(lisp-crud:start-server 8080)
(start-file-watcher)

;; Keep the main thread alive
(sb-thread:join-thread *file-watcher-thread*)