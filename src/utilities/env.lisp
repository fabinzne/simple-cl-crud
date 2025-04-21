(in-package #:lisp-crud.utilities)

(defun getenv (name)
  "Get environment variable value"
  (uiop:getenv name))

(defun load-env (&optional (path #P".env"))
  "Load environment variables from .env file"
  (when (probe-file path)
    (with-open-file (stream path)
      (loop for line = (read-line stream nil nil)
            while line 
            do (let ((trimmed-line (string-trim '(#\Space #\Tab) line)))
                (when (and (not (string= "" trimmed-line)) 
                            (not (char= #\# (char trimmed-line 0)))) 
                  (let ((split-pos (position #\= trimmed-line))) 
                    (when split-pos
                      (let ((key (string-trim '(#\Space #\Tab) (subseq trimmed-line 0 split-pos)))
                            (val (string-trim '(#\Space #\Tab) (subseq trimmed-line (1+ split-pos)))))
                        (setf (uiop:getenv key) val))))))))))