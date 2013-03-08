#-quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init))))

(ql:quickload :trivial-shell)
(ql:quickload :cl-test-more)

(import '(cl-test-more:plan
          cl-test-more:ok))

(defparameter *parse-binary*
              (asdf:system-relative-pathname :quickdocs #P"bin/parse"))

(defparameter *data-path*
              (asdf:system-relative-pathname :quickdocs #P"data/parse/"))

(defparameter *error-path*
              (asdf:system-relative-pathname :quickdocs #P"data/error/"))

(ensure-directories-exist *data-path*)
(ensure-directories-exist *error-path*)

(defun data-path (system &optional (base *data-path*))
  (ensure-directories-exist
   (merge-pathnames (format nil "~A/~A"
                            (ql-dist:project-name (ql-dist:release system))
                            (slot-value system 'ql-dist:name))
                    base)))

(defvar *cpulimit*
    (sb-ext:run-program
     "/bin/sh"
     `("-c" ,(format nil "cpulimit -e ~A -l 30"
              *parse-binary*))
     :wait nil))

(defun run-parse (system)
  (sb-ext:run-program "/bin/sh"
   `("-c" ,(format nil "~A ~A" *parse-binary* (ql-dist:name system)))
   :wait nil
   :output :stream
   :error :stream))

(defvar *systems*
        (loop for release in (ql-dist:provided-releases t)
              append
           (loop for system in (ql-dist:provided-systems release)
                 when (ql-dist:installedp system) collect system)))

(defvar *error-systems* nil)

(plan (length *systems*))

(loop for system in *systems*
      for real-time = (get-internal-real-time)
      for process = (run-parse system)
      for output-thread = (sb-thread:make-thread
                           #'(lambda ()
                               (trivial-shell::file-to-string-as-lines
                                (sb-impl::process-output process))))
      for error-thread = (sb-thread:make-thread
                          #'(lambda ()
                              (trivial-shell::file-to-string-as-lines
                               (sb-impl::process-error process))))
      do
   (let ((error-code
          (sb-impl::process-exit-code (sb-impl::process-wait process)))
         (output-string  (sb-thread:join-thread output-thread))
         (error-string  (sb-thread:join-thread error-thread)))
     (close (sb-impl::process-output process))
     (close (sb-impl::process-error process))
     (if (= error-code 0)
         (with-open-file (out (data-path system)
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede
                              :external-format :utf-8)
           (princ output-string out))
         (with-open-file (out (data-path system *error-path*)
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede
                              :external-format :utf-8)
           (princ error-string out)))
     (ok (= error-code 0)
         (format nil "~A (~Fs)"
                 (ql-dist:name system)
                 (/ (- (get-internal-real-time) real-time) 1000)))
     (unless (= error-code 0)
       (push (ql-dist:name system) *error-systems*))))

(test-more:finalize)

(format *error-output* "~&~{~&#~4t~A~%~}~%"
        (reverse *error-systems*))

(sb-ext:process-kill *cpulimit* sb-unix::sigkill)
