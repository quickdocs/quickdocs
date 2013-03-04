(in-package :cl-user)
(defpackage quickdocs.renderer.readme
  (:use :cl)
  (:import-from :cl-markdown
                :markdown)
  (:import-from :cl-fad
                :list-directory)
  (:import-from :trivial-shell
                :shell-command)
  (:import-from :alexandria
                :when-let)
  (:import-from :quickdocs.renderer.util
                :slurp-file))
(in-package :quickdocs.renderer.readme)

(cl-annot:enable-annot-syntax)

(defparameter *pandoc-path*
              (merge-pathnames #P".cabal/bin/pandoc"
                               (user-homedir-pathname)))

(defun parse-markdown (file)
  (multiple-value-bind (stdout stderr code)
      (trivial-shell:shell-command
       (format nil "~A ~A" *pandoc-path* file)
       :input "")
    (declare (ignore stderr))
    (if (= 0 code)
        stdout
        ;; fallback to CL-Markdown
        (with-output-to-string (s)
          (cl-markdown:markdown file :stream s)))))

(defmethod find-system-readme ((system asdf:system) &optional base-directory)
  (remove-if-not
   #'(lambda (path)
       (let ((filename (file-namestring path)))
         (and (>= (length filename) 6)
              (string= "README" (subseq filename 0 6)))))
   (fad:list-directory
    (or base-directory
        (slot-value system 'asdf::absolute-pathname)))))

(defmethod find-system-readme ((system ql-dist:system) &optional base-directory)
  (when-let (asdf-system
             (ignore-errors
               (asdf:find-system (slot-value system 'ql-dist:name))))
    (find-system-readme asdf-system
                        (or base-directory
                            (ql-dist:base-directory (slot-value system 'ql-dist:release))))))

(defun readme->html (readme-file)
  (let ((ext (pathname-type readme-file)))
    (cond
      ((find ext '("md" "markdown" "mkdn")
             :test #'string-equal)
       (format nil "<div class=\"markdown\">~A</div>"
               (parse-markdown readme-file)))
      (t (format nil "<div class=\"plain-text\"><pre>~A</pre></div>"
                 (emb::escape-for-xml (slurp-file readme-file)))))))

@export
(defun project-readme (system)
  (when-let (readme
             (find-system-readme system))
    (slurp-file (car readme))))

@export
(defun project-readme-in-html (system)
  (when-let (readme
             (find-system-readme system))
    (readme->html (car readme))))
