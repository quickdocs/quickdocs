(in-package :cl-user)
(defpackage quickdocs.search
  (:use :cl
        :split-sequence)
  (:import-from :alexandria
                :ensure-list)
  (:import-from :quickdocs.renderer.category
                :*category-db*)
  (:import-from :quickdocs.renderer.description
                :project-description))
(in-package :quickdocs.search)

(cl-annot:enable-annot-syntax)

(defparameter *ql-download-stats* "
1971 alexandria
1642 cffi
1546 babel
1523 trivial-features
1319 trivial-gray-streams
1274 slime
1259 cl-ppcre
1218 trivial-garbage
1144 cl+ssl
1058 bordeaux-threads
972 usocket
970 flexi-streams
933 cl-fad
883 chunga
806 md5
794 cl-base64
766 quicklisp-slime-helper
756 closer-mop
711 hunchentoot
653 drakma
619 rfc2388
590 nibbles
579 iterate
530 ironclad
524 clsql
523 puri
499 trivial-backtrace
447 uffi
424 cl-who
403 split-sequence
330 anaphora
322 named-readtables
319 salza2
315 lispbuilder
296 metabang-bind
295 cl-vectors
293 uuid
286 clack
285 postmodern
284 cl-opengl
280 local-time
268 trivial-utf-8
261 clx
258 zpng
244 cl-json
236 cl-smtp
231 fare-utils
226 zpb-ttf
225 parse-number
224 cl-clon
223 gbbopen
221 yason
221 restas
218 vecto
216 cl-cairo2
208 cl-closure-template
190 weblocks
189 parenscript
189 cl-annot
188 osicat
188 cxml
183 closure-common
181 optima
176 trivial-shell
173 lparallel
172 esrap
171 clfswm
169 cl-unicode
169 cl-syntax
169 cl-csv
166 html-template
161 trivial-types
159 lisp-unit
158 commonqt
153 metatilities-base
152 let-plus
151 bknr-datastore
150 mcclim
150 hu.dwim.asdf
149 cl-routes
147 portableaserve
146 contextl
146 cl-store
143 com.informatimago
139 quickproject
139 asdf-system-connections
135 ieee-floats
133 cl-project
130 data-sift
130 cl-marshal
130 caveman
129 linedit
129 gsll
129 antik
128 cl-utilities
127 hu.dwim.util
123 ltk
121 plokami
121 f2cl
120 cl-containers
")

@export
(defvar *ql-download-stats-hash*
    (let ((hash (make-hash-table :test 'equal)))
      (loop for line in (split-sequence #\Newline
                                         (string-trim '(#\Space #\Newline) *ql-download-stats*))
            for (count project-name) = (split-sequence #\Space line)
            do (setf (gethash project-name hash)
                     (parse-integer count)))
      hash))

@export
(defun search-projects (query)
  (when (or (null query)
            (string= query ""))
    (return-from search-projects (ql-dist:provided-releases t)))

  (remove-duplicates
   (append
    (ensure-list (search-exact-project query))
    (sort (search-by-categories query) #'sort-by-download-count)
    (sort (search-by-name query) #'sort-by-download-count)
    (sort (search-by-description query) #'sort-by-download-count))
   :test #'string-equal
   :key #'ql-dist:project-name
   :from-end t))

@export
(defun search-exact-project (query)
  (find-if
   #'(lambda (release)
       (string= query
                (slot-value release 'ql-dist:project-name)))
   (ql-dist:provided-releases t)))

(defun search-by (query &key key)
  (let ((re (mapcar
             #'(lambda (q)
                 (ppcre:create-scanner (ppcre:quote-meta-chars q) :case-insensitive-mode t))
             (ppcre:split "\\s+" query))))
    (remove-if-not
     #'(lambda (release)
         (let ((key (if key
                        (funcall key release)
                        release)))
           (every #'(lambda (re)
                      (ppcre:scan re key))
                  re)))
     (ql-dist:provided-releases t))))

@export
(defun search-by-name (query)
  (search-by query :key #'ql-dist:project-name))

@export
(defun search-by-categories (word)
  (loop with scanner = (ppcre:create-scanner (ppcre:quote-meta-chars word) :case-insensitive-mode t)
        for (project . categories) in *category-db*
        if (and (find-if
                     #'(lambda (category)
                         (or (string-equal category word)
                             (ppcre:scan scanner category)))
                     categories)
                (ql-dist:find-release project))
          collect project into project-names
        finally
     (return (mapcar #'ql-dist:find-release project-names))))

@export
(defun search-by-description (query)
  (search-by query
             :key #'(lambda (release)
                      (project-description (ql-dist:project-name release)))))

@export
(defun sort-by-download-count (a b)
  (> (gethash (slot-value a 'ql-dist:project-name) *ql-download-stats-hash* 0)
     (gethash (slot-value b 'ql-dist:project-name) *ql-download-stats-hash* 0)))