(in-package :cl-user)
(defpackage quickdocs.model.project
  (:use :cl
        :annot.class
        :caveman2.db
        :quickdocs.model.system
        :split-sequence))
(in-package :quickdocs.model.project)

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
@export-accessors
(defclass <project> ()
  ((id :type integer
       :initarg :|id|
       :reader project-id)
   (ql-dist-version :type string
                    :initarg :|ql_dist_version|
                    :reader dist-version)
   (name :type string
         :initarg :|name|
         :reader project-name)
   (release-version :type string
                    :initarg :|release_version|
                    :reader project-release-version)
   (homepage-url :type (or null string)
                 :initarg :|homepage_url|
                 :initform nil
                 :reader project-homepage-url)
   (repos-url :type (or null string)
              :initform nil
              :initarg :|repos_url|
              :reader project-repos-url)
   (archive-url :type (or null string)
                :initform nil
                :initarg :|archive_url|
                :reader project-archive-url)
   systems
   readme
   authors
   maintainers
   licenses
   categories
   description))

(defmethod initialize-instance :around ((project <project>) &rest initargs)
  (loop for column in '(:|homepage_url| :|repos_url| :|archive_url|)
        when (getf initargs column)
          do (setf (getf initargs column)
                   (babel:octets-to-string (getf initargs column))))
  (apply #'call-next-method project initargs))

@export
(defmethod project-systems ((project <project>))
  (unless (slot-boundp project 'systems)
    (load-project-systems project))

  (slot-value project 'systems))

@export
(defmethod project-authors ((project <project>))
  (unless (slot-boundp project 'authors)
    (load-project-authors project))

  (slot-value project 'authors))

@export
(defmethod project-maintainers ((project <project>))
  (unless (slot-boundp project 'maintainers)
    (load-project-authors project))

  (slot-value project 'maintainers))

@export
(defmethod project-readme ((project <project>))
  (unless (slot-boundp project 'readme)
    (load-project-readme project))

  (slot-value project 'readme))

@export
(defmethod project-licenses ((project <project>))
  (loop with licenses
        for system in (project-systems project)
        when (system-license system)
          do (pushnew (system-license system) licenses :test #'string=)
        finally
           (return (nreverse licenses))))

@export
(defmethod project-categories ((project <project>))
  (unless (slot-boundp project 'categories)
    (load-project-categories project))

  (slot-value project 'categories))

(defun search-project-dependencies (project &key (depends t))
  (let ((dependencies (mapcar
                       (lambda (row)
                         (apply #'make-instance '<project> row))
                       (select-all (connect-db) :project.*
                         (from :system_dependencies)
                         (if depends
                             (left-join :system :on (:= :system.id :system_dependencies.depends_system_id))
                             (left-join :system :on (:= :system.id :system_dependencies.system_id)))
                         (left-join :project :on (:= :system.project_id :project.id))
                         (if depends
                             (where (:and (:in :system_dependencies.system_id (mapcar #'system-id (project-systems project)))
                                          (:!= :project.name (project-name project))))
                             (where (:and (:in :system_dependencies.depends_system_id (mapcar #'system-id (project-systems project)))
                                          (:!= :project.name (project-name project)))))
                         (group-by :project.id)))))
    (loop for dependency in dependencies
          collect (list :name (project-name dependency)
                        :description (project-description dependency)))))

@export
(defmethod project-dependencies ((project <project>))
  (search-project-dependencies project :depends t))

@export
(defmethod depending-projects ((project <project>))
  (search-project-dependencies project :depends nil))

@export
(defmethod project-description ((project <project>))
  (unless (slot-boundp project 'description)
    (load-project-description project))

  (slot-value project 'description))

@export
(defmethod project-download-count ((project <project>))
  (gethash (project-name project) *ql-download-stats-hash*))

@export
(defun find-project (name &optional (dist-version (ql-dist:version (ql-dist:dist "quicklisp"))))
  (let ((row (select-one (connect-db) :*
               (from :project)
               (where (:and (:= :ql_dist_version dist-version)
                            (:= :name name)))
               (limit 1))))
    (when row
      (apply #'make-instance '<project> row))))

(defmethod load-project-readme ((project <project>))
  (let* ((db (connect-db))
         (row (select-one db :*
                (from :project_readme)
                (where (:= :project_id (project-id project)))
                (limit 1))))
    (setf (slot-value project 'readme) row)))

(defmethod load-project-authors ((project <project>))
  (unless (project-systems project)
    (return-from load-project-authors nil))

  (let ((all-authors (select-all (connect-db) (:type :author_name (:as (:count :*) :count))
                       (from :system_author)
                       (where (:in :system_id
                                   (mapcar #'system-id (project-systems project))))
                       (group-by :type :author_name)
                       (order-by (:desc :count) :id))))
    (loop for author in all-authors
          if (string= (getf author :|type|) "author")
            collect author into authors
          else
            collect author into maintainers
          finally
             (setf (slot-value project 'authors) authors
                   (slot-value project 'maintainers) maintainers))))

(defmethod load-project-systems ((project <project>))
  (setf (slot-value project 'systems)
        (search-systems-by-project-id (project-id project))))

(defmethod load-project-categories ((project <project>))
  (setf (slot-value project 'categories)
        (mapcar
         #'(lambda (row) (getf row :|category|))
         (select-all (connect-db) :category
           (from :project_category)
           (where (:= :project_name (project-name project)))))))

(defmethod load-project-description ((project <project>))
  (setf (slot-value project 'description)
        (let ((system (find-primary-system (project-id project) (project-name project))))
          (if (and system (getf system :|description|))
              (babel:octets-to-string (getf system :|description|))
              (getf (select-one (connect-db) :description
                      (from :project_cliki_description)
                      (where (:= :project_id (project-id project)))
                      (limit 1))
                    :|description|)))))
