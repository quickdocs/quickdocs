(in-package :cl-user)
(defpackage quickdocs.search
  (:use :cl
        :split-sequence))
(in-package :quickdocs.search)

(cl-annot:enable-annot-syntax)

(defparameter *ql-download-stats* "
1812 alexandria
1502 trivial-features
1391 slime
1225 babel
1207 cffi
1115 trivial-gray-streams
1113 cl+ssl
1043 flexi-streams
976 quicklisp-slime-helper
943 trivial-garbage
902 cl-fad
871 rfc2388
865 cl-ppcre
838 bordeaux-threads
752 trivial-backtrace
709 hunchentoot
694 usocket
683 drakma
613 closer-mop
597 iterate
585 cl-base64
580 chunga
543 cl-who
500 parenscript
500 md5
484 puri
472 nibbles
456 local-time
455 split-sequence
426 uffi
410 cl-glfw
406 salza2
395 fare-utils
366 anaphora
364 cl-json
358 named-readtables
350 cl-vectors
342 clx
300 ironclad
296 metatilities-base
284 zpng
280 zpb-ttf
280 asdf-utils
278 xcvb
262 metabang-bind
251 vecto
251 lispbuilder
237 clsql
235 html-template
228 postmodern
213 fare-matcher
211 cl-opengl
206 plokami
203 asdf-system-connections
195 weblocks
193 clack
192 osicat
188 s-xml
175 closure-common
172 cl-containers
168 parse-number
164 cxml
163 lisp-unit
153 mcclim
149 restas
146 f-underscore
145 cl-cont
142 moptilities
139 elephant
138 trivial-timeout
137 trivial-shell
135 metatilities
134 lparallel
134 cl-annot
133 cl-unicode
133 cl-syntax
130 yason
130 chipz
127 portableaserve
127 hu.dwim.asdf
126 quickproject
124 cl-irc
123 fiveam
122 iolib
121 trivial-types
118 linedit
118 closure-html
117 madeira-port
116 cl-pdf
115 cl-oauth
114 cl-cairo2
113 antik
111 esrap
110 trivial-utf-8
108 ltk
106 plexippus-xpath
106 gsll
105 let-plus
104 xmls
104 cl-gtk2
")

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

  (let ((re (mapcar
             #'(lambda (q)
                 (ppcre:create-scanner (ppcre:quote-meta-chars q)))
             (ppcre:split "\\s+" query))))
    (remove-if-not
     #'(lambda (release)
         (let ((project-name (slot-value release 'ql-dist:project-name)))
           (every #'(lambda (re)
                      (ppcre:scan re project-name))
                  re)))
     (ql-dist:provided-releases t))))

@export
(defun sort-by-download-count (a b)
  (> (gethash (slot-value a 'ql-dist:project-name) *ql-download-stats-hash* 0)
     (gethash (slot-value b 'ql-dist:project-name) *ql-download-stats-hash* 0)))