LISP=sbcl
SERVER_PORT=8080
SWANK_PORT=4005
PROJECT_ROOT=$(abspath $(dir $(lastword $(MAKEFILE_LIST))))
ERROR_LOG=/dev/null

all: bin

bin: parse render

setup: ensure_bin_dir
	chmod a+w $(PROJECT_ROOT)/bin
	git submodule update --init

ensure_bin_dir:
	mkdir -p $(PROJECT_ROOT)/bin

start:
	$(call $(LISP), \
		(ql:quickload :quickdocs) (ql:quickload :swank), \
		(quickdocs.server:start-server :mode :production :debug nil :server :fcgi :port $(SERVER_PORT) :error-log "$(ERROR_LOG)") \
		(swank:create-server :port $(SWANK_PORT) :style :spawn :dont-close t))

parse: ensure_bin_dir
	$(call $(LISP)-save,bin/parse, \
		(ql:quickload :quickdocs), \
		(prin1 (let ((retry-recompile 0) (retry-continue 0)) \
			(handler-bind ( \
			(error (lambda (e) (if (and (< retry-recompile 5) (find-restart (quote asdf:try-recompiling))) (progn (incf retry-recompile) (invoke-restart (quote asdf:try-recompiling))) (signal e)))) \
			(error (lambda (e) (if (and (< retry-continue  5) (find-restart (quote continue))) (progn (incf retry-continue) (invoke-restart (quote continue))) (signal e))))) \
			(quickdocs.parser:parse-documentation (asdf:find-system (cadr $($(LISP)_argv))))))))

render: ensure_bin_dir
	$(call $(LISP)-save,bin/render, \
		(ql:quickload :quickdocs), \
		(princ (handler-bind ((error (function continue))) (quickdocs.renderer:render-api-reference (ql-dist:find-release (cadr $($(LISP)_argv)))))))

#
# Lisp Implementation

sbcl_argv=sb-ext:*posix-argv*

define sbcl
	sbcl --noinform --disable-debugger \
		--eval '(pushnew #P"$(PROJECT_ROOT)/" asdf:*central-registry*)' \
		--eval '(progn $1)' \
		--eval '(progn $2)'
endef

define sbcl-save
	$(call sbcl, \
		(setf ql:*local-project-directories* nil) $2, \
		(defun main () \
			(setf sb-impl::*default-external-format* :utf-8) \
			(setf sb-alien::*default-c-string-external-format* :utf-8) \
			$3) \
		(sb-ext:save-lisp-and-die "$(PROJECT_ROOT)/$1" :executable t :toplevel (quote main)))
endef

ccl_argv=ccl:*command-line-argument-list*

define ccl
	ccl --quiet --batch \
		--eval '(pushnew #P"$(PROJECT_ROOT)/" asdf:*central-registry*)' \
		--eval '(progn $1)' \
		--eval '(progn $2)'
endef

define ccl-save
	$(call ccl, \
		(setf ql:*local-project-directories* nil) $2, \
		(defun main () $3) \
		(ccl:save-application #P"$(PROJECT_ROOT)/$1" :toplevel-function (function main) :prepend-kernel t))
endef
