LISP=sbcl
SERVER_PORT=8080
SWANK_PORT=4005
PROJECT_ROOT=$(abspath $(dir $(lastword $(MAKEFILE_LIST))))

all: bin

bin: parse render

setup: ensure_bin_dir
	chmod a+w $(PROJECT_ROOT)/bin
	git submodule update --init

ensure_bin_dir:
	mkdir -p $(PROJECT_ROOT)/bin

start: bin
	$(call $(LISP), \
		(ql:quickload :quickdocs) (ql:quickload :swank), \
		(quickdocs.server:start-server :mode :production :debug nil :server :fcgi :port $(SERVER_PORT)) \
		(swank:create-server :port $(SWANK_PORT) :style :spawn :dont-close t))

parse: ensure_bin_dir
	$(call $(LISP)-save,bin/parse,main, \
		(ql:quickload :quickdocs), \
		(defun main () (prin1 (handler-bind ((error (function continue))) (quickdocs.parser:parse-documentation (asdf:find-system (cadr $($(LISP)_argv))))))))

render: ensure_bin_dir
	$(call $(LISP)-save,bin/render,main, \
		(ql:quickload :quickdocs), \
		(defun main () (prin1 (handler-bind ((error (function continue))) (quickdocs.renderer:render-api-reference (ql-dist:find-release (cadr $($(LISP)_argv))))))))

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
	$(call sbcl, $3, $4 \
		(sb-ext:save-lisp-and-die "$(PROJECT_ROOT)/$1" :executable t :toplevel (quote $2)))
endef

ccl_argv=ccl:*command-line-argument-list*

define ccl
	ccl --quiet --batch \
		--eval '(pushnew #P"$(PROJECT_ROOT)/" asdf:*central-registry*)' \
		--eval '(progn $1)' \
		--eval '(progn $2)'
endef

define ccl-save
	$(call ccl, $3, $4 \
		(ccl:save-application #P"$(PROJECT_ROOT)/$1" :toplevel-function (function $2) :prepend-kernel t))
endef
