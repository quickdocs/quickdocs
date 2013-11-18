PROJECT_ROOT=$(abspath $(dir $(lastword $(MAKEFILE_LIST))))

all: schema quicklisp

quicklisp:
	(curl -L http://beta.quicklisp.org/quicklisp.lisp && echo '(quicklisp-quickstart:install :path #P"quicklisp/")') | sbcl --no-inform --no-userinit

schema:
	mysql -uroot -e 'CREATE DATABASE IF NOT EXISTS quickdocs DEFAULT CHARACTER SET utf8';
	mysql -uroot quickdocs < $(PROJECT_ROOT)/db/schema.sql
