EMACS ?= emacs
EASK ?= eask

install:
	$(EASK) package
	$(EASK) install

compile:
	$(EASK) compile

all: clean install compile test

clean:
	$(EASK) clean all

test:
	$(EASK)	test ert ./php-runtime-test.el

.PHONY: clean test
