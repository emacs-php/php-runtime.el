EMACS ?= emacs
EASK ?= eask

install:
	$(EASK) package
	$(EASK) install

compile:
	$(EASK) compile

# TODO: Add `test` back once it's passed on all OSs!
all: clean install compile

clean:
	$(EASK) clean all

test:
	$(EASK)	test ert ./php-runtime-test.el

.PHONY: clean test
