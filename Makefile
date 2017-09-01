EMACS ?= emacs
ELS = php-runtime.el php-runtime-test.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

all: $(ELCS)

clean:
	rm -f $(ELCS)

test: clean all
	$(EMACS) -Q --batch -L . -l php-runtime-test.el -f ert-run-tests-batch-and-exit

.PHONY: clean test
