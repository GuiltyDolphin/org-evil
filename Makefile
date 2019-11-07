.PHONY: build test

build :
	@cask build

test :
	@cask build \
	&& cask emacs -batch -L . -l tests/org-evil-tests.el -f ert-run-tests-batch-and-exit
