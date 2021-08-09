.PHONY: build test

build :
	@cask build

test :
	@cask build \
	&& cask emacs -q -batch -L . -l tests/org-evil-tests.el -f ert-run-tests-batch-and-exit 2>&1 | grep -v -e '^-- INSERT --$$'
