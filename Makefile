.PHONY: build clean install test

build : clean
	@cask build

clean :
	@cask clean-elc

install :
	@cask install

test : clean
	@cask build \
	&& cask emacs -q -batch -L . -l tests/org-evil-tests.el -f ert-run-tests-batch-and-exit
