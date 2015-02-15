EMACS ?= emacs
CASK ?= cask
ECUKES ?= ecukes

test: elpa
	${CASK} exec ${EMACS} -Q -batch -L . -L tests \
		-l tests/evil-smartparens-tests.el \
		-f ert-run-tests-batch-and-exit

ecukes-features: elpa
	${CASK} exec ${ECUKES} --no-win

elpa:
	mkdir -p elpa
	${CASK} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

clean-elc:
	rm -f *.elc tests/*.elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CASK=${CASK}
	@echo ECUKES=${ECUKES}

travis-ci: print-deps test
