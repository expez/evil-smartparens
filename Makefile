EMACS ?= emacs
CASK ?= cask

test: elpa
	${CASK} exec ${EMACS} -Q -batch -L . -L tests/evil-tests \
		-l tests/evil-smartparens-tests.el \
		-f ert-run-tests-batch-and-exit

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

ci-test:
	$(CASK) excec ${EMACS} --version
	EMACS=$$(evm bin) ${CASK}
	EMACS=$$(evm bin) ${CASK} exec $$(evm bin) -Q -batch -L . -L tests/evil-tests \
		-l tests/evil-smartparens-tests.el \
		-f ert-run-tests-batch-and-exit

travis-ci: print-deps ci-test
