.PHONY: vendor test test-sbcl test-ccl test-ecl test-abcl pubdocs

sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidocs = $(shell ls docs/*reference*.markdown)

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp


# Testing ---------------------------------------------------------------------
test: test-sbcl test-ccl test-ecl test-abcl

test-sbcl:
	echo; figlet -kf computer 'SBCL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	ros run -L sbcl --load test/run.lisp

test-ccl:
	echo; figlet -kf slant 'CCL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	ros run -L ccl-bin --load test/run.lisp

test-ecl:
	echo; figlet -kf roman 'ECL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	ros run -L ecl --load test/run.lisp

test-abcl:
	echo; figlet -kf broadway 'ABCL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	abcl --load test/run.lisp

# Documentation ---------------------------------------------------------------
$(apidocs): $(sourcefiles)
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

docs/build/index.html: $(docfiles) $(apidocs) docs/title
	cd docs && ~/.virtualenvs/d/bin/d

docs: docs/build/index.html

pubdocs: docs
	hg -R ~/src/sjl.bitbucket.org pull -u
	rsync --delete -a ./docs/build/ ~/src/sjl.bitbucket.org/cl-digraph
	hg -R ~/src/sjl.bitbucket.org commit -Am 'cl-digraph: Update site.'
	hg -R ~/src/sjl.bitbucket.org push
