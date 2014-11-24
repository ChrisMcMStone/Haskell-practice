LABNO = 5

TESTB = TestBench$(LABNO).hs
TESTGEN = GenerateTests$(LABNO).hs
TEMPLATE = Template$(LABNO).hs
TESTHELPERS = TestHelpers.hs
EXERCISE = Exercise$(LABNO).hs
SOLUTION = Solution$(LABNO).hs

TEMPDIR = /tmp
TMPTESTB = $(TEMPDIR)/$(TESTB)
EDIR = Exercise$(LABNO)

test: testexercise

# Test Exercisen.hs using the test bench.
testexercise: $(TESTB) TestHelpers.hs
	@# Check for the right GHC version.
	@if [ "$$(dnsdomainname)" = "cs.bham.ac.uk" ]; then ghc --version | grep -q 'version 7.[46]' || (ghc --version; echo; echo "Error: we need ghc version 7.6.3 in the lab."; echo; echo "To load the correct version, write this:";echo;echo "    module load ghc/7.6.3";echo;false); fi
	@ghc --version | grep -q 'version 7.[45678]' || (ghc --version; echo; echo "Error: we need ghc >= 7.4.1 ."; false)

	runghc $(TESTB)

$(TMPTESTB):
	(runghc $(TESTGEN) && cat TestBench_appendix.hs) > $(TMPTESTB)

	@echo
	@echo Success. New test bench can be found in $(TMPTESTB).
	@echo
	@echo If you like it, run this:
	@echo
	@echo "    cp $(TMPTESTB) ./"
	@echo
	@echo 'Then, you can run `make test`'.

.PHONY: test testexercise
