TESTB = TestBench4

test: Exercise4.hs TestBench4.hs TestHelpers.hs
	@# Test the GHC version.
	@if [ "$(dnsdomainname)" = "cs.bham.ac.uk" ]; then ghc --version | grep -q 'version 7.[46]' || (ghc --version; echo; echo "Error: we need ghc version 7.6.3 in the lab."; echo; echo "To load the correct version, write this:";echo;echo "    module load ghc/7.6.3";echo;false); fi
	@ghc --version | grep -q 'version 7.[45678]' || (ghc --version; echo; echo "Error: we need ghc >= 7.4.1 ."; false)

	runghc $(TESTB).hs

.PHONY: test
