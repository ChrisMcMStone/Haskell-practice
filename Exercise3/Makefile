TESTB = TestBench3
TESTBFILE = $(TESTB).hs
OBJ_DIR = obj
TESTGEN = GenerateTests3

TESTGEN_DEPENDENCIES = $(TESTGEN).hs Exercise3.hs TestBench_appendix.hs TestHelpers.hs

test: clean
	@# If we can run a Haskell file without storing an executable
	@# using runghc, why do we not use it? Well, runghc -XSafe does not work.
	ghc -XSafe -odir $(OBJ_DIR) -hidir $(OBJ_DIR) -main-is $(TESTB) $(TESTB).hs
	./$(TESTB)
	rm $(TESTB)

$(GENMAINFILE):
	runghc -XExistentialQuantification -XTrustworthy $(TESTGEN) > $(TEMPDIR)/$(TESTBFILE)

	cat TestBench_appendix.hs >> $(TEMPDIR)/$(TESTBFILE)

	@echo
	@echo Success. New test bench can be found in $(TEMPDIR)/$(TESTBFILE).
	@echo
	@echo If you like it, run this:
	@echo
	@echo "    cp $(TEMPDIR)/$(TESTBFILE) ./"
	@echo
	@echo 'Then, you can run `make test`'.

clean:
	rm -f $(OBJ_DIR)/* $(TESTGEN)

.PHONY: test newtests clean $(GENMAINFILE)