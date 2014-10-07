MAIN = TestBench2
OBJ_DIR = obj

all: clean
	ghc -odir $(OBJ_DIR) -hidir $(OBJ_DIR) -main-is $(MAIN) $(MAIN).hs
	./$(MAIN)

clean:
	rm -f $(OBJ_DIR)/* $(MAIN)
