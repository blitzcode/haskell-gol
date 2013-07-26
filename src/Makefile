
# Run with '+RTS -p -s'
# PROF_FLAGS = -prof -fprof-auto

WARN_FLAGS = -Wall

.PHONY : all
all:
	ghc $(PROF_FLAGS) $(WARN_FLAGS) -funbox-strict-fields -O2 -threaded -o out --make Main

.PHONY : clean
clean:
	rm *.hi *.o out
