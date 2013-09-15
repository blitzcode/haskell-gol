
#
# We're mostly relying on GHC's make mode, but the presence of some C++ and
# Template Haskell is making things a little bit trickier
#

# Default target depending on main program
.PHONY : all
all: out

WARN_FLAGS = -Wall
LLVM_FLAGS = -fllvm -optlo-O3
# REPA_FLAGS = -Odph -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000
# Run with '+RTS -p -s'
# PROF_FLAGS = -prof -fprof-auto -DNO_TH
GHC_FLAGS = $(PROF_FLAGS) $(WARN_FLAGS) $(LLVM_FLAGS) -rtsopts -funbox-strict-fields -O2 -threaded -cpp -H512m

# We assume all .cpp files in the directory are meant to be compiled
CXXSRC = $(wildcard *.cpp)
CXXOBJ = $(CXXSRC:.cpp=.o)

# Rule for producing the main program
HSSRC = $(wildcard *.hs)
out: $(HSSRC) Grid.o $(CXXOBJ)
	ghc $(GHC_FLAGS) -o $@ -lstdc++ --make Main $(CXXOBJ)

# The Repa code in Grid uses Template Haskell and needs to be loaded by the GHC byte code
# interpreter. It unfortunately trips up when it tries to load libstd++, which we need for
# linking in the C++ GoL code. Compile grid separately here to avoid this
#
# TODO: Might want to use 'ghc -M' to recompile Grid if one of its dependencies changes
Grid.o : Grid.hs
	ghc $(GHC_FLAGS) $(REPA_FLAGS) -c Grid.hs

.PHONY : clean
clean:
	$(RM) $(HSSRC:.hs=.hi) $(HSSRC:.hs=.o) $(CXXOBJ) $(CXXOBJ:.o=.d) out

#
# C++ compilation rules beyond this point
#

# We use LLVM as our GHC backend, use Clang for the C++ sources
CXX = clang++
CXXFLAGS = -std=c++11 -g -m32 -O3 -mtune=native -Wall -Wextra -Wno-unused-parameter

# Only include this if we actually build the main program, don't want to
# generate dependencies for the clean targets etc.
ifeq ($(MAKECMDGOALS), )

# Automatically generate dependencies with the compiler for each C++ source file
%.d: %.cpp
	@set -e; rm -f $@; \
	 $(CXX) -MM $(CXXFLAGS) $< > $@.$$$$; \
	 sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	 $(RM) $@.$$$$

# Include the generated dependencies for object and dependency files. Be silent
# during the first compile where the .d files have not been generated yet and
# everything is recompiled
-include $(CXXSRC:.cpp=.d)

endif

