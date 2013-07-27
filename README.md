
# A parallel Haskell 'Game of Life' implementation

![gol](https://raw.github.com/blitzcode/haskell-gol-parallel-glfwb/master/screenshot.png)

This repository contains the source code for a parallel version of my [earlier Game of Life implementation][oldgol] in Haskell.
[oldgol]:https://github.com/blitzcode/haskell-gol-vector-glfwb

The purpose of this project was to try different approaches to parallelism in Haskell and see how they work out for this type of program. I've also made a number of other improvements over the predecessor. A GPU version using [Accelerate][accelerate] or [Nikola][nikola] seems like an interesting idea, but I haven't done any work on it so far.
[accelerate]:http://hackage.haskell.org/package/accelerate
[nikola]:http://www.eecs.harvard.edu/~mainland/projects/nikola/

Note that there are various 'Tweak Me' comments throughout the program that might require adjustment for optimum performance over a range of system configurations.

### LLVM

GHC's LLVM backend often generates faster code for numeric computation and Vector operations. It is also targeted by the Repa library, so a logical first step was to build the program using LLVM. Using the [3.3 release of LLVM / Clang][llvm33] built from source gives a speed increase of ~100 gen/sec to ~600 gen/sec for the basic serial Vector code on the dual core laptop.
[llvm33]:http://llvm.org/releases/download.html#3.3

### Async

Starting from the original serial stepGrid function, splitting the grid into even vertical segments and distributing the work using [Control.Concurrent.Async][async] brings us to ~1050 gen/sec on my dual core laptop and ~2000 gen/sec on my quad core desktop. Fast and rather simple.
[async]:http://hackage.haskell.org/packages/archive/async/2.0.0.0/doc/html/Control-Concurrent-Async.html

### C++ / pthreads

A reasonably optimized, serial C++ implementation of stepGrid compiled with Clang gets ~1350 gen/sec on the dual core laptop and ~2950 gen/sec on the quad core desktop machine. The function does multiple steps at once to hide the array conversion overhead (Unboxed Bool Vector into Storable Word8 Vector) as much as possible. The parallel version uses plain pthreads (OpenMP is not available on Clang, had issues linking gcc C++ into my LLVM Haskell binary) and gets ~2350 gen/sec and ~6650 gen/sec on the dual / quad core machines. Note that even the serial C++ version beats all the different parallelized Haskell codes on both machines.

### Repa

The program contains four different versions of stepGrid using [Repa][repa].
[repa]:http://repa.ouroborus.net/

I found Repa interesting, but difficult to work with. Simple things like the torus array were rather tricky to implement, and the actual code ended up being slower and more complicated than a straightforward approach with Vector + Async. The tutorials in the parallel Haskell book or on the Wiki do not explain advanced topics like cursors, partitions and stencils. With sparse Hackage documentation and papers describing an older version of the library, the last resort was to simply read the Repa code (which was rather nice, actually). Haskell performance is often fragile and difficult to reason about, but I found writing the Repa versions of the GoL code to be particularly hard. The library relies heavily on various compiler optimizations, and getting just about anything wrong regarding inlining, strictness, ordering etc. seems to reduce performance tenfold with no easy way to diagnose the issue for a novice Haskell programmer.

#### Traverse

There's a `traverse` / `unsafeTraverse` based version which is very slow (~80 gen/sec dual core laptop) as there is no good way to apply different border / non-border versions of the code or compose two computations through mutable arrays.

#### Static Stencil

Taking a note from [this][stencil] blog post, I tried a stencil based version. It runs at ~700 gen/sec on the dual core laptop and ~1850 gen/sec on the quad core desktop. Repa's static stencil code can not support the torus array boundary condition, though.
[stencil]:http://www.tapdancinggoats.com/haskell-life-repa.htm

#### Convolve

Repa's algorithm package has a more flexible stencil implementation which can support torus arrays, but the performance is far worse. Getting ~165 gen/sec on the dual core laptop and ~430 gen/sec on the quad core desktop.

#### Partitioned Arrays

Repa's partitioned array representation allows us to efficiently split the array into inner and boundary regions. Getting ~700 gen/sec on the dual core laptop and ~1850 gen/sec on the quad core desktop, it is just as efficient as the stencil version while correctly implementing the torus array. The code is actually rather similar to Repa's own mapStencil2, I don't see a reason why that function couldn't support more flexible boundary conditions.

# TODO & Disclaimer

I was able to reach my goal of writing several parallel versions of the original code, resulting in significant speedup.

Still, there's likely plenty to improve in this program. Quite some tweaking could be done regarding GC. I often see large fluctuations in performance over time. Running the single threaded C++ code with a large grid results in CPU usage of 300% - 500%, with all Haskell threads primarily doing GC. Profiling with `+RTS -s` shows ~1% time spent in GC, but when running a native code profiler on the program it's clear there are several GC worker threads running. I probably made some big blunders regarding optimization. I frequently stumble on innocent looking little changes that give a significant speedup. I might have gotten the actual timing code wrong (laziness can be a challenge). There are probably several more places where I should have introduced strict evaluation or inline pragmas. Unlike the pthreads in the C++ code, the Haskell threads seem to idle an awful lot even when seem to have plenty of unblocked work to do.

# Building

I've only tested on OS X 10.6 with the Haskell Platform 2013.2.0.0, building should succeed with a simple

    $ make

For compilation `GLFW-b`, `repa` and `repa-algorithms` from Hackage need to be installed. LLVM and Clang also need to be available, I tested with the 3.3 release built from source.

# Legal

This program is published under the [MIT License][mit].

[mit]:http://en.wikipedia.org/wiki/MIT_License

# Author

Developed by Tim C. Schroeder, visit my [website][blitzcode] to learn more.

[blitzcode]:http://www.blitzcode.net

