
# A Haskell 'Game of Life' implementation using Data.Vector and GLFW-b

![gol](https://raw.github.com/blitzcode/haskell-gol-vector-glfwb/master//screenshot.png)

This repository contains the source code for a rewrite of my [earlier Game of Life implementation][oldgol] in Haskell.
[oldgol]:https://github.com/blitzcode/haskell-gol-array-glut

The purpose of this project was to improve on my first version and learn more about using arrays, OpenGL, concurrency, parallelism, strictness and game / simulation program structure in Haskell. 

Here's a list of improvements over the first version:

* Replaced GLUT with the much nicer GLFW-b library, main loop now remains in Haskell
* Replaced Data.Array with Data.Vector, much more elegant interface, no more Fortran style column-major 2D layout, faster
* Various optimizations make the new version >13x faster
* Now uses an STM TQueue to store and retrieve events coming from the OpenGL wrapper framework
* RWST / ReaderT is now used to keep various bits of state and environment
* Drawing is capped to the screen refresh rate while the actual GoL simulation happens concurrently in a different thread, MVars used for communication
* Frames-per-second and Generations-per-seconds now independently measured
* Since GLFW-b has no build-in font rendering (like GLUT / GLFW) I wrote my own bitmap font renderer
* GoL simulation now only applies the torus array checking at the border for a significant speedup. ST monad and mutable vectors are used to efficiently compose both parts of the grid. Code for neighbour counting and life / death rules has also been made faster and clearer
* Added new 'orientation test' and 'spacefiller' patterns

Profile run:

    COST CENTRE           MODULE    %time %alloc
    
    stepGrid.alive.nb     Main       40.5    0.0
    stepGrid.alive.nbList Main       28.6   68.2
    stepGrid.alive        Main        9.0    6.8
    stepGrid              Main        7.1   11.1
    stepGrid.\            Main        3.9    9.7
    draw.arr              Main        3.1    3.9
    stepGrid.alive.nb.\   Main        2.4    0.0
    draw                  Main        2.3    0.0
    draw.toPixels         Main        1.1    0.0

     9,222,714,516 bytes allocated in the heap
        19,211,828 bytes copied during GC
         1,850,576 bytes maximum residency (109 sample(s))
           245,980 bytes maximum slop
                 7 MB total memory in use (0 MB lost due to fragmentation)
  
                                      Tot time (elapsed)  Avg pause  Max pause
    Gen  0     18112 colls, 18112 par    1.26s    0.69s     0.0000s    0.0100s
    Gen  1       109 colls,   108 par    0.14s    0.06s     0.0005s    0.0116s
  
    Parallel GC work balance: 11.80% (serial 0%, perfect 100%)
  
    TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)
  
    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
  
    INIT    time    0.00s  (  0.00s elapsed)
    MUT     time   17.36s  ( 16.49s elapsed)
    GC      time    1.39s  (  0.74s elapsed)
    RP      time    0.00s  (  0.00s elapsed)
    PROF    time    0.00s  (  0.00s elapsed)
    EXIT    time    0.00s  (  0.00s elapsed)
    Total   time   18.77s  ( 17.24s elapsed)
  
    Alloc rate    531,161,234 bytes per MUT second
  
    Productivity  92.6% of total user, 100.8% of total elapsed

We're up to a respectable 400 gen/sec for a 256^2 grid from previously 30 gen/sec.

Parallelizing the grid simulation and trying the LLVM backend seem like good next steps. The [Repa][repa] (REgular PArallel arrays), [Accelerate][accelerate] (embedded language for high-performance array computations, CUDA backend available) and [Nikolai][nikolai] (first-order language of array computations embedded in Haskell that compiles to GPUs via CUDA) packages are probably worth a look as well.
[repa]:http://repa.ouroborus.net/
[accelerate]:https://github.com/AccelerateHS/accelerate
[nikolai]:https://github.com/mainland/nikola

# Building

I've only tested on OS X 10.6 with the Haskell Platform 2013.2.0.0, building should succeed with a simple

    $ make

You'll also need to have GLFW-b installed from Hackage.

# Legal

This program is published under the [MIT License][mit].

[mit]:http://en.wikipedia.org/wiki/MIT_License

# Author

Developed by Tim C. Schroeder, visit my [website][blitzcode] to learn more.

[blitzcode]:http://www.blitzcode.net
