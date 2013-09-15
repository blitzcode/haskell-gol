
# A Haskell 'Game of Life' implementation using Data.Array and GLUT

__UPDATE: See [here][newgol] for a rewrite of this program using Data.Vector / GLFW-b__
[newgol]:https://github.com/blitzcode/haskell-gol-vector-glfwb

![gol](https://raw.github.com/blitzcode/haskell-gol-array-glut/master//screenshot.png)

This repository contains the source code for a simple Game of Life implementation in Haskell using Data.Array for the simulation grid / frame buffer and GLUT for drawing and input handling.

The purpose of this project was to learn more about writing fast Haskell code working with arrays and displaying the results using OpenGL.

Profile run:

    COST CENTRE                    MODULE    %time %alloc

    lookup                         Main       52.5   36.2
    stepGrid.alive.nbList          Main       22.6   38.9
    stepGrid.alive.nb              Main        7.9   12.6
    stepGrid.alive.nbList.wrapGrid Main        5.6    3.0
    stepGrid.alive.countBool       Main        3.8    0.7
    stepGrid.alive                 Main        2.0    0.8
    display                        Main        1.6    3.4
    makeRandomGrid.rndList         Main        1.4    1.1
    stepGrid                       Main        1.1    3.3

~95% of the runtime is the actual GoL simulation, which is good, and more than half is spent on array lookups, which is bad. I get somewhere between 30 and 40 iterations per second on my machines for a 256^2 grid, so it seems we require a different choice of array library to get this to decent performance levels.

GLUT worked OK, but having control over the main loop taken away resulted in having `IORef`s connecting the different parts of the program. I'd probably chose a different OpenGL framework next time.

I hope this serves as a useful example of an OpenGL / array program for Haskell beginners.

# Building

I've only tested on OS X 10.6 with the Haskell Platform 2013.2.0.0, building should succeed with a simple

    $ make

# Legal

This program is published under the [MIT License][mit].

[mit]:http://en.wikipedia.org/wiki/MIT_License

# Author

Developed by Tim C. Schroeder, visit my [website][blitzcode] to learn more.

[blitzcode]:http://www.blitzcode.net
