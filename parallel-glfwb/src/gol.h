
#ifndef GOL_H
#define GOL_H

extern "C" void step_grid(
    int nthreads,
    int nsteps,
    int width,
    int height,
    unsigned char *src,
    unsigned char *dst);

#endif // GOL_H

