
#include "gol.h"
#include "barrier.h"

#include <cassert>
#include <vector>

void step_grid_interior_segment(
    int            width,
    int            height,
    unsigned char *src,
    unsigned char *dst,
    int            seg_low,
    int            seg_high)
{
    // Process a vertical segment of the grid interior

    // Interior: No wrapping, 1D indexing
    for (int y=seg_low; y<seg_high; y++)
        for (int x=1; x<width - 1; x++)
        {
            const int idx = x + y * width;
            unsigned int nb = 0;
            nb = src[idx + 1        ] +
                 src[idx - 1        ] +
                 src[idx     + width] +
                 src[idx     - width] +
                 src[idx + 1 + width] +
                 src[idx + 1 - width] +
                 src[idx - 1 + width] +
                 src[idx - 1 - width];

            dst[idx] = (nb == 3) || (src[idx] && nb == 2);
        }
}

struct param
{
    int            nsteps;
    int            width;
    int            height;
    unsigned char *src;
    unsigned char *dst;
    int            seg_low;
    int            seg_high;
    barrier_t     *barrier;
};

void * thread_entry(void *arg)
{
    // Do N steps of a single segment

    param *p = reinterpret_cast<param *> (arg);

    // We can do multiple simulation steps to compensate for call and data conversion
    // overhead on the Haskell side
    for (int step=0; step<p->nsteps; step++)
    {
        step_grid_interior_segment(p->width, p->height, p->src, p->dst, p->seg_low, p->seg_high);

        // Synchronize after each step and then swap buffers
        barrier_wait(p->barrier);
        std::swap(p->src, p->dst);
    }

    return nullptr;
}

void step_grid(int nthreads, int nsteps, int width, int height, unsigned char *src, unsigned char *dst)
{
    // Need an odd number for the result to end up in dst after the array ping pong
    assert(nsteps % 2 == 1);

    int ret;

    barrier_t barrier;
    ret = barrier_init(&barrier, nthreads + 1);
    assert(ret == 0);

    // Create and launch threads for interior processing, split grid into segments
    std::vector<pthread_t> threads(nthreads);
    std::vector<param>     params (nthreads);
    for (int i=0; i<nthreads; i++)
    {
        params[i].nsteps = nsteps;
        params[i].width  = width;
        params[i].height = height;
        params[i].src    = src;
        params[i].dst    = dst;

        const int range = (height - 2) / nthreads;
        params[i].seg_low = 1 + range * i;
        if (i == nthreads - 1)
            params[i].seg_high = height - 1;
        else
            params[i].seg_high = 1 + range * (i + 1);

        params[i].barrier = &barrier;
        ret = pthread_create(&threads[i], NULL, thread_entry, &params[i]);
        assert(ret == 0);
    }

    // While the threads do the interior, we'll be doing the border
    for (int step=0; step<nsteps; step++)
    {
        if (nthreads == 0)
        {
            // No threads, we need to do the interior
            step_grid_interior_segment(width, height, src, dst, 1, height - 1);
        }

        // Torus array border: Wrapping, 2D indexing
        for (unsigned int b=0; b<4; b++)
        {
            int x, y, end;
            int *incr = nullptr;

            switch (b)
            {
                case 0: x = 0;         y = 0;          incr = &x; end = width;  break; // Top
                case 1: x = 0;         y = height - 1; incr = &x; end = width;  break; // Bottom
                case 2: x = 0;         y = 0;          incr = &y; end = height; break; // Left
                case 3: x = width - 1; y = 0;          incr = &y; end = height; break; // Right
            }

            for (; (* incr) < end; (* incr)++)
            {
                const int offs[8][2] =
                {
                    { x + 1, y     }, { x,     y + 1 },
                    { x - 1, y     }, { x,     y - 1 },
                    { x + 1, y + 1 }, { x - 1, y - 1 },
                    { x + 1, y - 1 }, { x - 1, y + 1 }
                };

                unsigned int nb = 0;
                for (unsigned int i=0; i<8; i++)
                {
                    int ox = offs[i][0], oy = offs[i][1];
                    if      (ox < 0)          ox += width;
                    else if (ox > width  - 1) ox -= width;
                    if      (oy < 0)          oy += height;
                    else if (oy > height - 1) oy -= height;
                    nb += src[ox + oy * width];
                }

                const unsigned int idx = x + y * width;
                dst[idx] = (nb == 3) || (src[idx] && nb == 2);
            }
        }

        // Synchronize after each step and then swap buffers
        barrier_wait(&barrier);
        std::swap(src, dst);
    }

    // Wait for all threads to be done with the last segment
    for (int i=0; i<nthreads; i++)
    {
        ret = pthread_join(threads[i], NULL);
        assert(ret == 0);
    }

    ret = barrier_destroy(&barrier);
    assert(ret == 0);
}

