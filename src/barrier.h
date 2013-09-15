
#ifndef BARRIER_H
#define BARRIER_H

#include <pthread.h>

// Pthread style barriers for systems that don't support them like Cygwin
// and Mac OS X. Taken from http://www.howforge.com/implementing-barrier-in-pthreads
struct barrier_t
{
    int             needed;
    int             called;
    pthread_mutex_t mutex;
    pthread_cond_t  cond;
};
int barrier_init(barrier_t *barrier, int needed)
{
    barrier->needed = needed;
    barrier->called = 0;
    if (pthread_mutex_init(&barrier->mutex, NULL) != 0)
        return -1;
    if (pthread_cond_init(&barrier->cond, NULL) != 0)
        return -1;
    return 0;
}
int barrier_destroy(barrier_t *barrier)
{
    if (pthread_mutex_destroy(&barrier->mutex) != 0)
        return -1;
    if (pthread_cond_destroy(&barrier->cond) != 0)
        return -1;
    return 0;
}
int barrier_wait(barrier_t *barrier)
{
    pthread_mutex_lock(&barrier->mutex);
    barrier->called++;
    if (barrier->called == barrier->needed)
    {
        barrier->called = 0;
        pthread_cond_broadcast(&barrier->cond);
    }
    else
        pthread_cond_wait(&barrier->cond, &barrier->mutex);
    pthread_mutex_unlock(&barrier->mutex);
    return 0;
}   

#endif // BARRIER_H

