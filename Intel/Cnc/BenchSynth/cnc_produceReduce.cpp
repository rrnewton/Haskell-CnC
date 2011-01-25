#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tbb/tick_count.h>
#include <cnc/cnc.h>

// Turn this on to do a simple test of scheduler/reduction overhead with NOOP tasks.
#define DISABLE_PRIME_CHECK 0

// Create global variables for benchmark dimensions/parameters:
int threads;
int length;
int width;
int flops;

#define DEBUG 0

struct my_context;

struct myStep_t
{
    int execute( int n, my_context & c ) const;
};

int sum_int(int x, int y) {
    return x + y;
}


// typedef double reduc_scalar_t;
#define reduc_scalar_t double


/*
reduc_scalar_t simpleFlops(int flops, reduc_scalar_t input) {
            //  Simple (flops / 2) FLOPs kernel, input i0 output tmp1
            reduc_scalar_t tmp = input;
            for (int i2 = 0; i2 < (flops / 2); i2++)
            {
                tmp = (2.0000001 * tmp);
                tmp = (tmp - input);
            }
            return tmp;
}
*/

// inline
reduc_scalar_t simpleFlops2(int flops, reduc_scalar_t arg0, reduc_scalar_t arg1) 
// reduc_scalar_t simpleFlops2(reduc_scalar_t arg0, reduc_scalar_t arg1) 
{
    // Simple TWO argument flops FLOPs kernel, inputs: arg0 arg1
    tbb::tick_count t0 = tbb::tick_count::now();
    double tmp0 = (arg0 + arg1);

    // for (int j = 0; j < 2; j++)
    // {
    //    for (int i1 = 0; i1 < flops/2; i1++)
    //    {
    //        tmp0 = (0.0000001 + tmp0);
    //    }
    // }

    // for (int i1 = 0; i1 < flops; i1++)
    // {
    //     tmp0 = (0.0000001 + tmp0);
    // }


#pragma simd
    // TEMP: HACK: HANDICAP ADJUSTMENT              
    for (int i1 = 0; i1 < flops / 2.3; i1++)
    {
        tmp0 = (0.0000001 + tmp0);
    }



    tbb::tick_count t1 = tbb::tick_count::now();
    // END TWO argument kernel, output: tmp0
    if (DEBUG) printf("Computed %lf from %lf and %lf in %d flops / %g seconds\n", tmp0, arg0, arg1, flops, (t1-t0).seconds());
    return tmp0;
}

reduc_scalar_t wrapper(reduc_scalar_t arg0, reduc_scalar_t arg1) {
    return simpleFlops2(flops * 60, arg0, arg1);
}


/*
reduc_scalar_t reduce_them(reduc_scalar_t x, reduc_scalar_t y) {
    // WARNING: uses global flops variable:
    reduc_scalar_t result = simpleFlops2(flops, x, y);
    if (DEBUG) printf("  Reduced %lf and %lf down to %lf, flops %d\n", x, y, result, flops);
    return result;
}
*/


struct my_context : public CnC::context< my_context >
{
    CnC::tag_collection< int, CnC::Internal::strided_range< int > > m_tags;
    CnC::step_collection<myStep_t> myStep;
    CnC::eager_reducer<reduc_scalar_t>* reducer;
    CnC::item_collection<int, reduc_scalar_t> m_items; // THIS IS UGLY.

    // Used by the producer to access the result of the previous reduction:
    reduc_scalar_t prev; 
   
    my_context() 
        : CnC::context< my_context >(),
          myStep( this, "myStep_t" ),
          m_tags( this )
        , m_items(this)
    {
        //        reducer = new CnC::eager_reducer<reduc_scalar_t>((reduc_scalar_t)0, &reduce_them,  &m_items);
        reducer = new CnC::eager_reducer<reduc_scalar_t>((reduc_scalar_t)0, &wrapper,  &m_items);
        prescribe( m_tags, myStep );
        produce ( this->m_env, m_tags );
    }
};

// This is the producer.
int myStep_t::execute( int n, my_context & c ) const
{
    int factor = 3;
    bool doput = false;
    
    //    reduc_scalar_t result = simpleFlops2(flops, n, c.prev);

    reduc_scalar_t result;
    // for (int foo=0; foo < 2; foo++) 
   //    if (n == 0)
    if (n == 95)
    {
        printf("SLOW producer...\n");
        result = simpleFlops2(flops * 1000, n, c.prev);
        printf("SLOW finished...\n");
    }
    else
       result = simpleFlops2(flops, n, c.prev);

    if (DEBUG) printf("Producer %d, flops %d, computed %lf from %lf and %lf\n", n, flops, result, c.prev, (double)n);

    c.reducer->put(result);

    return CnC::CNC_Success;
}

int main(int argc, char* argv[])
{
    if ((argc >= 5))
    {
        printf("Reading command line arguments: threads, length, width, op flops\n");
        threads = strtol(argv[1], 0, 10);
        length = strtol(argv[2], 0, 10);
        width = strtol(argv[3], 0, 10);
        flops = strtol(argv[4], 0, 10);
    }
    else
    {
        printf("No command line arguments: using default benchmark params.\n");
        threads = 1;
        length = 10;
        width = 10;
        flops = 10;
    }

    printf("Running pipe of length %d, width %d, op flops %d\n", length
           , width, flops);

    bool verbose = false;
    reduc_scalar_t result = 0;
    my_context c;

    // Initial value for the computation:
    c.prev = 1.0;

    tbb::tick_count t0 = tbb::tick_count::now();

#if 0
    c.m_tags.put_range( CnC::Internal::strided_range< int >( 0, width, 1 ) );
#else
    for(int i=0; i<width; i++)  c.m_tags.put( i );
#endif

    c.wait();

    // Signal that the reduction is finished. (wait() could do this!)
    c.reducer->done();

    // delete c.reducer;

    tbb::tick_count t1 = tbb::tick_count::now();

    // FIXME we have to transfer the items to the host first (distCnC)
    //number_ofr_primes = (int)c.m_primes.size() + 1;

    c.reducer->get(result);

    // Final step after reduction, divide by length:
    result /= (reduc_scalar_t)width;

    printf("Result = %lf, Completed in %g seconds\n", result, (t1-t0).seconds());
}
