

#include<tagfun_checking_valid.h>

template < class ctxt > 
int S::execute( const int & tag, ctxt & c) const {
    printf("Woo, exec with tag %d\n", tag);

    // Inexplicable difference... the wrapper class exposes the same put(const int&,
    // double&) interface as the original item collection, and yet we get different error
    // behavior here:
    double d = tag * 3.33;
    //c.I.put(tag+1, d);        // This is ok...
    c.I.put(tag+1, (tag * 3.33)); // This creates an error
// tagfun_checking_valid.cpp:14: error: no matching function for call to 'S_context::I_wrapper::put(int, double)'
// ./tagfun_checking_valid.h:97: note: candidates are: void S_context::I_wrapper::put(const int&, double&)
//
// Is it because the original item_collection class is templated??
// 
//   void put( const Tag & tag, const Item & item, int get_count = -1 );
//   inline void put(const int& tag, double& ref)
//
// Or is it because it's not const?
// Or wait, worse yet, is it because "inline" changes the typechecking behavior!!?
// 

    return CnC::CNC_Success;    
}

int main () {
    printf ("Running with tagfun checking.\n");
    tagfun_checking_valid_context context;

    // Put some tags into the collection in a naive way:
    for(int i=0; i<10; i++)
      context.T.put(i);

    context.wait();

    double fl;
    context.I.get(3, fl);

    printf("Retrieve a single item, index 3: %lf\n", fl);
    return 0;
}
