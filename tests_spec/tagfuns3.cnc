

// Tag functions can be bounded.
// items and tags can be dense.
// Dense & bounded is not a GUARANTEE of total coverage in that interval.

tags <(dense int,int)>      T;
items<dense (int,int), Foo> I;

//tags <(int,int)>      T with dense = true;
//items<(int,int), Foo> I with dense = true;

//type densetup = dense tuple<int,int>;

//Tags <dense (int,int)>      T;
//Items<(dense int,int), Foo> I;

// Tags <dense tuple<int,int> >     T;
// Items<tuple<dense int,int>, Foo> I;

//tags <dense< tuple<int,int> > >      T;
//items<dense< tuple<int,int> >, Foo>  I;

steps                 S;

T prescribes S; 
