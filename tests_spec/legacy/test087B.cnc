//Each name starts and ends with '_'
//a starting '_' is omitted

//Is correct: Yes

(_Work_);
(_Start_);
[_type_ _item_ <_int_>];
<_int_ _singleton_>;
<_singleton_> :: (_Start_);
<_int_ _worker_>;
<_worker_> :: (_Work_);
env -> <_singleton_>;
(_Start_) -> [_item_];
(_Start_) -> <_worker_>;
(_Work_) -> [_item_];
[_item_] -> env; 
