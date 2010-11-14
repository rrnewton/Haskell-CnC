//********************************************************************************
// Copyright (c) 2007-2010 Intel Corporation. All rights reserved.              **
//                                                                              **
// Redistribution and use in source and binary forms, with or without           **
// modification, are permitted provided that the following conditions are met:  **
//   * Redistributions of source code must retain the above copyright notice,   **
//     this list of conditions and the following disclaimer.                    **
//   * Redistributions in binary form must reproduce the above copyright        **
//     notice, this list of conditions and the following disclaimer in the      **
//     documentation and/or other materials provided with the distribution.     **
//   * Neither the name of Intel Corporation nor the names of its contributors  **
//     may be used to endorse or promote products derived from this software    **
//     without specific prior written permission.                               **
//                                                                              **
// This software is provided by the copyright holders and contributors "as is"  **
// and any express or implied warranties, including, but not limited to, the    **
// implied warranties of merchantability and fitness for a particular purpose   **
// are disclaimed. In no event shall the copyright owner or contributors be     **
// liable for any direct, indirect, incidental, special, exemplary, or          **
// consequential damages (including, but not limited to, procurement of         **
// substitute goods or services; loss of use, data, or profits; or business     **
// interruption) however caused and on any theory of liability, whether in      **
// contract, strict liability, or tort (including negligence or otherwise)      **
// arising in any way out of the use of this software, even if advised of       **
// the possibility of such damage.                                              **
//********************************************************************************
//

// Tag declarations : <tag-type tag-name>

// type pair = tuple<int,int>
// #define pair tuple<int,int>

// position is indexed by the x and y axis of the complex plane

// How should we do comprehensions?  Or provide some way to specify tag domains exactly.
tags< (int,int) > position;  // = [ (x,y) | x <- [1..N], y <- [1..M]]
steps compute;



// Item declarations : items<tag-type, item-type>
items< (int,int), complex> data; // data is indexed by the x and y axis of the complex plane 
items< (int,int), int> pixel;    // pixel is indexed by the x and y axis of the complex plane
items<int, int> max_depth; // max_depth is the number of iterations. It is a singleton

// Prescriptive relations
position prescribes compute;

// Consumer/Producer releations
data[p], max_depth[0] -> compute(p) -> pixel[p];

// Input from the environment and output to the environment
env -> position, data[], max_depth[];
pixel[] -> env;
