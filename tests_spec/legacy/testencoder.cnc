//********************************************************************************
// Copyright (c) 2007-2008 Intel Corporation. All rights reserved.              **
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

// Declarations

//large blocks of data, are used on the 2nd stage
[data_chunk* block : int blockID];
<blockTag : int blockID>;

//small data pieces, are used on the 3rd stage
[data_chunk* item : int anchorid, int reassembleid];
<itemTag : int anchorid, int reassembleid>;

//small data pieces for the 4th stage
[send_buf_item* itemToCompress: int anchorid, int reassembleid];
<itemToCompressTag : int anchorid, int reassembleid>;

//small data pieces for the 5th stage
[send_buf_item* itemToSend: int anchorid, int reassembleid];
<sendingTag : int anchorid, int reassembleid>;

//starts the pipeline execution, contains input file descriptor
<inputFileTag : int fileID>;
//output file descriptor
[int outputFile];
//size of blocks
[int blockSize];
//number of small data chunks in a large data block
[int chuncksPerAnchor : int chuncksPerAnchorID];

//a global hash table, different items for different states
[hash_entry* hash : int k1, int k2, int k3, int k4, int k5];
[hash_entry* hashToSend : int k1, int k2, int k3, int k4, int k5];
[hash_entry* hashWritten : int k1, int k2, int k3, int k4, int k5];
//always GZIP
[int compressWay : int compressWayID];

//inputs from the environment
env -> <inputFileTag>, [outputFile], [blockSize], [compressWay];

//The 1st stage, divide the input stream into large data blocks
<inputFileTag> :: (DataProcess);
[blockSize] -> (DataProcess) -> [block], <blockTag>;

//The 2nd stage, dividing blocks into small data chunks
<blockTag> :: (FindAllAnchors);
[block] -> (FindAllAnchors) -> [item], <itemTag>, [chuncksPerAnchor];

//The 3rd stage, computing hashes for data chunks
<itemTag> :: (ChunkProccess);
[item], [hash] -> (ChunkProccess) -> [itemToCompress], <itemToCompressTag>, [itemToSend], [hash];

//The 4th stage, compressing data chunks
<itemToCompressTag> :: (Compress);
[itemToCompress], [compressWay] -> (Compress) -> [itemToSend], [hashToSend];

//The 5th stage, serial. Assembling compressed chunks into an output file
<sendingTag> :: (SendBlock);
[itemToSend], [outputFile], [chuncksPerAnchor], [hashToSend], [hashWritten] -> (SendBlock) -> [hashWritten], <sendingTag>;
