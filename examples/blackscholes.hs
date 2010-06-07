{-# LANGUAGE RecordWildCards  #-}
-- Intel Concurrent Collections for Haskell
-- Copyright (c) 2010, Intel Corporation.
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms and conditions of the GNU Lesser General Public License,
-- version 2.1, as published by the Free Software Foundation.
--
-- This program is distributed in the hope it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
-- more details.
--
-- You should have received a copy of the GNU Lesser General Public License along with
-- this program; if not, write to the Free Software Foundation, Inc., 
-- 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.

-- Ported from CnC/C++ program by Ryan Newton

-- Description
-- ===========

-- The Black-Scholes equation is a differential equation that describes how,
-- under a certain set of assumptions, the value of an option changes as the 
-- price of the underlying asset changes.

-- The formula for a put option is similar. The cumulative normal distribution 
-- function, CND(x), gives the probability that normally distributed random
-- variable will have a value less than x. There is no closed form expression for
-- this function, and as such it must be evaluated numerically. The other 
-- parameters are as follows: S underlying asset's current price, 
-- X the strike price, T time to the expiration date, r risk-less rate of return,
-- and v stock's volatility.

-- Blackscholes has a single step collection called (compute). It is executed once
-- for each tag in the tag collection <tags>. This tag collection is produced
-- by the environment.

-- (compute) inputs [data] items. Each [data] item contains a pointer to an 
-- array of ParameterSet objects. For each ParameterSet object the Black-Scholes equation 
-- is solved, and results are stored in the emitted items [price].

-- To reduce the influence of the serial part each calculation is repeated for NUM_RUNS times.

-- There are no constraints on the parallelism in this example. An instance of
-- (compute) does not rely on a data item from another instance and it does not
-- relay on a control tag from another instance. All the tags are produced by
-- the environment before the program begins execution. All steps can be executed
-- in parallel if there are enough processors.


-- Usage
-- =====

-- The command line is:

-- blackscholes n b t
--     n  : positive integer for the number of options
--     b  : positive integer for the size of blocks
--     t  : positive integer for the number of threads
    
-- e.g.
-- blackscholes 100000 100 4

import Intel.Cnc
--import Intel.CncUtil
import Control.Monad
--import Data.Array
import Data.Array.Unboxed
--import Data.Array.IArray

import System.Environment

-- #define fptype float
-- #define ERR_CHK
num_runs = 100

type FpType = Float

data OptionData = OptionData {
	s       :: FpType,              -- spot price
	strike  :: FpType,		-- strike price
	r       :: FpType ,	        -- risk-free interest rate
	divq    :: FpType ,	        -- dividend rate
	v       :: FpType ,		-- volatility
	t       :: FpType ,		-- time to maturity or option expiration in years 
				       	--     (1yr = 1.0, 6mos = 0.5, 3mos = 0.25, ..., etc)  
	optionType :: String,	-- Option type.  "P"=PUT, "C"=CALL
	divs :: FpType,		-- dividend vals (not used in this test)
	dGrefval :: FpType	-- DerivaGem Reference Value
} deriving Show

data ParameterSet =  ParameterSet {
	dat :: OptionData,
	sptprice   :: UArray Int FpType,
	strikep    :: UArray Int FpType,
	rate       :: UArray Int FpType,
	volatility :: UArray Int FpType ,
	otime      :: UArray Int FpType,
	otype      :: UArray Int Bool,
	granularity :: Int 
} deriving Show

 -- Big constant array:
--data_init :: Array Int Int
data_init :: Array Int OptionData
--data_init = listArray (1,3) [undefined, undefined, undefined]
-- This defines some hard coded data as a big constant array:
#include "blackscholes_data.hs"

size_init = let (s,e) = bounds data_init in e - s + 1

inv_sqrt_2xPI = 0.39894228040143270286

cndf :: FpType -> FpType
cndf inputX = if sign then 1.0 - xLocal else xLocal
  where 
    sign = inputX < 0.0
    inputX' = if sign then -inputX else inputX
    
    -- Compute NPrimeX term common to both four & six decimal accuracy calcs
    xNPrimeofX = inv_sqrt_2xPI * exp(-0.5 * inputX * inputX);

    xK2 = 1.0 / (0.2316419 * inputX + 1.0);    
    xK2_2 = xK2   * xK2; -- Need all powers of xK2 from ^1 to ^5:
    xK2_3 = xK2_2 * xK2;
    xK2_4 = xK2_3 * xK2;
    xK2_5 = xK2_4 * xK2;
    
    xLocal   = 1.0 - xLocal_1 * xNPrimeofX;
    xLocal_1 = xK2   *   0.319381530  + xLocal_2;
    xLocal_2 = xK2_2 * (-0.356563782) + xLocal_3 + xLocal_3' + xLocal_3'';
    xLocal_3   = xK2_3 * 1.781477937;
    xLocal_3'  = xK2_4 * (-1.821255978);
    xLocal_3'' = xK2_5 * 1.330274429;


blkSchlsEqEuroNoDiv :: FpType -> FpType -> FpType -> FpType -> FpType -> Bool -> Float -> FpType
blkSchlsEqEuroNoDiv sptprice strike rate volatility time otype timet =
   if not otype
   then (sptprice * nofXd1) - (futureValueX * nofXd2)
   else let negNofXd1 = 1.0 - nofXd1
	    negNofXd2 = 1.0 - nofXd2
	in (futureValueX * negNofXd2) - (sptprice * negNofXd1)
 where 
   logValues  = log( sptprice / strike )                
   xPowerTerm = 0.5 * volatility * volatility
   xDen = volatility * sqrt(time)
   xD1  = (((rate + xPowerTerm) * time) + logValues) / xDen
   xD2  = xD1 -  xDen

   nofXd1 = cndf xD1 
   nofXd2 = cndf xD1    
   futureValueX = strike *  exp ( -(rate) * (time) )


-- executeStep :: (Ord tag, Show tag) => 
--                ItemCol tag ParameterSet -> 
-- 	       ItemCol tag [UArray Int FpType] ->
-- 	       tag -> StepCode () 

executeStep :: ItemCol Int ParameterSet -> 
	       ItemCol Int [UArray Int FpType] ->
	       Int -> StepCode () 
executeStep params prices t = 
-- int compute::execute(const int & t, blackscholes_context& c ) const
    do ParameterSet { .. } <- get params t  
--    do --let OptionData { .. } = data_init ! (t `mod` size_init)
       --stepPutStr$ "YAY got options, strike: " ++ show strike ++ "\n"
       let ls = map (\ j -> 
                     listArray (0, granularity-1) $
--		        map (\i -> blkSchlsEqEuroNoDiv (sptprice!i) (strikep!i) (rate!i)
--                                                  (volatility!i) (otime!i) (otype!i)  0) 
		        map (\i -> 
                              let OptionData { .. } = data_init ! (t+i `mod` size_init)
			      in blkSchlsEqEuroNoDiv sptprice strikep rate volatility otime otype 0)
		            [0 .. granularity-1])
	         [0 .. num_runs-1]
       put prices t ls;

-- #ifdef ERR_CHK   
--             fptype priceDelta = data[i].DGrefval - prices[i];
--             if(fabs(priceDelta) >= 1e-4){
-- 				printf("Error on %d:%d. Computed=%.5f, Ref=%.5f, Delta=%.5f\n",
--                        int(t), i, prices[i], data[i].DGrefval, priceDelta);
--             }
-- #endif 

init numOptions granularity tags items = 
  do let (quot,rem) = numOptions `quotRem` granularity
	 len = quot + (if rem > 0 then 1 else 0)
     --forM_ [0 .. len-1] $ \i ->
     forM_ [0, granularity .. numOptions] $ \loopnum ->
        do put items loopnum undefined
           putt tags loopnum 


-- blackscholes n b t
--     n  : positive integer for the number of options
--     b  : positive integer for the size of blocks
--     t  : positive integer for the number of threads


--graph :: Int -> Int -> GraphCode FpType
graph numOptions granularity = 
   do tags  <- newTagCol
      params <- newItemCol
      prices <- newItemCol

      prescribe tags (executeStep params prices)

      initialize$ 
        cncFor 1 numOptions (putt tags)
--        cncFor 1 numOptions $ \i -> 
--          do put params i $ data_init ! (i `mod` size_init)

      finalize$ get prices 0

main = do args <- getArgs 
          let (numOptions, granularity, nThreads) =
               case args of 
  	         []      -> (10, 10, -1)
--	         [n,b,t] -> (read n, read b, read t)

	  putStrLn$ "Running blackscholes..."	  
--          result <- runGraph $ graph numOptions granularity
	  let result  = 99
          putStrLn$ "Final result: "++ show result
	    


#if 0

--initialize items and tags
-- void init(blackscholes_context& c, int numOptions, int granularity, OptionData* data,
-- 	  fptype* sptprice, fptype* strike, fptype* rate, fptype* volatility, fptype* otime, int* otype){

--         ParameterSet* ps = new ParameterSet[]
-- 	for (int counter = 0, loopnum = 0 loopnum < numOptions loopnum += granularity, counter++){
-- 		int tag = loopnum
-- 		ps[counter].data		= &data[loopnum]
-- 		ps[counter].sptprice	= &sptprice[loopnum]
-- 		ps[counter].strike		= &strike[loopnum]
-- 		ps[counter].rate		= &rate[loopnum]
-- 		ps[counter].volatility	= &volatility[loopnum]
-- 		ps[counter].otime		= &otime[loopnum]
-- 		ps[counter].otype		= &otype[loopnum]
-- 		ps[counter].granularity = (numOptions - loopnum >= granularity) ? granularity : numOptions - loopnum
-- 		c.data.put(tag, &ps[counter])
-- 		c.tags.put(tag)
--     }
-- }

int main (int argc, char **argv){

	printf("Number of Options: %d\n", numOptions)
	printf("Number of Runs: %d\n", NUM_RUNS)
	printf("Granularity: %d\n", granularity)

	-- initialize the data array
	OptionData* data = new OptionData[numOptions]
	int initOptionNum =  ( (sizeof(data_init)) / sizeof(OptionData) )
	for ( int loopnum = 0 loopnum < numOptions loopnum++ )
	{
		OptionData* temp = data_init + loopnum % initOptionNum
		data[loopnum].OptionType = new char[strlen(temp->OptionType)+1]
		strcpy(data[loopnum].OptionType, temp->OptionType)
		data[loopnum].s = temp->s
		data[loopnum].strike = temp->strike
		data[loopnum].r = temp->r
		data[loopnum].divq = temp->divq
		data[loopnum].v = temp->v
		data[loopnum].t = temp->t
		data[loopnum].divs = temp->divs
		data[loopnum].DGrefval = temp->DGrefval
	}

#define PAD 256
#define LINESIZE 64

    --copy all option data into a single array
	fptype* buffer = (fptype *) malloc(5 * numOptions * sizeof(fptype) + PAD)
	fptype* sptprice = (fptype *) (((unsigned long long)buffer + PAD) & ~(LINESIZE - 1))
	fptype* strike = sptprice + numOptions
	fptype* rate = strike + numOptions
	fptype* volatility = rate + numOptions
	fptype* otime = volatility + numOptions
    
	int* buffer2 = (int *) malloc(numOptions * sizeof(fptype) + PAD)
	int* otype = (int *) (((unsigned long long)buffer2 + PAD) & ~(LINESIZE - 1))

	for (int i = 0 i < numOptions i++) {
		otype[i]      = (!strcmp(data[i].OptionType, "P")) ? 1 : 0
		sptprice[i]   = data[i].s
		strike[i]     = data[i].strike
		rate[i]       = data[i].r
		volatility[i] = data[i].v    
		otime[i]      = data[i].t
	}

    -- Set the number of threads for the execution of the CnC graph
    if (nThreads > 0) {  -- If specified by user
        CnC::debug::set_num_threads(nThreads)
    }

    -- Create an instance of the context class which defines the graph
    blackscholes_context c

    init(c, numOptions, granularity, data, sptprice, strike, rate, volatility, otime, otype)

    -- Wait for all steps to finish
    c.wait()

	for (int loopnum = 0 loopnum < numOptions loopnum++){
		delete[] data[loopnum].OptionType
    }
    delete[] data
	free(buffer)
	free(buffer2)
	--free an array of ParameterSets
    ParameterSet* tmpdata
    c.data.get(0, tmpdata)
    delete[] tmpdata

	tbb::tick_count t1 = tbb::tick_count::now()
	printf("Time taken: %.3lf\n", (t1-t0).seconds())
    return 0
}


#endif

