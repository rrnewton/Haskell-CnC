// Eigensolver textual notation 


// Declarations
[double* Lijk_space <triple>: int, int, int];        // The input positive definite matrix
[double* Aijk_space <quadruple>: int, int, int, int];		 
[double* Adlatrd_space <triple>: int, int, int];
[double* W <pair>: int, int];
[double* E <int>];
[double* D <int>];
[double tau <pair>: int, int];
[double E_temp <pair>: int, int];
[double* A_tempspace <quadruple>: int, int, int, int]; // temp
[double* Wwork <triple>: int, int, int];
[double* W_th_space <int>];
[double* Y_th_space <quadruple>: int, int, int, int];
[double* Y_th <int>];
[double alpha <pair>: int, int];
[int num_threads <int>];
[double thread_col_size <int>];

[int p <int>];             // Tile loop end value
[int b <int>];			 // Tile size 
[int n <int>];			 // Matrix size

// Tags
<int singleton>;
<int control_S1_space>;       // Tag values are indices of Step 1 [k = 0...p-1]
<pair control_S2_space: int, int>;	// Tag values are indices of Step 2 [j = k+1...p-1]
<triple control_S3_space: int, int, int>;	// Tag values are indices of Step 3 [i = k+1...j]
<int dsygs2_t_tags: int>;
<pair dsymm_t_tags: int, int>;
<pair dsymma_tags: int, int>;
<triple dsyr2k_tags: int, int, int>;
<pair dgemv1_tags: int, int>;
<pair dgemv2_tags: int, int>;
<pair dlarf_tags: int, int>;
<triple dsymv_tags: int, int, int>;
<pair dsymv_check_tags: int, int>;
<pair dgemv3_tags: int, int>;
<pair dgemv4_tags: int, int>;
<pair dgemv5_tags: int, int>;
<pair dgemv6_tags: int, int>;
<pair dscal_tags: int, int>;
<pair daxpy_tags: int, int>;
<pair dot_tags: int, int>;
<triple dsyr2kAW_tags: int, int, int>;
<int dsterf_tags>;

// Step Prescriptions
<singleton> :: (dsytd2_t);		
<control_S1_space> :: (S1_compute),(dtrsml);		
<control_S2_space> :: (S2_compute),(dtrsm_t);
<control_S3_space> :: (S3_compute);
<dsygs2_t_tags> :: (dsygs2_t);
<dsymm_t_tags> :: (dsymm_t);
<dsymma_tags> :: (dsymma);
<dsyr2k_tags> :: (dsyr2k_c);
<dgemv1_tags> :: (dgemv1);
<dgemv2_tags> :: (dgemv2);
<dlarf_tags> :: (dlarf_c);
<dsymv_tags> :: (dsydmv);
//<dsymv_tags> :: (dsymv_reduction);
<dsymv_check_tags> :: (dsymv_check);
<dgemv3_tags> :: (dgemv3);
<dgemv4_tags> :: (dgemv4);
<dgemv5_tags> :: (dgemv5);
<dgemv6_tags> :: (dgemv6);
<dscal_tags> :: (dscalar);
<dot_tags> :: (dot);
<daxpy_tags> :: (ddaxpy);
<dsyr2kAW_tags> :: (dsyr2kAW);
<dsterf_tags> :: (dsterf_t);

// Input from the caller: tile pointers, tile size and loop end value
env -> [Lijk_space], [Aijk_space], [p: sTag], [b: sTag], [n: sTag], [num_threads], <singleton>, <control_S1_space>, [Y_th], [thread_col_size];

// S1_compute I/O
[Lijk_space],
[b: sTag],
[p: sTag],
[n: sTag] -> (S1_compute);

(S1_compute) -> [Lijk_space], <control_S2_space>, <dsygs2_t_tags>;

// S2_compute I/O
[Lijk_space],
[b: sTag],
[p: sTag],
[n: sTag] -> (S2_compute);

(S2_compute) -> [Lijk_space], <control_S3_space>;

// S3_compute I/O
[Lijk_space],
[b: sTag],
[n: sTag] -> (S3_compute);

(S3_compute) -> [Lijk_space];
 
// dsygs2_t I/O
[Lijk_space], 
[Aijk_space],
[b: sTag],
[n: sTag] -> (dsygs2_t);

(dsygs2_t) -> [Aijk_space];

// dtrsm_t I/O
[Lijk_space],
[Aijk_space],
[b: sTag],
[n: sTag] -> (dtrsm_t);

(dtrsm_t) -> [Aijk_space], <dsymm_t_tags>;

// dsymm_t I/O
[Lijk_space],
[Aijk_space],
[b: sTag],
[n: sTag] -> (dsymm_t);

(dsymm_t) -> [Aijk_space], [A_tempspace], <dsymma_tags>, <dsyr2k_tags>;

// dsyr2k I/O
[Lijk_space],
[Aijk_space],
[A_tempspace],
[b: sTag],
[n: sTag] -> (dsyr2k_c);

(dsyr2k_c) -> [Aijk_space];

// dsymma I/O
[Lijk_space],
[Aijk_space],
[b: sTag],
[n: sTag] -> (dsymma);

(dsymma) -> [Aijk_space];

// dtrsml I/O
[Lijk_space],
[Aijk_space],
[b: sTag],
[p: sTag],
[n: sTag] -> (dtrsml);

(dtrsml) -> [Aijk_space], <dgemv1_tags>;

// dgemv1 I/O
[Aijk_space],
[Adlatrd_space],
[p: sTag],
[b: sTag],
[n: sTag] -> (dgemv1);

(dgemv1) -> [Adlatrd_space], <dgemv2_tags>;

// dgemv2 I/O
[Adlatrd_space],
[Wwork],
[p: sTag],
[b: sTag],
[n: sTag] -> (dgemv2);

(dgemv2) -> [Adlatrd_space], <dlarf_tags>;

// dlarf I/O
[Adlatrd_space],
[num_threads],
[p: sTag],
[b: sTag],
[n: sTag] -> (dlarf_c);

(dlarf_c) -> [tau], [E_temp], [Adlatrd_space], <dsymv_tags>, <dsymv_check_tags>;

// dsydmv I/O
[Adlatrd_space],
[Wwork],
[Y_th],
[num_threads],
[thread_col_size],
[p: sTag],
[b: sTag],
[n: sTag] -> (dsydmv);

(dsydmv) -> [Y_th_space];

// dsymv_check I/O
[num_threads],
[b: sTag],
[n: sTag],
[p: sTag],
[Wwork],
[Y_th_space] -> (dsymv_check);

(dsymv_check) -> [Wwork], <dgemv3_tags>;

// dgemv3 I/O
[Adlatrd_space],
[b: sTag],
[n: sTag],
[p: sTag],
[Wwork] -> (dgemv3);

(dgemv3) -> [Wwork], <dgemv4_tags>;

// dgemv4 I/O
[Adlatrd_space],
[b: sTag],
[n: sTag],
[p: sTag],
[Wwork] -> (dgemv4);

(dgemv4) -> [Wwork], <dgemv5_tags>;

// dgemv5 I/O
[Adlatrd_space],
[b: sTag],
[n: sTag],
[p: sTag],
[Wwork] -> (dgemv5);

(dgemv5) -> [Wwork], <dgemv6_tags>;

// dgemv6 I/O
[Adlatrd_space],
[b: sTag],
[n: sTag],
[p: sTag],
[Wwork] -> (dgemv6);

(dgemv6) -> [Wwork], <dscal_tags>;

// dscalar I/O
[Adlatrd_space],
[b: sTag],
[n: sTag],
[p: sTag],
[Wwork],
[tau] -> (dscalar);

(dscalar) -> [Wwork], <dot_tags>;

// dot I/O
[Adlatrd_space],
[b: sTag],
[n: sTag],
[p: sTag],
[Wwork],
[tau] -> (dot);

(dot) -> [alpha], <daxpy_tags>;

// ddaxpy I/O
[Adlatrd_space],
[E_temp],
[p: sTag],
[b: sTag],
[n: sTag],
[alpha],
[Wwork] -> (ddaxpy);

(ddaxpy) -> [Adlatrd_space], [Wwork], <dgemv1_tags>, [D], [E], [W], [Aijk_space], [E_temp], <dsyr2kAW_tags>;

//dsyr2kAW Executions
[Aijk_space],
[W],
[p: sTag],
[n: sTag],
[b: sTag] -> (dsyr2kAW);

(dsyr2kAW) -> [Aijk_space];

//dsytd2_t Executions
[Aijk_space],
[n: sTag],
[p: sTag],
[b: sTag] -> (dsytd2_t);

(dsytd2_t) -> [D], [E], <dsterf_tags>;

//dsterf_t Executions
[D],
[E],
[p: sTag],
[b: sTag] -> (dsterf_t);

(dsterf_t) -> [D];

// Return to the caller
[D] -> env;


// eof
