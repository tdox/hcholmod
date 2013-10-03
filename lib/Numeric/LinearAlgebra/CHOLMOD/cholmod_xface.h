/*
--------------------------------------------------------------------------------
--
--  Copyright (c) 2010 - 2013 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------
*/

#ifndef CHOLMOD_XFACE_H_
#define CHOLMOD_XFACE_H_

#include "cholmod.h" 

// void** addressOf(void* ptr);

cholmod_common* cholmod_allocate_common();

void cholmod_free_common(cholmod_common* c);


size_t cholmod_triplet_get_nrow(cholmod_triplet* a);
size_t cholmod_triplet_get_ncol(cholmod_triplet* a);
size_t cholmod_triplet_get_nzmax(cholmod_triplet* a);
size_t cholmod_triplet_get_nnz(cholmod_triplet* a);
int* cholmod_triplet_get_row_indices(cholmod_triplet* a);
int* cholmod_triplet_get_column_indices(cholmod_triplet* a);
double* cholmod_triplet_get_x(cholmod_triplet* a);
void cholmod_triplet_set_nnz(cholmod_triplet* a, int nnz);
int cholmod_triplet_free_xface(cholmod_triplet* a, cholmod_common* c);
void cholmod_triplet_free_void(cholmod_common* c, cholmod_triplet* a);

size_t cholmod_dense_get_nrow(cholmod_dense* a);
size_t cholmod_dense_get_ncol(cholmod_dense* a);
size_t cholmod_dense_get_nzmax(cholmod_dense* a);
double* cholmod_dense_get_x(cholmod_dense* a);
int cholmod_dense_free_xface(cholmod_dense* a, cholmod_common* c);

size_t cholmod_sparse_get_nrow(cholmod_sparse* a);
size_t cholmod_sparse_get_ncol(cholmod_sparse* a);
size_t cholmod_sparse_get_nzmax(cholmod_sparse* a);
double* cholmod_sparse_get_x(cholmod_sparse* a);
int cholmod_sparse_free_xface(cholmod_sparse* a, cholmod_common* c);

int cholmod_factor_free_xface(cholmod_factor* a, cholmod_common* c);
void cholmod_factor_free_void(cholmod_common* c, cholmod_factor* a);

#endif


