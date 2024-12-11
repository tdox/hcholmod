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

cholmod_common* cholmodx_allocate_common();

void    cholmodx_free_common(cholmod_common* c);

size_t  cholmodx_triplet_get_nrow(cholmod_triplet* a);
size_t  cholmodx_triplet_get_ncol(cholmod_triplet* a);
size_t  cholmodx_triplet_get_nzmax(cholmod_triplet* a);
size_t  cholmodx_triplet_get_nnz(cholmod_triplet* a);
int*    cholmodx_triplet_get_row_indices(cholmod_triplet* a);
int*    cholmodx_triplet_get_column_indices(cholmod_triplet* a);
double* cholmodx_triplet_get_x(cholmod_triplet* a);
void    cholmodx_triplet_set_nnz(cholmod_triplet* a, int nnz);
int     cholmodx_triplet_free_xface(cholmod_triplet* a, cholmod_common* c);
void    cholmodx_triplet_free_void(cholmod_common* c, cholmod_triplet* a);

size_t  cholmodx_dense_get_nrow(cholmod_dense* a);
size_t  cholmodx_dense_get_ncol(cholmod_dense* a);
size_t  cholmodx_dense_get_nzmax(cholmod_dense* a);
double* cholmodx_dense_get_x(cholmod_dense* a);
int     cholmodx_dense_free_xface(cholmod_dense* a, cholmod_common* c);

size_t  cholmodx_sparse_get_nrow(cholmod_sparse* a);
size_t  cholmodx_sparse_get_ncol(cholmod_sparse* a);
size_t  cholmodx_sparse_get_nzmax(cholmod_sparse* a);
double* cholmodx_sparse_get_x(cholmod_sparse* a);
int     cholmodx_sparse_free_xface(cholmod_sparse* a, cholmod_common* c);

int     cholmodx_factor_free_xface(cholmod_factor* a, cholmod_common* c);
void    cholmodx_factor_free_void(cholmod_common* c, cholmod_factor* a);

#endif


