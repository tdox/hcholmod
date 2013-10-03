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

#include "cholmod_xface.h"

// void** addressOf(void* ptr) {return &ptr;}

cholmod_common* cholmod_allocate_common()
{
  return malloc( sizeof(cholmod_common) );
}

void cholmod_free_common(cholmod_common* c) {free(c);}


size_t cholmod_triplet_get_nrow(cholmod_triplet* a) {return a->nrow;}
size_t cholmod_triplet_get_ncol(cholmod_triplet* a) {return a->ncol;}
size_t cholmod_triplet_get_nzmax(cholmod_triplet* a) {return a->nzmax;}
size_t cholmod_triplet_get_nnz(cholmod_triplet* a) {return a->nnz;}
int* cholmod_triplet_get_row_indices(cholmod_triplet* a) {return (int*) a->i;}
int* cholmod_triplet_get_column_indices(cholmod_triplet* a) {return (int*) a->j;}
double* cholmod_triplet_get_x(cholmod_triplet* a) {return a->x;}
void cholmod_triplet_set_nnz(cholmod_triplet* a, int nnz) {a->nnz = nnz;}
int cholmod_triplet_free_xface(cholmod_triplet* a, cholmod_common* c)
  {return cholmod_free_triplet(&a, c);}

void cholmod_triplet_free_void(cholmod_common* c, cholmod_triplet* a)
  {cholmod_free_triplet(&a, c);}

size_t cholmod_dense_get_nrow(cholmod_dense* a) {return a->nrow;}
size_t cholmod_dense_get_ncol(cholmod_dense* a) {return a->ncol;}
size_t cholmod_dense_get_nzmax(cholmod_dense* a) {return a->nzmax;}
double* cholmod_dense_get_x(cholmod_dense* a) {return a->x;}
int cholmod_dense_free_xface(cholmod_dense* a, cholmod_common* c)
  {return cholmod_free_dense(&a, c);}

void cholmod_dense_free_void(cholmod_common* c, cholmod_dense* a)
  {cholmod_free_dense(&a, c);}

size_t cholmod_sparse_get_nrow(cholmod_sparse* a) {return a->nrow;}
size_t cholmod_sparse_get_ncol(cholmod_sparse* a) {return a->ncol;}
size_t cholmod_sparse_get_nzmax(cholmod_sparse* a) {return a->nzmax;}
double* cholmod_sparse_get_x(cholmod_sparse* a) {return a->x;}
int cholmod_sparse_free_xface(cholmod_sparse* a, cholmod_common* c)
  {return cholmod_free_sparse(&a, c);}

void cholmod_sparse_free_void(cholmod_common* c, cholmod_sparse* a)
  {cholmod_free_sparse(&a, c);}

int cholmod_factor_free_xface(cholmod_factor* a, cholmod_common* c)
  {return cholmod_free_factor(&a, c);}

void cholmod_factor_free_void(cholmod_common* c, cholmod_factor* a)
  {cholmod_free_factor(&a, c);}

void cholmod_common_finish_and_free(cholmod_common* c)
{
  cholmod_finish(c);
  cholmod_free_common(c);
}

