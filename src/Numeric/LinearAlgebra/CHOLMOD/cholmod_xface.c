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

#include "cholmod.h"
#include "cholmod_xface.h"

// void** addressOf(void* ptr) {return &ptr;}

cholmod_common* cholmodx_allocate_common()
{
  return malloc( sizeof(cholmod_common) );
}

void cholmodx_free_common(cholmod_common* c) {free(c);}

size_t cholmodx_triplet_get_nrow(cholmod_triplet* a) {return a->nrow;}
size_t cholmodx_triplet_get_ncol(cholmod_triplet* a) {return a->ncol;}
size_t cholmodx_triplet_get_nzmax(cholmod_triplet* a) {return a->nzmax;}
size_t cholmodx_triplet_get_nnz(cholmod_triplet* a) {return a->nnz;}
int*   cholmodx_triplet_get_row_indices(cholmod_triplet* a) {return (int*) a->i;}

int*   cholmodx_triplet_get_column_indices(cholmod_triplet* a)
   {return (int*) a->j;}

double* cholmodx_triplet_get_x(cholmod_triplet* a) {return a->x;}
void    cholmodx_triplet_set_nnz(cholmod_triplet* a, int nnz) {a->nnz = nnz;}
int     cholmodx_triplet_free_xface(cholmod_triplet* a, cholmod_common* c)
  {return cholmod_free_triplet(&a, c);}

void cholmodx_triplet_free_void(cholmod_common* c, cholmod_triplet* a)
  {cholmod_free_triplet(&a, c);}

size_t  cholmodx_dense_get_nrow(cholmod_dense* a) {return a->nrow;}
size_t  cholmodx_dense_get_ncol(cholmod_dense* a) {return a->ncol;}
size_t  cholmodx_dense_get_nzmax(cholmod_dense* a) {return a->nzmax;}
double* cholmodx_dense_get_x(cholmod_dense* a) {return a->x;}

int cholmodx_dense_free_xface(cholmod_dense* a, cholmod_common* c)
  {return cholmod_free_dense(&a, c);}

void cholmodx_dense_free_void(cholmod_common* c, cholmod_dense* a)
  {cholmod_free_dense(&a, c);}

size_t  cholmodx_sparse_get_nrow(cholmod_sparse* a) {return a->nrow;}
size_t  cholmodx_sparse_get_ncol(cholmod_sparse* a) {return a->ncol;}
size_t  cholmodx_sparse_get_nzmax(cholmod_sparse* a) {return a->nzmax;}
double* cholmodx_sparse_get_x(cholmod_sparse* a) {return a->x;}

int cholmodx_sparse_free_xface(cholmod_sparse* a, cholmod_common* c)
  {return cholmod_free_sparse(&a, c);}

void cholmodx_sparse_free_void(cholmod_common* c, cholmod_sparse* a)
  {cholmod_free_sparse(&a, c);}

int cholmodx_factor_free_xface(cholmod_factor* a, cholmod_common* c)
  {return cholmod_free_factor(&a, c);}

void cholmodx_factor_free_void(cholmod_common* c, cholmod_factor* a)
  {cholmod_free_factor(&a, c);}

void cholmodx_common_finish_and_free(cholmod_common* c)
{
  cholmod_finish(c);
  cholmodx_free_common(c);
}

