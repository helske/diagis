#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP diagis_arma_running_mean(SEXP);
extern SEXP diagis_arma_running_mean_vec(SEXP);
extern SEXP diagis_arma_running_var(SEXP, SEXP);
extern SEXP diagis_arma_running_weighted_mean(SEXP, SEXP);
extern SEXP diagis_arma_running_weighted_mean_vec(SEXP, SEXP);
extern SEXP diagis_arma_running_weighted_var(SEXP, SEXP, SEXP);
extern SEXP diagis_arma_weighted_mean(SEXP, SEXP);
extern SEXP diagis_arma_weighted_mean_mat(SEXP, SEXP);
extern SEXP diagis_arma_weighted_mean_vec(SEXP, SEXP);
extern SEXP diagis_arma_weighted_se(SEXP, SEXP);
extern SEXP diagis_arma_weighted_se_vec(SEXP, SEXP);
extern SEXP diagis_arma_weighted_var(SEXP, SEXP, SEXP);
extern SEXP diagis_arma_weighted_var_mat(SEXP, SEXP, SEXP);
extern SEXP diagis_arma_weighted_var_vec(SEXP, SEXP, SEXP);
extern SEXP diagis_arma_weighted_var_vec_t(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"diagis_arma_running_mean",              (DL_FUNC) &diagis_arma_running_mean,              1},
  {"diagis_arma_running_mean_vec",          (DL_FUNC) &diagis_arma_running_mean_vec,          1},
  {"diagis_arma_running_var",               (DL_FUNC) &diagis_arma_running_var,               2},
  {"diagis_arma_running_weighted_mean",     (DL_FUNC) &diagis_arma_running_weighted_mean,     2},
  {"diagis_arma_running_weighted_mean_vec", (DL_FUNC) &diagis_arma_running_weighted_mean_vec, 2},
  {"diagis_arma_running_weighted_var",      (DL_FUNC) &diagis_arma_running_weighted_var,      3},
  {"diagis_arma_weighted_mean",             (DL_FUNC) &diagis_arma_weighted_mean,             2},
  {"diagis_arma_weighted_mean_mat",         (DL_FUNC) &diagis_arma_weighted_mean_mat,         2},
  {"diagis_arma_weighted_mean_vec",         (DL_FUNC) &diagis_arma_weighted_mean_vec,         2},
  {"diagis_arma_weighted_se",               (DL_FUNC) &diagis_arma_weighted_se,               2},
  {"diagis_arma_weighted_se_vec",           (DL_FUNC) &diagis_arma_weighted_se_vec,           2},
  {"diagis_arma_weighted_var",              (DL_FUNC) &diagis_arma_weighted_var,              3},
  {"diagis_arma_weighted_var_mat",          (DL_FUNC) &diagis_arma_weighted_var_mat,          3},
  {"diagis_arma_weighted_var_vec",          (DL_FUNC) &diagis_arma_weighted_var_vec,          3},
  {"diagis_arma_weighted_var_vec_t",        (DL_FUNC) &diagis_arma_weighted_var_vec_t,        3},
  {NULL, NULL, 0}
};

void R_init_diagis(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
