// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cpp_set_attr_inplace
void cpp_set_attr_inplace(SEXP& x, std::string name, SEXP& attribute);
RcppExport SEXP _ravecore_cpp_set_attr_inplace(SEXP xSEXP, SEXP nameSEXP, SEXP attributeSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type attribute(attributeSEXP);
    cpp_set_attr_inplace(x, name, attribute);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ravecore_cpp_set_attr_inplace", (DL_FUNC) &_ravecore_cpp_set_attr_inplace, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_ravecore(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
