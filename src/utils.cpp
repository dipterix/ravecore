#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

template <class T>
void cpp_set_attr_inplace_template(T x, std::string& name, SEXP& attribute){
  x.attr(name) = attribute;
}

// [[Rcpp::export]]
void cpp_set_attr_inplace(SEXP& x, std::string name, SEXP& attribute){
  switch(TYPEOF(x)){
  case INTSXP:
    cpp_set_attr_inplace_template<IntegerVector>(x, name, attribute);
    break;
  case LGLSXP:
    cpp_set_attr_inplace_template<LogicalVector>(x, name, attribute);
    break;
  case REALSXP:
    cpp_set_attr_inplace_template<NumericVector>(x, name, attribute);
    break;
  case CPLXSXP:
    cpp_set_attr_inplace_template<ComplexVector>(x, name, attribute);
    break;
  case STRSXP:
    cpp_set_attr_inplace_template<CharacterVector>(x, name, attribute);
    break;
  case RAWSXP:
    cpp_set_attr_inplace_template<RawVector>(x, name, attribute);
    break;
  case VECSXP:
    cpp_set_attr_inplace_template<List>(x, name, attribute);
    break;
  default:
    stop("set_dim_inplace input x has unsupported type.");
  }
}

/*** R
x = list(as.character(1:4))
y = x
a1 = pryr::address(x)
cpp_set_attr_inplace(x, 'class', 'asdad')
a2 = pryr::address(x)
a1 == a2


*/
