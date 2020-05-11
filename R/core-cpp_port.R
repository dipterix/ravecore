
set_attr_inplace <- function(x, name, attribute) {
  stopifnot2(is.character(name), msg = 'attribute name should be a string')
  invisible(.Call('_ravecore_cpp_set_attr_inplace', PACKAGE = 'ravecore', x, name, attribute))
}
