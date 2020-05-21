rand_string <- function(length = 50){
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = '')
}

compare_rave_version <- function(ver, strict = FALSE){
  if(length(ver) != 1){
    return(FALSE)
  }
  rave_ver <- utils::packageVersion('ravecore')
  compare <- utils::compareVersion(as.character(ver), as.character(rave_ver))
  if(compare > 0){ return(TRUE) }
  if(compare == 0 && !strict){ return(TRUE) }
  return(FALSE)
}

