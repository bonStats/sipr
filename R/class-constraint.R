#' Methods for class "constraint"
#'
#' @param cns The constraint object.
#' @param padding Add LHS padding to output.
#' @param ... Additional arguments to be passed to function
#' @export
#' @method print constraint
print.constraint <- function(cns, padding = F, ...){
  if(all(c("inequality","infinite") %in% class(cns))){
    if(padding) cat(" ")
    print(cns$original$fn)
    if(padding) cat(" ")
    cat(" between [", paste0(cns$original$bounds, collapse = ", "),"]\n")
    if(padding) cat(" ")
    cat(" for all t in ")
    for(j in seq_along(cns$original$tset)){
      cat("[",paste0(cns$original$tset[[j]], collapse = ", "),"]")
      if(length(cns$original$tset) == j){
        cat("\n")
      } else {
        cat(", ")
      }
    }

  } else if(all(c("inequality","finite") %in% class(cns))){

    if(padding) cat(" ")
    print(cns$fn)
    if(padding) cat(" ")
    cat(" between [", paste0(cns$bounds, collapse = ", "),"]\n")

  } else if("equality" %in% class(cns)){

    if(padding) cat(" ")
    print(cns$fn)
    if(padding) cat(" ")
    cat(" =", cns$value,"\n")

  }
}

