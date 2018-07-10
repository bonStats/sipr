#' Methods for class "sipo"
#'
#' @param sip The sipo object.
#' @param ... Additional arguments to be passed to function
#' @export
#' @method print sipo
print.sipo <- function(sip, ...){

  if(class(sip) != "sipo") stop("Class of sip must be 'sipo'. See ?sip for details.")

  # objective
  cat("-- Objective --\n ")
  print(sip$obj$fn)
  if(!is.null(sip$obj$gr)){
    cat(" gradient:\t")
    print(sip$obj$gr)
  }

  # constraints
  cat("\n-- Constraint(s) --\n")
  for(i in seq_along(sip$constraint_list)){

    print(sip$constraint_list[[i]], padding = T)

  }
  if(!is.null(sip$x$lower)){
    cat(" for",sip$x$lower,"<")
  }
  if(!(is.null(sip$x$lower) | is.null(sip$x$upper))){
    cat(" x ")
  }
  if(!is.null(sip$x$upper)){
    cat("<", sip$x$upper)
  }
}
