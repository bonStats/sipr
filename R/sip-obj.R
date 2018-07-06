#' Make a semi-infinite program R object, \code{sipo}.
#'
#' Makes a \code{sipo} object to be optimised by SIP solvers included in \code{sipr}. \code{sipo} object contains objective function, constraints, and known feasible points (if applicable).
#'
#' @param fn Objective function to be minimized.
#' @param gr (Optional) gradient (vector) of objective function \code{fn}.
#' @param lower (Optional) Lower bound of \code{x}.
#' @param upper (Optional) Upper bound of \code{x}.
#' @param constraint List of constraint objects to be used.
#' @param x.feasible (Optional) List of feasible x (vector) points for optimisation.
#' @param ... (Optional) Other items to store in the sipo.
#' @return A sipo: semi-infinite programming object
#' @export

sip <- function(fn, gr = NULL, lower = NULL, upper = NULL,
                constraint, x.feasible = NULL, ...){

  if(missing(fn)) stop("fn required")
  if(missing(constraint)) stop("constraint(s) required")

  if(class(fn) != "function") stop("fn must be function")

  if(!missing(gr) & class(gr) != "function") stop("gr must be function if supplied")

  if(length(lower) > 0 & length(upper) > 0 & length(lower) != length(upper)) stop("lower and upper must have same length (or a least one is NULL")

  prog <- structure(
    list(
    obj = list(fn = fn, gr = gr),
    x = list(lower = lower, upper = upper, feasible = NULL),
    ineq = list(inf =  list(fn = NULL, gr = NULL, lower = NULL, upper = NULL),
                fin =  list(fn = NULL, gr = NULL, lower = NULL, upper = NULL),
                all =  list(fn = NULL, gr = NULL, lower = NULL, upper = NULL)
                  ),
    eq = list(fn = NULL, gr = NULL, value = NULL),
    constraint_list = NULL,
    ...
    ),
  class = "sipo")

  if("constraint" %in% class(constraint)){
    prog$constraint_list <- list(constraint)
  } else{
    prog$constraint_list <- constraint
  }

  if(any(!"constraint" %in% sapply(prog$constraint_list,class))) stop("constraint supplied must be of class constraint. See ?inf_ineq_constr.")

  if(class(x.feasible) != "list"){
    prog$x$feasible <- list(x.feasible)
  } else{
    prog$x$feasible <- x.feasible
  }

  # get indices for type of constraint
  inf_ineq_i <- sapply(prog$constraint_list, function(x) all(c("infinite","inequality") %in% class(x)) )
  fin_ineq_i <- sapply(prog$constraint_list, function(x) all(c("finite","inequality") %in% class(x)) )
  eq_i <- sapply(prog$constraint_list, function(x) all(c("finite","equality") %in% class(x)) )

  # get inf inequality functions, bounds, gradient
  if(sum(inf_ineq_i) > 0){
    prog$ineq$inf$fn <- function(x, ...){
      sapply(prog$constraint_list[inf_ineq_i], function(cn) cn$lowerlevel$fn(x = x, ...))
    }
    prog$ineq$inf$lower <- c(lapply(prog$constraint_list[inf_ineq_i], function(cn) cn$lowerlevel$lower), recursive = T)
    prog$ineq$inf$upper <- c(lapply(prog$constraint_list[inf_ineq_i], function(cn) cn$lowerlevel$upper), recursive = T)
    if(!any(sapply(prog$constraint_list[inf_ineq_i], function(cn) is.null(cn$lowerlevel$gr)))){
      prog$ineq$inf$gr <- function(x, ...){
        sapply(prog$constraint_list[inf_ineq_i], function(cn) cn$lowerlevel$gr(x = x, ...))
      }
    }
  }

  # get finite inequ  ality functions, bounds, gradient
  if(sum(fin_ineq_i) > 0){
    prog$ineq$fin$fn <- function(x, ...){
      sapply(prog$constraint_list[fin_ineq_i], function(cn) cn$fn(x = x, ...))
    }
    prog$ineq$fin$lower <- c(lapply(prog$constraint_list[fin_ineq_i], function(cn) cn$lower), recursive = T)
    prog$ineq$fin$upper <- c(lapply(prog$constraint_list[fin_ineq_i], function(cn) cn$upper), recursive = T)
    if(!any(sapply(prog$constraint_list[fin_ineq_i], function(cn) is.null(cn$gr)))){
      prog$ineq$fin$gr <- function(x, ...){
        sapply(prog$constraint_list[fin_ineq_i], function(cn) cn$gr(x = x, ...))
      }
    }
  }

  if(sum(inf_ineq_i) + sum(fin_ineq_i) > 0){

    all_ineq <- c(list(prog$ineq$inf),list(prog$ineq$fin))
    all_ineq <- all_ineq[sapply(all_ineq, function(cn) !is.null(cn$fn))]

    prog$ineq$all$fn <- function(x, ...){
      sapply(all_ineq, function(cn) cn$fn(x = x, ...))
    }
    prog$ineq$all$lower <- c(lapply(all_ineq, function(cn) cn$lower), recursive = T)
    prog$ineq$all$upper <- c(lapply(all_ineq, function(cn) cn$upper), recursive = T)
    if(!any(sapply(all_ineq, function(cn) is.null(cn$gr)))){
      prog$ineq$all$gr <- function(x, ...){
        sapply(all_ineq, function(cn) cn$gr(x = x, ...))
      }
    }
  }

  if(sum(eq_i) > 0){
    prog$eq$fn <- function(x, ...){
      sapply(prog$constraint_list[eq_i], function(cn) cn$fn(x = x, ...))
    }
    prog$eq$value <- c(lapply(prog$constraint_list[eq_i], function(cn) cn$value), recursive = T)
    if(!any(sapply(prog$constraint_list[eq_i], function(cn) is.null(cn$gr)))){
      prog$eq$gr <- function(x, ...){
        sapply(prog$constraint_list[eq_i], function(cn) cn$gr(x = x, ...))
      }
    }
  }

  return(prog)
}
