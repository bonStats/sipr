#' Make a semi-infinite program R object, \code{sipo}.
#'
#' Makes a \code{sipo} object to be optimised by SIP solvers included in \code{sipr}. \code{sipo} object contains objective function, constraints, and known feasible points (if applicable).
#'
#' @param obj.fn Objective function to be minimized.
#' @param obj.gr (Optional) gradient (vector) of objective function \code{obj.fn}.
#' @param x.bounds (Optional) Lower and upper bound of \code{fn} input argument \code{x}. As list, or 2-row matrix if multiple.
#' @param eq.fn (Optional) Equality constraint (vector) function.
#' @param eq.gr (Optional) gradient (vector) of equality constraint function.
#' @param eq.val (Optional) Value(s) equality constraint(s) are equal to.
#' @param ineq.fn Inequality constraint function. First argument for objective input variable \code{x}. Second argument for index set variable \code{t}.
#' @param ineq.bounds Lower and upper bound of \code{fn} input argument \code{x}. One, but not both, may be infinite.
#' @param ineq.tset Compact index set for \code{t} in which inequality constraint \code{ineq.fn} must be satisfied. Must be finite.
#' @param ineq.gr.x (Optional) gradient (vector) of inequality constraint function w.r.t \code{x}.
#' @param ineq.gr.t (Optional) gradient of inequality constraint function w.r.t \code{t}.
#' @param x.feasible List (or one) of feasible \code{x}.
#' @param t.start (Optional) where to start the optimisation of the lower level problem. Defaults to \code{mean(ineq.tset)}.
#' @param ... (Optional) Other items to store in the sipo.
#' @return A sipo: semi-infinite programming object
#' @export

sip <- function(obj.fn, obj.gr = NULL, x.bounds = NULL,
                eq.fn = NULL, eq.gr = NULL, eq.val = NULL,
                ineq.fn, ineq.bounds, ineq.tset,
                ineq.gr.x = NULL, ineq.gr.t = NULL,
                x.feasible, ...){

  if(missing(obj.fn)) stop("obj.fn required")
  if(missing(ineq.fn)) stop("ineq.fn required")
  if(missing(ineq.bounds)) stop("ineq.bounds required")
  if(missing(ineq.tset)) stop("ineq.tset required")
  if(missing(x.feasible)) stop("Please provide at least one feasible solution")

  if(class(ineq.tset) != "numeric" |
     length(ineq.tset) != 2 |
     is.infinite(diff(ineq.tset))) stop("ineq.tset must be a numeric with length 2, defining a compact set")

  if(class(ineq.bounds) != "numeric" |
     length(ineq.bounds) != 2 |
     sum(is.infinite(ineq.bounds)) == 2 ) stop("ineq.bounds must be a numeric with length 2, with at most one infinite bound")

  prog <- structure(
    list(
    obj = list(fn = obj.fn, gr = obj.gr),
    eq = list(fn = eq.fn, gr = eq.gr, val = eq.val),
    ineq = list(fn = ineq.fn, bounds = NULL, tset = sort(ineq.tset), gr = list(x = ineq.gr.x, t = ineq.gr.t), type = NULL),
    x = list(bounds = NULL, feasible = x.feasible),
    ll = list(fn = NULL, gr = NULL, gen = list(fn = NULL, gr = NULL), tstart = mean(ineq.tset)),
    ...
    ),
  class = "sipo")

  if(!is.null(x.bounds)){
    # x bounds
    b_x_mat <- matrix(unlist(x.bounds), nrow = 2)
    prog$x$bounds <- list(
      lower = b_x_mat[1,],
      upper = b_x_mat[2,]
    )
  }
  # inequality constraint bound
  b_ineq <- sort(ineq.bounds)
  n_ineq <- sum(is.finite(b_ineq))
  if(n_ineq == 2){
    prog$ineq$type <- "both"
  } else if(is.finite(b_ineq[1])) {
    prog$ineq$type <- "lower"
  } else if(is.finite(b_ineq[2])) {
    prog$ineq$type <- "upper"
  }

  prog$ineq$bounds <-list(lower = rep(b_ineq[1], times = n_ineq), upper = rep(b_ineq[2], times = n_ineq))

  # implement tests for provided functions

  # define lower level problem(s)
    prog$ll$gen$fn <- function(par.t, x, bound, ...){

        sc <- ifelse(bound == "lower", 1, -1)

        if(is.null(prog$ineq$gr$t)){
          gr <- NULL
        } else {
          gr <- function(t) prog$ineq$gr$t(x, t)
        }

        opt <- optim(par = par.t,
                     fn = function(t) prog$ineq$fn(x=x,t),
                     gr = gr,
                     lower = prog$ineq$tset[1], upper = prog$ineq$tset[2],
                     control = list(fnscale = sc), method = "L-BFGS-B",
                     ...)

      return(opt)

    }

    if(!is.null(prog$ineq$gr$x)){
      prog$ll$gen$gr <- function(par.t, x, bound, ...){
        t_opt <- prog$ll$fn(par.t, x, bound = bound, ...)
        x_gr <- prog$ineq$gr$x(x, t_opt$par) # envelope / implicit function theorem
      }
    }

    # set specific problems
    if(n_ineq == 2){ # two-sided inequality
      prog$ll$fn <- function(x, ...){
        c(prog$ll$gen$fn(par.t = prog$ll$tstart, x, bound = "lower", ...),
          prog$ll$gen$fn(par.t = prog$ll$tstart, x, bound = "upper", ...)
          )
      }
      if(!is.null(prog$ineq$gr$x)){
        prog$ll$gr <- function(x, ...){
          c(prog$ll$gen$gr(par.t = prog$ll$tstart, x, bound = "lower", ...)$value,
            prog$ll$gen$gr(par.t = prog$ll$tstart, x, bound = "upper", ...)$value
            )
        }
      }
    } else { # one sided inequality
      prog$ll$fn <- function(x, ...){
        prog$ll$gen$fn(par.t = prog$ll$tstart, x, bound = prog$ineq$type, ...)$value
      }
      if(!is.null(prog$ineq$gr$x)){
        prog$ll$gr <- function(x, ...){
          prog$ll$gen$gr(par.t = prog$ll$tstart, x, bound = prog$ineq$type, ...)$value
        }
      }
    }

  return(prog)
}

