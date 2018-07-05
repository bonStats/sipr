#' Make a infinite inequality constraint object to be used by in \code{sip} to create a \code{sipo}.
#'
#' Only one infinite constraint may be specified per object.
#'
#' @param fn Inequality function \code{g(x,t)}.
#' @param gr.x (Optional) gradient (vector) of inequality constraint function w.r.t \code{x}.
#' @param gr.t (Optional) gradient (vector) of inequality constraint function w.r.t \code{t}.
#' @param fn.bounds Lower and/or upper bound of \code{fn} input argument \code{x}. One, but not both, may be infinite.
#' @param t.bounds List of compact index set(s) for \code{t} in which inequality constraint \code{fn} must be satisfied. Must be finite.
#' @return An infinite constraint object (\code{class = c("constraint","inequality","infinite")})
#' @export

inf_ineq_constr <- function(fn, gr.x = NULL, gr.t = NULL, fn.bounds, t.bounds){

  if(missing(fn)) stop("fn for inequality required")
  if(missing(fn.bounds)) stop("fn.bounds for inequality required")
  if(missing(t.bounds)) stop("t.bounds (index set bounds) for inequality required")

  if(class(fn) != "function") stop("fn must be function")

  if(!all(c("x","t") %in% names(formals(fn)))) stop("fn must have arguments x and t")

  if(!missing(gr.x) & !all(c("x","t") %in% names(formals(gr.x)))) stop("gr.x must have arguments x and t")
  if(!missing(gr.t) & !all(c("x","t") %in% names(formals(gr.t)))) stop("gr.t must have arguments x and t")

  if(class(fn.bounds) != "numeric" |
     length(fn.bounds) != 2 |
     sum(is.finite(fn.bounds)) < 1) stop("fn.bounds must be numeric and length == 2 with at least one element finite")

  if(class(t.bounds) != "list"){
    t_bounds <- list(t.bounds)
  } else{
    t_bounds <- t.bounds
  }

  if(any(sapply(t_bounds, class) != "numeric") |
     any(sapply(t_bounds, length) != 2) |
     any(!sapply(t_bounds, is.finite))) stop("t.bounds must have elements that are numeric, length == 2, and finite")

  cstr <- structure(
    list(
      original =
        list(fn = fn,
             gr = list(x = gr.x, t = gr.t),
             bounds = sort(fn.bounds),
             tset = lapply(t_bounds, sort)),
      lowerlevel =
        list(fn = NULL,
             lower = NULL,
             upper = NULL,
             boundtype = NULL,
             gr = NULL,
             gen = list(fn = NULL, gr = NULL),
             tstart = sapply(t_bounds, mean))
    ),
    class = c("constraint","inequality","infinite"))

  lower_tset <- sapply(cstr$original$tset, getElement, 1)
  upper_tset <- sapply(cstr$original$tset, getElement, 2)

  cstr$lowerlevel$boundtype <- c("lower","upper")[which(is.finite(cstr$original$bounds))]

  if(sum(is.finite(cstr$original$bounds)) == 1){ # one-sided case
    cstr$lowerlevel$lower <- cstr$original$bounds[1]
    cstr$lowerlevel$upper <- cstr$original$bounds[2]
  } else { # two-sided case
    cstr$lowerlevel$lower <- c(cstr$original$bounds[1], -Inf)
    cstr$lowerlevel$upper <- c(Inf, cstr$original$bounds[2])
  }

  cstr$lowerlevel$gen$fn <- function(par.t, x, bound, ...){

    sc <- ifelse(bound == "lower", 1, -1)

    if(is.null(cstr$original$gr$t)){
      gr <- NULL
    } else {
      gr <- function(t) cstr$original$gr$t(x = x, t)
    }

    opt <- optim(par = par.t,
                 fn = function(t) cstr$original$fn(x = x, t),
                 gr = gr,
                 lower = lower_tset,
                 upper = upper_tset,
                 control = list(fnscale = sc), method = "L-BFGS-B",
                 ...)

    return(opt)

  }

  cstr$lowerlevel$fn <- function(x, ...){

    sapply(cstr$lowerlevel$boundtype, function(bnd) cstr$lowerlevel$gen$fn(par.t = cstr$lowerlevel$tstart, x = x, bound = bnd, ...)$value)

  }

  if(!is.null(cstr$original$gr$x)){

    cstr$lowerlevel$gen$gr <- function(par.t, x, bound, ...){
      t_opt <- cstr$lowerlevel$gen$fn(par.t = par.t, x = x, bound = bound, ...)
      return(cstr$original$gr$x(x = x, t_opt$par)) # envelope / implicit function theorem

    }

    cstr$lowerlevel$gr <- function(x, ...){

      sapply(cstr$lowerlevel$boundtype, function(bnd) cstr$lowerlevel$gen$gr(par.t = cstr$lowerlevel$tstart, x = x, bound = bnd, ...)$value)

    }

  }

  return(cstr)

}



#' Make a (finite) inequality constraint object to be used by in \code{sip} to create a \code{sipo}.
#'
#' Only one infinite constraint may be specified per object.
#'
#' @param fn Inequality function \code{g(x,t)}.
#' @param fn.bounds Lower and/or upper bound of \code{fn} input argument \code{x}. One, but not both, may be infinite.
#' @return A constraint object (\code{class = c("constraint","inequality","finite")}).
#' @export

ineq_constr <- function(fn, gr.x = NULL, fn.bounds){

  if(missing(fn)) stop("fn for inequality required")
  if(missing(fn.bounds)) stop("fn.bounds for inequality required")

  if(class(fn) != "function") stop("fn must be function")

  if(!all(c("x") %in% names(formals(fn)))) stop("fn must have arguments x")

  if(!missing(gr.x) & !all(c("x") %in% names(formals(gr.x)))) stop("gr.x must have arguments x and t")

  if(class(fn.bounds) != "numeric" |
     length(fn.bounds) != 2 |
     sum(is.finite(fn.bounds)) < 1) stop("fn.bounds must be numeric and length == 2 with at least one element finite")

  cstr <- structure(
    list(fn = fn,
         gr = gr.x,
         bounds = sort(fn.bounds)
    ),
    class = c("constraint","inequality","finite"))

  return(cstr)

}


#' Make an equality constraint object to be used by in \code{sip} to create a \code{sipo}.
#'
#' Only one constraint may be specified per object.
#'
#' @param fn Equality function \code{g(x,t)}.
#' @param fn.bounds Lower and/or upper bound of \code{fn} input argument \code{x}. One, but not both, may be infinite.
#' @param value Value for equality.
#' @return A constraint object (\code{class = c("constraint","equality","finite")})
#' @export

eq_constr <- function(fn, gr.x = NULL, value){

  if(missing(fn)) stop("fn for equality required")

  if(missing(value)) stop("value for equality required")

  if(class(fn) != "function") stop("fn must be function")

  if(!all(c("x") %in% names(formals(fn)))) stop("fn must have arguments x")

  if(!missing(gr.x) & !all(c("x") %in% names(formals(gr.x)))) stop("gr.x must have arguments x")

  cstr <- structure(
    list(fn = fn,
         gr = gr.x,
         value = value),
    class = c("constraint","equality","finite"))

  return(cstr)

}
