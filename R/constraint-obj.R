#' Make a infinite inequality constraint object to be used by in \code{sip} to create a \code{sipo}.
#'
#' Only one infinite constraint may be specified per object.
#'
#' @param fun Inequality function \code{g(x,t)}.
#' @param grad.x (Optional) gradient (vector) of inequality constraint function w.r.t \code{x}.
#' @param grad.t (Optional) gradient (vector) of inequality constraint function w.r.t \code{t}.
#' @param fun.bounds Lower and/or upper bound of \code{fun} input argument \code{x}. One, but not both, may be infinite.
#' @param t.bounds List of compact index set(s) for \code{t} in which inequality constraint \code{fun} must be satisfied. Must be finite.
#' @param t.start (Optional) where to start the optimisation of the lower level problem. Defaults to \code{mean(ineq.tset)}.
#' @return An infinite constraint object (\code{class = c("constraint","inequality","infinite")})
#' @export

infinite_inequality_constraint <- function(fun, grad.x = NULL, grad.t = NULL, fun.bounds, t.bounds, t.start = NULL){

  if(missing(fun)) stop("fun for inequality required")
  if(missing(fun.bounds)) stop("fun.bounds for inequality required")
  if(missing(t.bounds)) stop("t.bounds (index set bounds) for inequality required")

  if(class(fun) != "function") stop("fun must be function")

  if(!all(c("x","t") %in% names(formals(fun)))) stop("fun must have arguments x and t")

  if(!missing(grad.x) & !all(c("x","t") %in% names(formals(grad.x)))) stop("grad.x must have arguments x and t")
  if(!missing(grad.t) & !all(c("x","t") %in% names(formals(grad.t)))) stop("grad.t must have arguments x and t")

  if(class(fun.bounds) != "numeric" |
     length(fun.bounds) != 2 |
     sum(is.finite(fun.bounds)) < 1) stop("fun.bounds must be numeric and length == 2 with at least one element finite")

  if(class(t.bounds) != "list"){
    t_bounds <- list(t.bounds)
  } else{
    t_bounds <- t.bounds
  }

  if(missing(t.start)){
    t_start <- sapply(t_bounds, mean)
  } else {
    t_start <- t.start
  }

  if(any(sapply(t_bounds, class) != "numeric") |
     any(sapply(t_bounds, length) != 2) |
     any(!sapply(t_bounds, is.finite))) stop("t.bounds must have elements that are numeric, length == 2, and finite")

  cstr <- structure(
    list(
      original =
        list(fun = fun,
             gr = list(x = grad.x, t = grad.t),
             bounds = sort(fun.bounds),
             tset = lapply(t_bounds, sort)),
      lowerlevel =
        list(fun = NULL,
             lower = NULL,
             upper = NULL,
             boundtype = NULL,
             gr = NULL,
             gen = list(fun = NULL, gr = NULL),
             tstart = t_start)
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

  cstr$lowerlevel$gen$fun <- function(par.t, x, bound, ...){

    sc <- ifelse(bound == "lower", 1, -1)

    if(is.null(cstr$original$gr$t)){
      gr <- NULL
    } else {
      gr <- function(t) cstr$original$gr$t(x = x, t)
    }

    opt <- optim(par = par.t,
                 fun = function(t) cstr$original$fun(x = x, t),
                 gr = gr,
                 lower = lower_tset,
                 upper = upper_tset,
                 control = list(funscale = sc), method = "L-BFGS-B",
                 ...)

    return(opt)

  }

  cstr$lowerlevel$fun <- function(x, ...){

    sapply(cstr$lowerlevel$boundtype, function(bnd) cstr$lowerlevel$gen$fun(par.t = cstr$lowerlevel$tstart, x = x, bound = bnd, ...)$value)

  }

  if(!is.null(cstr$original$gr$x)){

    cstr$lowerlevel$gen$gr <- function(par.t, x, bound, ...){
      t_opt <- cstr$lowerlevel$gen$fun(par.t = par.t, x = x, bound = bound, ...)
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
#' @param fun Inequality function \code{g(x,t)}.
#' @param fun.bounds Lower and/or upper bound of \code{fun} input argument \code{x}. One, but not both, may be infinite.
#' @return A constraint object (\code{class = c("constraint","inequality","finite")}).
#' @export

finite_inequality_constraint <- function(fun, grad.x = NULL, fun.bounds){

  if(missing(fun)) stop("fun for inequality required")
  if(missing(fun.bounds)) stop("fun.bounds for inequality required")

  if(class(fun) != "function") stop("fun must be function")

  if(!all(c("x") %in% names(formals(fun)))) stop("fun must have arguments x")

  if(!missing(grad.x) & !all(c("x") %in% names(formals(grad.x)))) stop("grad.x must have arguments x and t")

  if(class(fun.bounds) != "numeric" |
     length(fun.bounds) != 2 |
     sum(is.finite(fun.bounds)) < 1) stop("fun.bounds must be numeric and length == 2 with at least one element finite")

  cstr <- structure(
    list(fun = fun,
         gr = grad.x,
         bounds = sort(fun.bounds)
    ),
    class = c("constraint","inequality","finite"))

  return(cstr)

}


#' Make an equality constraint object to be used by in \code{sip} to create a \code{sipo}.
#'
#' Only one constraint may be specified per object.
#'
#' @param fun Equality function \code{g(x,t)}.
#' @param fun.bounds Lower and/or upper bound of \code{fun} input argument \code{x}. One, but not both, may be infinite.
#' @param value Value for equality.
#' @return A constraint object (\code{class = c("constraint","equality","finite")})
#' @export

finite_equality_constraint <- function(fun, grad.x = NULL, value){

  if(missing(fun)) stop("fun for equality required")

  if(missing(value)) stop("value for equality required")

  if(class(fun) != "function") stop("fun must be function")

  if(!all(c("x") %in% names(formals(fun)))) stop("fun must have arguments x")

  if(!missing(grad.x) & !all(c("x") %in% names(formals(grad.x)))) stop("grad.x must have arguments x")

  cstr <- structure(
    list(fun = fun,
         gr = grad.x,
         value = value),
    class = c("constraint","equality","finite"))

  return(cstr)

}
