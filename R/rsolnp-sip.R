#' Rsolnp::solnp for semi-infinite programming.
#'
#' Wrapper for \code{\link[Rsolnp]{solnp}} to solve semi-infinite program. \code{solnp} uses the augmented Lagrange method for non-linear optimisation.
#'
#' @param pars starting parameter vector.
#' @param sipo SIP object created by \code{\link{make_sip}}.
#' @param control See control argument in \code{\link[Rsolnp]{solnp}}.
#' @return Results from \code{\link[Rsolnp]{solnp}} as sipo object (see \code{\link{sip}}).
#' @export

optim_solnp <- function(pars, sip, control = list(), ...){

  if(missing(sip) | class(sip) != "sipo") stop("Must include SIP object (sipo) to optimise, see sipr::sip")

  opt <-
    solnp(pars = pars,
          fun = sip$obj$fn,
          eqfun = sip$eq$fn,
          eqB = sip$eq$val,
          ineqfun = sip$ll$fn, # lower level problem function (not sip$ineq$fn)
          ineqLB = sip$ineq$bounds$lower,
          ineqUB = sip$ineq$bounds$upper,
          LB = sip$x$bounds$lower,
          UB = sip$x$bounds$upper,
          control = control, ...)

  # convert to standardised output

  return(opt)

}
