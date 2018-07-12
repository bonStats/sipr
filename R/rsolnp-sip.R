#' Rsolnp::solnp for semi-infinite programming.
#'
#' Wrapper for \code{\link[Rsolnp]{solnp}} to solve semi-infinite program. \code{solnp} uses the augmented Lagrange method for non-linear optimisation.
#'
#' @param pars starting parameter vector.
#' @param sip SIP object created by \code{\link{make_sip}}.
#' @param control See control argument in \code{\link[Rsolnp]{solnp}}.
#' @return Results from \code{\link[Rsolnp]{solnp}} as sipo object (see \code{\link{sip}}).
#' @export

optim_solnp <- function(pars, sip, control = list(), ...){

  if(missing(sip) | class(sip) != "sipo") stop("Must include SIP object (sipo) to optimise, see sipr::sip")

  opt <-
    solnp(pars = pars,
          fun = sip$obj$fn,
          eqfun = sip$eq$fn,
          eqB = sip$eq$value,
          ineqfun = sip$ineq$all$fn,
          ineqLB = sip$ineq$all$lower,
          ineqUB = sip$ineq$all$upper,
          LB = sip$x$lower,
          UB = sip$x$upper,
          control = control, ...)

  # convert to standardised output

  return(opt)

}
