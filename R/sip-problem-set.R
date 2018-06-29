#' Problem set from XXX for SIP problems.
#'
#' Create SIP objects \code{sipo} from classis SIP problems.
#'
#' @param number Problem number
#' @return A predefined SIP problem
#' @export
#'

sip_problem <- function(number){

  if(missing(number)) stop("Problem number must be specified")

  if(number == 1){

    prob <- sip(obj.fn = function(x){ sum(x^2) + x[2] },
                obj.gr = function(x){ 2 * x },
                x.bounds = list(c(-2,2),c(-5,5)),
                eq.fn = NULL,
                eq.gr = NULL,
                eq.val = NULL,
                ineq.fn = function(x,t){ t * x[1] + (t^2) * x[2] },
                ineq.bounds = c(-Inf, 0),
                ineq.tset = c(0,1),
                ineq.gr.x = function(x,t){ c(t, t^2)},
                ineq.gr.t = function(x,t){  x[1] + 2 * t *  x[2]},
                x.feasible = c(0,0)
                )


  } else {

    warning("SIP problem number ", number, " not found.")

  }

  return(prob)

}
