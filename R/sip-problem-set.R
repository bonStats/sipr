#' Problem set from XXX for SIP problems.
#'
#' Create SIP objects \code{sipo} from classis SIP problems.
#'
#' Problem 1: Original problem
#' Problem 2: # Ex. 3 Sect. 4 JJ Ruckman & A Shapiro (2009)
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


  } else if(number == 2){
    # Ex. 3 Sect. 4 JJ Ruckman & A Shapiro (2009)
    prob <- sip(obj.fn = function(x){ -x[1]^2 + x[2] },
                obj.gr = function(x){ c(-2 * x[1], 1) },
                x.bounds = NULL,
                eq.fn = NULL,
                eq.gr = NULL,
                eq.val = NULL,
                ineq.fn = function(x,t){ 2 * t * x[1] - x[2] - t^2},
                ineq.bounds = c(-Inf, 0),
                ineq.tset = c(-1,1),
                ineq.gr.x = function(x,t){ c(2 * t, -1)},
                ineq.gr.t = function(x,t){  2 * x[1] - 2 * t},
                x.feasible = c(0,0)
    )

  } else {

    stop("SIP problem number ", number, " not found.")

  }

  return(prob)

}
