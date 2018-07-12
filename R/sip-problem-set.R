#' Problem set for SIP problems.
#'
#' Create SIP objects \code{sipo} for given  SIP problems.
#'
#' Set covering: hyper-sphere covering hyper-cube
#'
#' @param cube.centre Centre of cube.
#' @param cube.diameter Diameter of cube.
#' @return A predefined SIP problem.
#' @export
#'

sip_hsphere_covers_hcube <- function(cube.centre, cube.diameter){

  if(length(cube.diameter) != 1) stop("Cube diameter must be one number")

  cube_xlim <- lapply(cube.centre, function(x){c(x - cube.diameter/2, x + cube.diameter/2)})

  cube_constraint <- inf_ineq_constr(
    fn = function(x, t){ sum( (x[-1] - t)^2 ) - x[1]^2 },
    fn.bounds = c(-Inf, 0),
    t.bounds  = cube_xlim
  )

  v_h_sphere <- function(r, n){
    (pi^(n/2)) * (r^n) / gamma(n/2 + 1)
  }

  prob <- sip(
    fn = function(x) v_h_sphere(r = x[1], n = length(x) - 1),
    constraint = list(cube_constraint),
    lower = c(0, rep(-Inf, times = length(cube.centre)))#,
    #upper = rep(Inf, times = (length(cube.centre) + 1))
  )

  return(prob)

}

