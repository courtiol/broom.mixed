#' Tidying methods for mixed effects models
#'
#' These methods tidy the coefficients of mixed effects models, particularly
#' responses of the \code{HLfit} class
#'
#' @inheritParams glmmTMB_tidiers
#' @param x An object of class \code{HLfit}, such as those from \code{fitme},
#' \code{HLfit}, or \code{corrHLfit}
#'
#' @return All tidying methods return a \code{tibble}.
#' The structure depends on the method chosen.
#'
#' @name spaMM_tidiers
#' @aliases tidy.HLfit
#'
#' @examples
#'
#' if (require("spaMM") && require("lme4")) {
#'   lmm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'   tidy(lmm1)
#'   augment(lmm1)
#'   glance(lmm1)
#' }
NULL

#' @rdname spaMM_tidiers
#' @export
tidy.HLfit <- function(x, effects = c("ran_pars", "fixed"), ...) {
  message("method not yet implemented")
}

#' @rdname spaMM_tidiers
#' @export
augment.HLfit <- function(x, ...) {
  message("method not yet implemented")
}

#' @rdname spaMM_tidiers
#' @export
glance.HLfit <- function(x, ...) {
  message("method not yet implemented")
}
