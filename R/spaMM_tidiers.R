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
#'   data("blackcap")
#'   lmm2 <- fitme(Reaction ~ Days, data = sleepstudy, method = "REML")
#'   lmm2 <- fitme(Reaction ~ Days + (Days | Subject), data = sleepstudy, method = "REML")
#'   lmm2 <- fitme(migStatus ~ means + Matern(1|longitude+latitude), data = blackcap)
#'   #tidy(lmm2)
#'   #augment(lmm2)
#'   glance(lmm2)
#'   glance(lmm2, AIC.details = TRUE)
#'   glance(lmm2, npar.details = TRUE)
#'   glance(lmm2, AIC.details = TRUE, npar.details = TRUE)
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
#' @param AIC.details whether to include conditional AIC (cAIC) and dispersion AIC (dAIC), see [spaMM::AIC()]
#' @param npar.details whether to include a breakdown of the number of parameters:
#'
#'   - \code{npar.fixed} the number of parameters used to model fixed effects
#'   
#'   - \code{npar.rand} the number of parameters used to model random effects variances and covariances
#'   
#'   - \code{npar.cor} the number of parameters used to adjust autocorrelation
#'   
#'   - \code{npar.family} the number of parameters used to adjust the error distribution (1 for NegBin, 0 otherwise)
#'   
#'   - \code{npar.resid} the number of parameters used in the residual model ## FIXME: fixed, random or both?
#'   
#' @export
glance.HLfit <- function(x, AIC.details = FALSE, npar.details = FALSE, ...) {

  ## Add basic statistics:
  sigma <- sqrt(spaMM::get_residVar(x))
  sigma <- ifelse(length(unique(sigma) > 1L), sigma[1L], NA)
  AICs <- spaMM::AIC.HLfit(x, verbose = FALSE, also_cAIC = AIC.details)

  ret <- tibble::tibble(sigma = sigma,
                        logLik = spaMM::logLik.HLfit(x)[[1]],
                        AIC = AICs[[1]],
                        deviance = spaMM::deviance.HLfit(x))
  
  ## Add cAIC and dAIC (if requested):
  if (AIC.details) {
    if (length(AICs) > 1L) {
      ret %>%
        dplyr::mutate(cAIC = AICs[[2]],
                      dAIC = AICs[[3]], .after = .data$AIC) -> ret
    } else {
      message("`AIC.details = TRUE` has no effect in the abscence of random effects")
    }
  }
  
  ## Compute number of parameters (always):
  tbl_npar <- tibble::tibble(npar.fixed = x$dfs$pforpv,
                             npar.rand = x$dfs$p_lambda, #length(attr(spaMM::get_ranPars(x)$ranCoefs[[1]], "transf"))
                             npar.cor = x$dfs$p_corrPars, #max(c(as.numeric(lapply(x$corrPars, length)), 0L))
                             npar.family = ifelse(x$family$family == "negbin", 1L, 0L),
                             npar.resid = x$dfs$p_fixef_phi,
                             npar = .data$npar.fixed + .data$npar.rand + .data$npar.cor + .data$npar.family + .data$npar.resid)
  
  ## Add details about number of parameters (if requested):
  if (npar.details) {
    ret %>%
      dplyr::bind_cols(tbl_npar) -> ret
  }
  
  ## Add df.residual and the observations (always):
  ret %>% 
    dplyr::mutate(df.residual = nrow(x$X.pv) - tbl_npar$npar,
                  nobs = nrow(x$X.pv)) -> ret
  
  ## Return
  ret
  }
