# Modeling — catalog, options, build-runs, fits.

#' Model catalog
#' @param data_version Optional data-version filter.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_model_catalog <- function(data_version = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_catalog()")
}

#' Describe a model
#' @param model_name Model name.
#' @param client A `vmx_client`.
#' @return A list / S3 object.
#' @export
vmx_model_describe <- function(model_name, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_describe()")
}

#' Preview modeling options for a data version
#' @param data_version A data-version id or `vmx_data_version`.
#' @param time_basis `"actual"` or `"nominal"`.
#' @param pd_marker Optional PD marker.
#' @param covariate Optional covariate(s).
#' @param client A `vmx_client`.
#' @return A list.
#' @export
vmx_modeling_options <- function(data_version, time_basis, pd_marker = NULL,
                                 covariate = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_modeling_options()")
}

#' Start a model build run (optionally wait)
#' @param data_version A data-version id or `vmx_data_version`.
#' @param time_basis `"actual"` or `"nominal"`.
#' @param pd_marker Optional `"GEN_UUID:increasing"` / `":decreasing"`.
#' @param covariate Optional covariate(s).
#' @param idempotency_key Optional idempotency key.
#' @param retried_from Optional prior run to retry from.
#' @param wait If `TRUE`, block until the run settles.
#' @param client A `vmx_client`.
#' @return A `vmx_model_build_run`.
#' @export
vmx_model_build <- function(data_version, time_basis, pd_marker = NULL,
                            covariate = NULL, idempotency_key = NULL,
                            retried_from = NULL, wait = FALSE,
                            client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build()")
}

#' List model build runs
#' @param data_version Optional filter.
#' @param study Optional filter.
#' @param treatment Optional filter.
#' @param status Optional status filter.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_model_build_runs <- function(data_version = NULL, study = NULL,
                                 treatment = NULL, status = NULL,
                                 client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_runs()")
}

#' Build-run status
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return A `vmx_model_build_run`.
#' @export
vmx_model_build_status <- function(run, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_status()")
}

#' Build-run logs
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @export
vmx_model_build_logs <- function(run, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_logs()")
}

#' Build-run events
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_model_build_events <- function(run, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_events()")
}

#' Build-run results
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @export
vmx_model_build_results <- function(run, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_results()")
}

#' Download build-run artifacts
#' @param run A build-run id or object.
#' @param dest Destination directory.
#' @param client A `vmx_client`.
#' @return Character vector of file paths.
#' @export
vmx_model_build_artifacts <- function(run, dest = ".", client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_artifacts()")
}

#' Markdown export of a build run
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @export
vmx_model_build_export <- function(run, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_export()")
}

#' Signed HTML report URL for a build run
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @export
vmx_model_build_report <- function(run, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_report()")
}

#' Cancel a build run
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @export
vmx_model_build_cancel <- function(run, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_cancel()")
}

# ---- Fits ------------------------------------------------------------------

#' List model fits
#' @param run Optional build-run filter.
#' @param data_version Optional data-version filter.
#' @param model_type Optional model-type filter.
#' @param marker_name Optional marker filter.
#' @param status Optional status filter.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_model_fits <- function(run = NULL, data_version = NULL, model_type = NULL,
                           marker_name = NULL, status = NULL,
                           client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_fits()")
}

#' Fetch one model fit
#' @param id A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A `vmx_model_fit`.
#' @export
vmx_model_fit <- function(id, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_fit()")
}

#' Subject-level parameter estimates
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_fit_subject_estimates <- function(fit, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_fit_subject_estimates()")
}

#' Global (population) parameter estimates
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_fit_global_estimates <- function(fit, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_fit_global_estimates()")
}

#' Observed-vs-predicted diagnostics
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A ggplot-ready tibble.
#' @export
vmx_fit_obs_vs_pred <- function(fit, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_fit_obs_vs_pred()")
}

#' Visual predictive check
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A ggplot-ready tibble.
#' @export
vmx_fit_vpc <- function(fit, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_fit_vpc()")
}
