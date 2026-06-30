# NCA — non-compartmental analysis verbs.

#' List NCA analyses
#' @param data_version Optional data-version filter.
#' @param study Optional study filter.
#' @param treatment Optional treatment filter.
#' @param status Optional status filter.
#' @param time_basis Optional time-basis filter.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_nca_analyses <- function(data_version = NULL, study = NULL, treatment = NULL,
                             status = NULL, time_basis = NULL,
                             client = vmx_client()) {
  vmx_abort_unimplemented("vmx_nca_analyses()")
}

#' Create an NCA analysis (optionally wait for it)
#' @param data_version A data-version id or `vmx_data_version`.
#' @param time_basis `"actual"` or `"nominal"`.
#' @param ... Additional NCA options.
#' @param wait If `TRUE`, block until the analysis settles.
#' @param client A `vmx_client`.
#' @return A `vmx_nca_analysis`.
#' @export
vmx_nca <- function(data_version, time_basis, ..., wait = TRUE,
                    client = vmx_client()) {
  vmx_abort_unimplemented("vmx_nca()")
}

#' Fetch one NCA analysis
#' @param id An NCA id or `vmx_nca_analysis`.
#' @param client A `vmx_client`.
#' @return A `vmx_nca_analysis`.
#' @export
vmx_nca_get <- function(id, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_nca_get()")
}

#' NCA result table (PK parameters)
#' @param nca An NCA id or `vmx_nca_analysis`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_nca_result <- function(nca, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_nca_result()")
}
