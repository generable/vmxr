# NCA — non-compartmental analysis verbs.

#' List NCA analyses
#' @param data_version Optional data-version (`dv_...`) filter.
#' @param study Optional study (`std_...`) filter.
#' @param treatment Optional treatment (`tmt_...`) filter.
#' @param status Optional status filter (`queued`/`running`/`completed`/
#'   `degraded`/`failed`).
#' @param time_basis Optional time-basis filter.
#' @param client A `vmx_client`.
#' @return A tibble, one row per analysis.
#' @export
vmx_nca_analyses <- function(data_version = NULL, study = NULL, treatment = NULL,
                             status = NULL, time_basis = NULL,
                             client = vmx_client()) {
  params <- list(
    data_version_id = vmx_opt_id(data_version, "dv", "data_version"),
    study_id = vmx_opt_id(study, "std", "study"),
    treatment_id = vmx_opt_id(treatment, "tmt", "treatment"),
    status = status,
    time_basis = time_basis
  )
  vmx_items_to_tibble(vmx_paginate(client, "/nca-analyses", params))
}

#' Run an NCA analysis
#'
#' Creates the analysis (`POST /nca-analyses`) and, by default, blocks until it
#' settles. `time_basis` is one of `"observed"`, `"nominal"`, or
#' `"nominal_from_observed_dose"` (validated server-side against the
#' DataVersion's available bases).
#'
#' @param data_version A data-version id (`dv_...`) or `vmx_data_version`.
#' @param time_basis The time basis to compute on.
#' @param idempotency_key,retried_from Optional create fields.
#' @param wait If `TRUE` (default), block until the analysis is terminal.
#' @param ... Polling controls forwarded to [vmx_wait()] when `wait = TRUE`
#'   (e.g. `timeout`, `interval`, `progress`).
#' @param client A `vmx_client`.
#' @return A `vmx_nca_analysis`.
#' @export
vmx_nca <- function(data_version, time_basis, idempotency_key = NULL,
                    retried_from = NULL, wait = TRUE, ...,
                    client = vmx_client()) {
  body <- vmx_compact(list(
    data_version_id = vmx_id(data_version, "dv", "data_version"),
    time_basis = time_basis,
    idempotency_key = idempotency_key,
    retried_from = retried_from
  ))
  nca <- new_vmx_resource(vmx_post(client, "/nca-analyses", body),
                          "vmx_nca_analysis", "nca_id")
  if (isTRUE(wait)) vmx_wait(nca, client = client, ...) else nca
}

#' Fetch one NCA analysis
#' @param id An NCA id (`nca_...`) or `vmx_nca_analysis`.
#' @param client A `vmx_client`.
#' @return A `vmx_nca_analysis`.
#' @export
vmx_nca_get <- function(id, client = vmx_client()) {
  data <- vmx_get(client, paste0("/nca-analyses/", vmx_id(id, "nca")))
  new_vmx_resource(data, "vmx_nca_analysis", "nca_id")
}

#' NCA result table (PK parameters, one row per subject)
#'
#' Reshapes the `point_estimates` payload (metric -> per-subject values, parallel
#' to the subject arrays) into a tidy tibble: `subject_id`, `gen_subject_uuid`,
#' and one column per PK quantity. The quantity metadata (display names, units,
#' explanations) is attached as the `"quantities"` attribute.
#'
#' @param nca An NCA id or `vmx_nca_analysis`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_nca_result <- function(nca, client = vmx_client()) {
  res <- vmx_get(client, paste0("/nca-analyses/", vmx_id(nca, "nca"), "/result"))
  cols <- list(
    subject_id = vmx_chr(res$subject_id),
    gen_subject_uuid = vmx_chr(res$gen_subject_uuid)
  )
  for (metric in names(res$point_estimates)) {
    cols[[metric]] <- vmx_num(res$point_estimates[[metric]])
  }
  out <- tibble::as_tibble(cols)
  attr(out, "quantities") <- res$quantities
  out
}

# Coerce a parsed JSON array (list of scalars, possibly with JSON nulls) to a
# vector. A null parses to NULL or an empty list depending on the encoder, so
# treat any length-0 element as a missing value.
vmx_chr <- function(x) {
  if (!length(x)) return(character(0))
  vapply(x, function(v) if (length(v) == 0) NA_character_ else as.character(v[[1]]), character(1))
}
vmx_num <- function(x) {
  if (!length(x)) return(numeric(0))
  vapply(x, function(v) if (length(v) == 0) NA_real_ else as.numeric(v[[1]]), numeric(1))
}
