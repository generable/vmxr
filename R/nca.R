# NCA — non-compartmental analysis verbs.

#' List NCA analyses
#' @param data_version Optional data-version (`dv_...`) filter.
#' @param study Optional study (`std_...`) filter.
#' @param treatment Optional treatment (`tmt_...`) filter.
#' @param status Optional status filter (`queued`/`running`/`completed`/
#'   `degraded`/`failed`).
#' @param time_basis Optional time-basis filter.
#' @param client A `vmx_client`.
#' @return A tibble containing all matching analyses.
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
  vmx_paginate(client, "/nca-analyses", params)
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
  time_basis <- vmx_nonempty_strings(
    time_basis, "time_basis", exactly_one = TRUE
  )
  if (!is.null(idempotency_key)) {
    vmx_id_like_scalar(idempotency_key, "idempotency_key")
  }
  body <- vmx_compact(list(
    data_version_id = vmx_id(data_version, "dv", "data_version"),
    time_basis = time_basis,
    idempotency_key = idempotency_key,
    retried_from = vmx_opt_id(retried_from, "nca", "retried_from")
  ))
  data <- vmx_post(client, "/nca-analyses", body)
  vmx_validate_response_id(
    data, "data_version_id", body$data_version_id, "NCA creation"
  )
  nca <- new_vmx_resource(data, "vmx_nca_analysis", "nca_id")
  if (isTRUE(wait)) vmx_wait(nca, client = client, ...) else nca
}

#' Fetch one NCA analysis
#' @param id An NCA id (`nca_...`) or `vmx_nca_analysis`.
#' @param client A `vmx_client`.
#' @return A `vmx_nca_analysis`.
#' @export
vmx_nca_get <- function(id, client = vmx_client()) {
  nca_id <- vmx_id(id, "nca")
  data <- vmx_get(client, paste0("/nca-analyses/", nca_id))
  vmx_validate_response_id(data, "nca_id", nca_id, "NCA analysis")
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
  nca_id <- vmx_id(nca, "nca")
  res <- vmx_get(client, paste0("/nca-analyses/", nca_id, "/result"))
  vmx_validate_response_id(res, "nca_id", nca_id, "NCA result")
  gen_subject_uuid <- vmx_response_vector(
    vmx_response_field(res, "gen_subject_uuid", "NCA result.gen_subject_uuid"),
    "NCA result.gen_subject_uuid",
    type = "character"
  )
  if (anyDuplicated(gen_subject_uuid)) {
    vmx_abort_response(
      "field 'NCA result.gen_subject_uuid' contains duplicate subject keys.",
      field = "gen_subject_uuid"
    )
  }
  n <- length(gen_subject_uuid)
  subject_id <- vmx_response_vector(
    vmx_response_field(res, "subject_id", "NCA result.subject_id"),
    "NCA result.subject_id",
    type = "character",
    size = n
  )
  estimates <- vmx_response_field(res, "point_estimates", "NCA result.point_estimates")
  if (!is.list(estimates) || is.null(names(estimates)) ||
      any(!nzchar(names(estimates))) || anyDuplicated(names(estimates))) {
    vmx_abort_response(
      "field 'NCA result.point_estimates' must be an object.",
      field = "point_estimates"
    )
  }
  reserved <- intersect(
    names(estimates), c("subject_id", "gen_subject_uuid")
  )
  if (length(reserved)) {
    vmx_abort_response(
      "NCA metric name conflicts with a subject-identity column.",
      field = paste0("point_estimates.", reserved[[1]])
    )
  }
  cols <- list(
    subject_id = subject_id,
    gen_subject_uuid = gen_subject_uuid
  )
  for (metric in names(estimates)) {
    cols[[metric]] <- vmx_response_vector(
      estimates[[metric]],
      paste0("NCA result.point_estimates.", metric),
      type = "numeric",
      size = n,
      nullable = TRUE
    )
  }
  out <- tibble::as_tibble(cols)
  metadata <- res[setdiff(
    names(res),
    c("gen_subject_uuid", "subject_id", "point_estimates")
  )]
  attr(out, "vmx_metadata") <- metadata
  for (name in c(
    "nca_id", "data_version_id", "status", "time_basis", "units",
    "quantities", "excluded_subjects", "worker_version", "trigger_source",
    "retried_from"
  )) {
    if (name %in% names(res)) attr(out, name) <- res[[name]]
  }
  out
}
