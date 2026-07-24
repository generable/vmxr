# Studies — thin resource verbs over /studies.

#' List studies for a treatment
#' @param treatment A treatment id (`tmt_...`) or `vmx_treatment`; `NULL` lists
#'   across all treatments.
#' @param status Optional status filter.
#' @param cursor Opaque cursor returned by [vmx_next_cursor()].
#' @param limit Server page-size hint (1--200).
#' @param client A `vmx_client`.
#' @return One server-owned page as a tibble.
#' @export
vmx_studies <- function(treatment = NULL, status = NULL, client = vmx_client(),
                        cursor = NULL, limit = NULL) {
  params <- list(
    treatment_id = vmx_opt_id(treatment, "tmt", "treatment"),
    status = status,
    cursor = cursor,
    limit = limit
  )
  vmx_get_page(client, "/studies", params)
}

#' Fetch one study
#' @param id A study id (`std_...`) or `vmx_study` object.
#' @param client A `vmx_client`.
#' @return A `vmx_study`.
#' @export
vmx_study <- function(id, client = vmx_client()) {
  study_id <- vmx_id(id, "std")
  data <- vmx_get(client, paste0("/studies/", study_id))
  vmx_validate_response_id(data, "study_id", study_id, "study")
  new_vmx_resource(data, "vmx_study", "study_id")
}

#' Create a study
#' @param treatment A treatment id or `vmx_treatment`.
#' @param name Study name.
#' @param study_type Study type; defaults to `"clinical"`.
#' @param phase Optional clinical phase.
#' @param ... Additional fields (`description`, `route_of_administration`,
#'   `pd_markers`).
#' @param client A `vmx_client`.
#' @return A `vmx_study`.
#' @export
vmx_study_create <- function(treatment, name, study_type = "clinical",
                             phase = NULL, ..., client = vmx_client()) {
  body <- vmx_compact(c(
    list(
      treatment_id = vmx_id(treatment, "tmt", "treatment"),
      name = name,
      study_type = study_type,
      phase = phase
    ),
    list(...)
  ))
  new_vmx_resource(vmx_post(client, "/studies", body), "vmx_study", "study_id")
}

#' Update a study
#'
#' Only the fields you pass are changed (the server applies `exclude_unset`).
#'
#' @param id A study id or `vmx_study`.
#' @param ... Fields to update.
#' @param client A `vmx_client`.
#' @return A `vmx_study`.
#' @export
vmx_study_update <- function(id, ..., client = vmx_client()) {
  # Preserve explicit NULL so callers can clear nullable set-the-field values
  # such as route_of_administration; omitted arguments remain absent.
  body <- list(...)
  vmx_validate_update_body(
    body,
    allowed = c(
      "name", "description", "study_type", "phase", "status",
      "route_of_administration", "pd_markers"
    ),
    nullable = c(
      "description", "study_type", "phase", "route_of_administration"
    ),
    resource = "study"
  )
  if ("pd_markers" %in% names(body) &&
      (!is.list(body$pd_markers) || !is.null(names(body$pd_markers)))) {
    vmx_abort(
      "`pd_markers` must be an array of marker objects; use `list()` to clear it.",
      class = "vmx_usage_error"
    )
  }
  study_id <- vmx_id(id, "std")
  data <- vmx_put(client, paste0("/studies/", study_id), body)
  vmx_validate_response_id(data, "study_id", study_id, "study update")
  new_vmx_resource(data, "vmx_study", "study_id")
}
