# Studies — thin resource verbs over /studies.

#' List studies for a treatment
#' @param treatment A treatment id (`tmt_...`) or `vmx_treatment`; `NULL` lists
#'   across all treatments.
#' @param status Optional status filter.
#' @param client A `vmx_client`.
#' @return A tibble, one row per study.
#' @export
vmx_studies <- function(treatment = NULL, status = NULL, client = vmx_client()) {
  params <- list(
    treatment_id = vmx_opt_id(treatment, "tmt", "treatment"),
    status = status
  )
  vmx_items_to_tibble(vmx_paginate(client, "/studies", params))
}

#' Fetch one study
#' @param id A study id (`std_...`) or `vmx_study` object.
#' @param client A `vmx_client`.
#' @return A `vmx_study`.
#' @export
vmx_study <- function(id, client = vmx_client()) {
  data <- vmx_get(client, paste0("/studies/", vmx_id(id, "std")))
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
  body <- vmx_compact(list(...))
  data <- vmx_put(client, paste0("/studies/", vmx_id(id, "std")), body)
  new_vmx_resource(data, "vmx_study", "study_id")
}
