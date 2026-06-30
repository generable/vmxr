# Studies — thin resource verbs over /studies.

#' List studies for a treatment
#' @param treatment A treatment id or `vmx_treatment`.
#' @param status Optional status filter.
#' @param client A `vmx_client`.
#' @return A tibble, one row per study.
#' @export
vmx_studies <- function(treatment, status = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_studies()")
}

#' Fetch one study
#' @param id A study id (`std_...`) or `vmx_study` object.
#' @param client A `vmx_client`.
#' @return A `vmx_study`.
#' @export
vmx_study <- function(id, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_study()")
}

#' Create a study
#' @param treatment A treatment id or `vmx_treatment`.
#' @param name Study name.
#' @param study_type Study type; defaults to `"clinical"`.
#' @param phase Optional clinical phase.
#' @param ... Additional fields.
#' @param client A `vmx_client`.
#' @return A `vmx_study`.
#' @export
vmx_study_create <- function(treatment, name, study_type = "clinical",
                             phase = NULL, ..., client = vmx_client()) {
  vmx_abort_unimplemented("vmx_study_create()")
}

#' Update a study
#' @param id A study id or `vmx_study`.
#' @param ... Fields to update.
#' @param client A `vmx_client`.
#' @return A `vmx_study`.
#' @export
vmx_study_update <- function(id, ..., client = vmx_client()) {
  vmx_abort_unimplemented("vmx_study_update()")
}
