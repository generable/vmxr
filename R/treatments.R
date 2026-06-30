# Treatments — thin resource verbs over /treatments.

#' List treatments
#' @param status Optional status filter.
#' @param client A `vmx_client`.
#' @return A tibble, one row per treatment.
#' @export
vmx_treatments <- function(status = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_treatments()")
}

#' Fetch one treatment
#' @param id A treatment id (`tmt_...`) or `vmx_treatment` object.
#' @param client A `vmx_client`.
#' @return A `vmx_treatment`.
#' @export
vmx_treatment <- function(id, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_treatment()")
}

#' Create a treatment
#' @param name Treatment name.
#' @param indication Optional indication.
#' @param description Optional free-text description.
#' @param client A `vmx_client`.
#' @return A `vmx_treatment`.
#' @export
vmx_treatment_create <- function(name, indication = NULL, description = NULL,
                                 client = vmx_client()) {
  vmx_abort_unimplemented("vmx_treatment_create()")
}

#' Update a treatment
#' @param id A treatment id or `vmx_treatment`.
#' @param ... Fields to update.
#' @param client A `vmx_client`.
#' @return A `vmx_treatment`.
#' @export
vmx_treatment_update <- function(id, ..., client = vmx_client()) {
  vmx_abort_unimplemented("vmx_treatment_update()")
}
