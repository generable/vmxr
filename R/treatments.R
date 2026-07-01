# Treatments — thin resource verbs over /treatments.

#' List treatments
#' @param status Optional status filter.
#' @param client A `vmx_client`.
#' @return A tibble, one row per treatment.
#' @export
vmx_treatments <- function(status = NULL, client = vmx_client()) {
  items <- vmx_paginate(client, "/treatments", list(status = status))
  vmx_items_to_tibble(items)
}

#' Fetch one treatment
#' @param id A treatment id (`tmt_...`) or `vmx_treatment` object.
#' @param client A `vmx_client`.
#' @return A `vmx_treatment`.
#' @export
vmx_treatment <- function(id, client = vmx_client()) {
  data <- vmx_get(client, paste0("/treatments/", vmx_id(id, "tmt")))
  new_vmx_resource(data, "vmx_treatment", "treatment_id")
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
  body <- vmx_compact(list(name = name, indication = indication,
                           description = description))
  new_vmx_resource(vmx_post(client, "/treatments", body), "vmx_treatment", "treatment_id")
}

#' Update a treatment
#'
#' Only the fields you pass are changed (the server applies `exclude_unset`).
#'
#' @param id A treatment id or `vmx_treatment`.
#' @param ... Fields to update (`name`, `indication`, `description`, `status`).
#' @param client A `vmx_client`.
#' @return A `vmx_treatment`.
#' @export
vmx_treatment_update <- function(id, ..., client = vmx_client()) {
  body <- vmx_compact(list(...))
  data <- vmx_put(client, paste0("/treatments/", vmx_id(id, "tmt")), body)
  new_vmx_resource(data, "vmx_treatment", "treatment_id")
}
