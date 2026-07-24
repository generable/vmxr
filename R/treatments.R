# Treatments — thin resource verbs over /treatments.

#' List treatments
#' @param status Optional status filter.
#' @param cursor Opaque cursor returned by [vmx_next_cursor()].
#' @param limit Server page-size hint (1--200).
#' @param client A `vmx_client`.
#' @return One server-owned page as a tibble. Use [vmx_next_cursor()] and
#'   [vmx_has_next_page()] to traverse further pages.
#' @export
vmx_treatments <- function(status = NULL, client = vmx_client(),
                           cursor = NULL, limit = NULL) {
  vmx_get_page(
    client,
    "/treatments",
    list(status = status, cursor = cursor, limit = limit)
  )
}

#' Fetch one treatment
#' @param id A treatment id (`tmt_...`) or `vmx_treatment` object.
#' @param client A `vmx_client`.
#' @return A `vmx_treatment`.
#' @export
vmx_treatment <- function(id, client = vmx_client()) {
  treatment_id <- vmx_id(id, "tmt")
  data <- vmx_get(client, paste0("/treatments/", treatment_id))
  vmx_validate_response_id(data, "treatment_id", treatment_id, "treatment")
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
  # Do not compact this body: an explicitly supplied NULL is JSON null and
  # clears nullable fields; an omitted argument is absent from list(...).
  body <- list(...)
  vmx_validate_update_body(
    body,
    allowed = c("name", "indication", "description", "status"),
    nullable = c("indication", "description"),
    resource = "treatment"
  )
  treatment_id <- vmx_id(id, "tmt")
  data <- vmx_put(client, paste0("/treatments/", treatment_id), body)
  vmx_validate_response_id(
    data, "treatment_id", treatment_id, "treatment update"
  )
  new_vmx_resource(data, "vmx_treatment", "treatment_id")
}

vmx_validate_update_body <- function(body, allowed, nullable, resource) {
  fields <- names(body)
  if (is.null(fields) || any(!nzchar(fields)) || anyDuplicated(fields)) {
    vmx_abort(
      sprintf("The %s update must use unique named fields.", resource),
      class = "vmx_usage_error"
    )
  }
  unknown <- setdiff(fields, allowed)
  if (length(unknown)) {
    vmx_abort(
      sprintf(
        "Unknown %s update field%s: %s.",
        resource,
        if (length(unknown) == 1L) "" else "s",
        paste(unknown, collapse = ", ")
      ),
      class = "vmx_usage_error"
    )
  }
  invalid_null <- fields[vapply(body, is.null, logical(1)) & !fields %in% nullable]
  if (length(invalid_null)) {
    vmx_abort(
      sprintf(
        "%s update field%s cannot be NULL: %s.",
        tools::toTitleCase(resource),
        if (length(invalid_null) == 1L) "" else "s",
        paste(invalid_null, collapse = ", ")
      ),
      class = "vmx_usage_error"
    )
  }
  invisible(body)
}
