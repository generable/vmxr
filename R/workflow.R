# High-level orchestration verbs — the value layer that hides async polling
# and pagination. These compose the resource verbs in R/*.R; they hold no
# server semantics of their own.

#' Audit / analysis log for a study
#'
#' `GET /studies/{std_id}/analysis-log` — the unified newest-first event feed,
#' auto-paginated into a tibble with a `kind` discriminator per row.
#'
#' @param study A study id (`std_...`) or `vmx_study`.
#' @param kind Optional kind filter.
#' @param event_type Optional event-type filter.
#' @param outcome Optional outcome filter.
#' @param severity Optional severity filter.
#' @param since Optional lower time bound: a `POSIXct`/`Date` (formatted to
#'   ISO-8601 UTC) or an ISO-8601 string.
#' @param resource Optional resource id (or object) to scope to.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_analysis_log <- function(study, kind = NULL, event_type = NULL,
                             outcome = NULL, severity = NULL, since = NULL,
                             resource = NULL, client = vmx_client()) {
  resource_id <- if (inherits(resource, "vmx_resource")) vmx_resource_id(resource) else resource
  params <- list(
    kind = kind,
    event_type = event_type,
    outcome = outcome,
    severity = severity,
    since = vmx_format_time(since),
    resource_id = resource_id
  )
  vmx_items_to_tibble(vmx_paginate(client, paste0("/studies/", vmx_id(study, "std"), "/analysis-log"), params))
}

# Format a time filter to ISO-8601 UTC; pass character through unchanged.
vmx_format_time <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, c("POSIXct", "POSIXt", "Date"))) {
    format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else {
    as.character(x)
  }
}
