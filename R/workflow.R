# High-level orchestration verbs — the value layer that hides async polling
# and pagination. These compose the resource verbs in R/*.R; they hold no
# server semantics of their own.

#' Audit / analysis log for a study
#' @param study A study id or `vmx_study`.
#' @param kind Optional kind filter.
#' @param event_type Optional event-type filter.
#' @param outcome Optional outcome filter.
#' @param severity Optional severity filter.
#' @param since Optional lower time bound.
#' @param resource Optional resource filter.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_analysis_log <- function(study, kind = NULL, event_type = NULL,
                             outcome = NULL, severity = NULL, since = NULL,
                             resource = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_analysis_log()")
}
