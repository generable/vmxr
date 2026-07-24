# High-level orchestration verbs — the value layer that hides async polling
# and pagination. These compose the resource verbs in R/*.R; they hold no
# server semantics of their own.

#' Audit / analysis log for a study
#'
#' `GET /studies/{std_id}/analysis-log` — one server-owned page of the unified
#' newest-first event feed, with a `kind` discriminator per row.
#'
#' @param study A study id (`std_...`) or `vmx_study`.
#' @param kind Optional kind filter.
#' @param event_type Optional event-type filter.
#' @param event_code Optional event-code filter.
#' @param outcome Optional outcome filter.
#' @param severity Optional severity filter.
#' @param requires_staff_review Optional staff-review filter.
#' @param since Optional lower time bound: a `POSIXct`/`Date` (formatted to
#'   ISO-8601 UTC) or an ISO-8601 string.
#' @param resource Optional resource id (or object) to scope to.
#' @param cursor Opaque cursor returned by [vmx_next_cursor()].
#' @param limit Server page-size hint (1--200).
#' @param client A `vmx_client`.
#' @return One newest-first server-owned page as a tibble.
#' @export
vmx_analysis_log <- function(study, kind = NULL, event_type = NULL,
                             outcome = NULL, severity = NULL,
                             since = NULL, resource = NULL,
                             client = vmx_client(), event_code = NULL,
                             requires_staff_review = NULL, cursor = NULL,
                             limit = NULL) {
  study_id <- vmx_id(study, "std")
  resource_id <- if (inherits(resource, "vmx_resource")) vmx_resource_id(resource) else resource
  params <- list(
    kind = kind,
    event_type = event_type,
    event_code = event_code,
    outcome = outcome,
    severity = severity,
    requires_staff_review = requires_staff_review,
    since = vmx_format_time(since),
    resource_id = resource_id,
    cursor = cursor,
    limit = limit
  )
  path <- paste0("/studies/", study_id, "/analysis-log")
  params <- vmx_compact(params)
  if (!is.null(params$cursor)) {
    vmx_id_like_scalar(params$cursor, "cursor")
  }
  if (!is.null(params$limit)) {
    # Reuse the generic validation without issuing a second request.
    vmx_get_page_params(params)
  }
  page <- vmx_get(client, path, params)
  vmx_validate_response_id(
    page, "study_id", study_id, "analysis log"
  )
  vmx_page_to_tibble(page, context = path)
}

# Format a time filter to ISO-8601 UTC; pass character through unchanged.
vmx_format_time <- function(x, arg = "since") {
  if (is.null(x)) return(NULL)
  if (inherits(x, c("POSIXct", "POSIXt", "Date"))) {
    if (length(x) != 1L || is.na(x)) {
      vmx_abort(
        sprintf("`%s` must be one non-missing date-time.", arg),
        class = "vmx_usage_error"
      )
    }
    return(format(
      as.POSIXct(x, tz = "UTC"),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ))
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !nzchar(trimws(x))) {
    vmx_abort(
      sprintf(
        "`%s` must be one non-empty ISO-8601 string or date-time.",
        arg
      ),
      class = "vmx_usage_error"
    )
  }
  x
}
