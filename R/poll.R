# Async polling — the vmx_wait() generic and per-type pollers.

# Terminal states per resource. "success" states are returned; "failed" states
# raise a classed error (unless the caller pinned an explicit `until`).
.vmx_prep_success <- c("formatted", "awaiting_input")
.vmx_prep_failed <- c("ineligible", "failed", "cancelled")
.vmx_nca_success <- c("completed", "degraded")
.vmx_nca_failed <- c("failed")
.vmx_build_success <- c("succeeded", "degraded")
.vmx_build_failed <- c("failed", "cancelled")
.vmx_dosing_success <- c("succeeded")
.vmx_dosing_failed <- c("failed")
.vmx_sim_success <- c("succeeded")
.vmx_sim_failed <- c("failed", "cancelled")

.vmx_prep_statuses <- c(
  "uploaded", "queued", "formatting", "awaiting_input", "formatted",
  "ineligible", "failed", "cancelled"
)
.vmx_nca_statuses <- c("queued", "running", "completed", "degraded", "failed")
.vmx_build_statuses <- c(
  "queued", "validating", "running", "cancelling", "succeeded", "degraded",
  "failed", "cancelled"
)
.vmx_dosing_statuses <- c("queued", "running", "succeeded", "failed")
.vmx_sim_statuses <- c(
  "queued", "running", "cancelling", "succeeded", "failed", "cancelled"
)

#' Block until an async handle reaches a terminal state
#'
#' An S3 generic dispatching on the handle type. Each method has a sensible
#' default terminal state. Terminal-but-unsuccessful states raise a classed
#' error so scripts fail loudly rather than hang.
#'
#' @param x A pollable handle: a dataset / prep-status or an NCA analysis (more
#'   types as the API surface lands: model-build-run, simulation-job).
#' @param until Target terminal state(s); a sensible default per type when
#'   `NULL`. An explicitly requested failure state is returned; any other
#'   terminal failure still raises immediately.
#' @param timeout Timeout in seconds. Resource methods use long-running defaults:
#'   70 minutes for prep, NCA, and dosing input; 130 minutes for simulation; and
#'   24 hours 10 minutes for model builds. The NCA/modeling worker defaults
#'   include a short persistence cushion beyond their execution ceilings; prep
#'   uses a client-side wait policy because its worker has no equivalent hard
#'   wall-clock ceiling.
#' @param interval Positive poll interval in seconds (exponential backoff up
#'   to 30s).
#' @param progress Show a progress message each poll; defaults to
#'   [interactive()].
#' @param client A `vmx_client`.
#' @param ... Passed to methods.
#' @return The updated object, or a `vmx_timeout_error`.
#' @export
vmx_wait <- function(x, until = NULL, timeout = 900, interval = 5,
                     progress = interactive(), client = vmx_client(), ...) {
  UseMethod("vmx_wait")
}

#' @export
vmx_wait.default <- function(x, until = NULL, timeout = 900, interval = 5,
                             progress = interactive(), client = vmx_client(), ...) {
  vmx_abort(
    sprintf("Don't know how to wait on an object of class <%s>.", class(x)[[1]]),
    class = "vmx_usage_error"
  )
}

#' @export
vmx_wait.vmx_dataset <- function(x, until = NULL, timeout = 4200, interval = 5,
                                 progress = interactive(), client = vmx_client(), ...) {
  id <- vmx_id(x, "ds", arg = "x")
  vmx_poll_status(id, function(i) vmx_prep_status(i, client = client),
                  .vmx_prep_success, .vmx_prep_failed,
                  .vmx_prep_statuses, until, timeout, interval, progress,
                  "Dataset")
}

#' @export
vmx_wait.vmx_prep_status <- function(x, until = NULL, timeout = 4200, interval = 5,
                                     progress = interactive(), client = vmx_client(), ...) {
  id <- vmx_id(x, "ds", arg = "x")
  vmx_poll_status(id, function(i) vmx_prep_status(i, client = client),
                  .vmx_prep_success, .vmx_prep_failed,
                  .vmx_prep_statuses, until, timeout, interval, progress,
                  "Dataset")
}

#' @export
vmx_wait.vmx_nca_analysis <- function(x, until = NULL, timeout = 4200, interval = 5,
                                      progress = interactive(), client = vmx_client(), ...) {
  id <- vmx_id(x, "nca", arg = "x")
  vmx_poll_status(id, function(i) vmx_nca_get(i, client = client),
                  .vmx_nca_success, .vmx_nca_failed,
                  .vmx_nca_statuses, until, timeout, interval, progress,
                  "NCA analysis")
}

#' @export
vmx_wait.vmx_model_build_run <- function(x, until = NULL, timeout = 87000, interval = 5,
                                         progress = interactive(), client = vmx_client(), ...) {
  id <- vmx_id(x, "run", arg = "x")
  vmx_poll_status(id, function(i) vmx_model_build_status(i, client = client),
                  .vmx_build_success, .vmx_build_failed,
                  .vmx_build_statuses, until, timeout, interval, progress,
                  "Model build run")
}

#' @export
vmx_wait.vmx_dosing_input <- function(x, until = NULL, timeout = 4200, interval = 5,
                                      progress = interactive(), client = vmx_client(), ...) {
  id <- vmx_dosing_input_id(x)
  vmx_poll_status(id, function(i) vmx_dosing_input_status(i, client = client),
                  .vmx_dosing_success, .vmx_dosing_failed,
                  .vmx_dosing_statuses, until, timeout, interval, progress,
                  "Dosing input")
}

#' @export
vmx_wait.vmx_simulation_job <- function(x, until = NULL, timeout = 7800, interval = 5,
                                        progress = interactive(), client = vmx_client(), ...) {
  id <- vmx_id(x, "simjob", arg = "x")
  vmx_poll_status(id, function(i) vmx_sim_status(i, client = client),
                  .vmx_sim_success, .vmx_sim_failed,
                  .vmx_sim_statuses, until, timeout, interval, progress,
                  "Simulation job")
}

#' Generic status poller with exponential backoff
#'
#' @param id Resource id.
#' @param fetch A function taking `id` and returning the refreshed object
#'   (something with a `$status`).
#' @param success,failed Character vectors of terminal states.
#' @param known Closed status vocabulary for this resource.
#' @param until Explicit target state(s), overriding the defaults.
#' @param label Human label for messages.
#' @keywords internal
#' @noRd
vmx_poll_status <- function(id, fetch, success, failed, known, until, timeout,
                            interval, progress, label) {
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
      !is.finite(timeout) || timeout <= 0) {
    vmx_abort("`timeout` must be one finite positive number.",
              class = "vmx_usage_error")
  }
  if (!is.numeric(interval) || length(interval) != 1L || is.na(interval) ||
      !is.finite(interval) || interval <= 0) {
    vmx_abort("`interval` must be one finite positive number.",
              class = "vmx_usage_error")
  }
  if (!is.null(until)) {
    if (!is.character(until) || !length(until) || anyNA(until) ||
        any(!until %in% known)) {
      vmx_abort(
        sprintf(
          "`until` must contain known %s statuses: %s.",
          tolower(label), paste(known, collapse = ", ")
        ),
        class = "vmx_usage_error"
      )
    }
  }
  targets <- until %||% success
  deadline <- Sys.time() + timeout
  wait <- interval
  repeat {
    obj <- fetch(id)
    status <- obj$status %||% ""
    if (!is.character(status) || length(status) != 1L || is.na(status) ||
        !status %in% known) {
      vmx_abort_response(
        sprintf("%s status response contains an unknown status.", label),
        field = "status"
      )
    }
    if (status %in% targets) {
      return(obj)
    }
    if (status %in% failed) {
      detail <- vmx_job_failure_detail(obj)
      suffix <- if (is.null(detail)) "" else paste0(" ", detail)
      vmx_abort(
        sprintf("%s %s reached terminal status '%s'.%s",
                label, id, status, suffix),
        class = c("vmx_job_error", "vmx_api_error"),
        resource_status = status,
        data = unclass(obj)
      )
    }
    if (status %in% success) {
      vmx_abort(
        sprintf(
          "%s %s reached terminal status '%s' before the requested status.",
          label, id, status
        ),
        class = "vmx_job_error",
        resource_status = status,
        data = unclass(obj)
      )
    }
    if (Sys.time() >= deadline) {
      vmx_abort(
        sprintf("Timed out after %gs waiting on %s; last status '%s'.",
                timeout, id, status),
        class = "vmx_timeout_error", status = status
      )
    }
    if (isTRUE(progress)) {
      cli::cli_alert_info("{id}: status '{status}', polling again in {round(wait)}s...")
    }
    Sys.sleep(min(wait, max(0, as.numeric(deadline - Sys.time(), units = "secs"))))
    wait <- min(wait * 2, 30)
  }
}

vmx_job_failure_detail <- function(obj) {
  candidates <- list(obj$failure_reason, obj$error_message)
  if (is.list(obj$progress)) {
    candidates[[length(candidates) + 1L]] <- obj$progress$message
  }
  for (candidate in candidates) {
    if (is.character(candidate) && length(candidate) == 1L &&
        !is.na(candidate) && nzchar(trimws(candidate))) {
      return(trimws(candidate))
    }
  }
  NULL
}
