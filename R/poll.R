# Async polling — the vmx_wait() generic and per-type pollers.

# Terminal states per resource. "success" states are returned; "failed" states
# raise a classed error (unless the caller pinned an explicit `until`).
.vmx_prep_success <- c("formatted", "awaiting_input")
.vmx_prep_failed <- c("ineligible", "failed", "cancelled")
.vmx_nca_success <- c("completed", "degraded")
.vmx_nca_failed <- c("failed")

#' Block until an async handle reaches a terminal state
#'
#' An S3 generic dispatching on the handle type. Each method has a sensible
#' default terminal state. Terminal-but-unsuccessful states raise a classed
#' error so scripts fail loudly rather than hang.
#'
#' @param x A pollable handle: a dataset / prep-status or an NCA analysis (more
#'   types as the API surface lands: model-build-run, simulation-job).
#' @param until Target terminal state(s); a sensible default per type when
#'   `NULL`.
#' @param timeout Timeout in seconds.
#' @param interval Poll interval in seconds (exponential backoff up to 30s).
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
vmx_wait.vmx_dataset <- function(x, until = NULL, timeout = 900, interval = 5,
                                 progress = interactive(), client = vmx_client(), ...) {
  id <- vmx_id(x, "ds", arg = "x")
  vmx_poll_status(id, function(i) vmx_prep_status(i, client = client),
                  .vmx_prep_success, .vmx_prep_failed,
                  until, timeout, interval, progress, "Dataset")
}

#' @export
vmx_wait.vmx_prep_status <- function(x, until = NULL, timeout = 900, interval = 5,
                                     progress = interactive(), client = vmx_client(), ...) {
  id <- vmx_id(x, "ds", arg = "x")
  vmx_poll_status(id, function(i) vmx_prep_status(i, client = client),
                  .vmx_prep_success, .vmx_prep_failed,
                  until, timeout, interval, progress, "Dataset")
}

#' @export
vmx_wait.vmx_nca_analysis <- function(x, until = NULL, timeout = 900, interval = 5,
                                      progress = interactive(), client = vmx_client(), ...) {
  id <- vmx_id(x, "nca", arg = "x")
  vmx_poll_status(id, function(i) vmx_nca_get(i, client = client),
                  .vmx_nca_success, .vmx_nca_failed,
                  until, timeout, interval, progress, "NCA analysis")
}

#' Generic status poller with exponential backoff
#'
#' @param id Resource id.
#' @param fetch A function taking `id` and returning the refreshed object
#'   (something with a `$status`).
#' @param success,failed Character vectors of terminal states.
#' @param until Explicit target state(s), overriding the defaults.
#' @param label Human label for messages.
#' @keywords internal
#' @noRd
vmx_poll_status <- function(id, fetch, success, failed, until, timeout,
                            interval, progress, label) {
  stop_at <- until %||% c(success, failed)
  deadline <- Sys.time() + timeout
  wait <- interval
  repeat {
    obj <- fetch(id)
    status <- obj$status %||% ""
    if (status %in% stop_at) {
      if (is.null(until) && status %in% failed) {
        vmx_abort(
          sprintf("%s %s reached terminal status '%s'.", label, id, status),
          class = "vmx_api_error", status = status, data = unclass(obj)
        )
      }
      return(obj)
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
