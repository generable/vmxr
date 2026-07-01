# Async polling — the vmx_wait() generic and per-type pollers.

# Terminal prep states (see vmx-api: formatter writes these). A dataset whose
# prep settles at `awaiting_input` needs the caller to answer prompts
# (vmx_prep_answer); the others are end states.
.vmx_prep_success <- c("formatted", "awaiting_input")
.vmx_prep_failed <- c("ineligible", "failed", "cancelled")

#' Block until an async handle reaches a terminal state
#'
#' An S3 generic dispatching on the handle type. Each method has a sensible
#' default terminal state. Terminal-but-unsuccessful states raise a classed
#' error so scripts fail loudly rather than hang.
#'
#' @param x A pollable handle: a dataset / prep-status (more types as the API
#'   surface lands: model-build-run, simulation-job).
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
  vmx_poll_prep(vmx_id(x, "ds", arg = "x"), until, timeout, interval, progress, client)
}

#' @export
vmx_wait.vmx_prep_status <- function(x, until = NULL, timeout = 900, interval = 5,
                                     progress = interactive(), client = vmx_client(), ...) {
  vmx_poll_prep(vmx_id(x, "ds", arg = "x"), until, timeout, interval, progress, client)
}

#' Poll a dataset's prep-status until terminal (or `until`)
#' @keywords internal
#' @noRd
vmx_poll_prep <- function(ds_id, until, timeout, interval, progress, client) {
  stop_at <- until %||% c(.vmx_prep_success, .vmx_prep_failed)
  deadline <- Sys.time() + timeout
  wait <- interval
  repeat {
    ps <- vmx_prep_status(ds_id, client = client)
    status <- ps$status %||% ""
    if (status %in% stop_at) {
      if (is.null(until) && status %in% .vmx_prep_failed) {
        vmx_abort(
          sprintf("Dataset %s reached terminal status '%s'.", ds_id, status),
          class = "vmx_api_error", status = status, data = unclass(ps)
        )
      }
      return(ps)
    }
    if (Sys.time() >= deadline) {
      vmx_abort(
        sprintf("Timed out after %gs waiting on %s; last status '%s'.",
                timeout, ds_id, status),
        class = "vmx_timeout_error", status = status
      )
    }
    if (isTRUE(progress)) {
      cli::cli_alert_info("{ds_id}: status '{status}', polling again in {round(wait)}s...")
    }
    Sys.sleep(min(wait, max(0, as.numeric(deadline - Sys.time(), units = "secs"))))
    wait <- min(wait * 2, 30)
  }
}
