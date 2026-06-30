# Async polling — the vmx_wait() generic and per-type pollers.

#' Block until an async handle reaches a terminal state
#'
#' An S3 generic dispatching on the handle type. Each method has a sensible
#' default terminal state (dataset -> `formatted`/`awaiting_input`;
#' model-build-run -> `succeeded`/`failed`; sim-job -> `succeeded`/`failed`).
#' Terminal-but-unsuccessful states raise a classed error.
#'
#' @param x A pollable handle: dataset, data-version, model-build-run, or
#'   simulation-job.
#' @param until Target terminal state(s); a sensible default per type when
#'   `NULL`.
#' @param timeout Timeout in seconds.
#' @param interval Poll interval in seconds (backoff capped at `interval`).
#' @param progress Show a progress bar; defaults to [interactive()].
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
  vmx_abort_unimplemented("vmx_wait()")
}
