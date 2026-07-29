# Condition classes for the vmxr client.
#
# Hierarchy (see design doc, section 9):
#   vmx_error (parent)
#     |- vmx_auth_error     401 / unauthenticated
#     |- vmx_api_error      4xx/5xx with server `reason` + body
#     |- vmx_response_error malformed successful API response
#     |- vmx_timeout_error  vmx_wait() exceeded its timeout
#     |- vmx_usage_error    client-side validation (e.g. bad id prefix)

#' Raise a classed vmxr error
#'
#' Internal constructor for the `vmx_error` condition hierarchy. Each public
#' helper sets the appropriate subclass so callers can dispatch with
#' [base::tryCatch()].
#'
#' @param message Human-readable error message.
#' @param class Character vector of subclasses prepended to `vmx_error`.
#' @param ... Additional data stored on the condition (e.g. `status`, `body`,
#'   `reason`).
#' @param call The calling environment, for the error backtrace.
#' @keywords internal
#' @noRd
vmx_abort <- function(message, class = character(), ..., call = rlang::caller_env()) {
  rlang::abort(
    message,
    class = c(class, "vmx_error"),
    ...,
    call = call
  )
}

# Raise when a successful response violates the public API contract. Messages
# name only the response field and expected shape; they never include payload
# values, which may contain sensitive study data.
vmx_abort_response <- function(message, field = NULL, data = NULL,
                               call = rlang::caller_env()) {
  vmx_abort(
    paste0("Invalid response from the VeloMetrix API: ", message),
    class = "vmx_response_error",
    reason = "invalid_response",
    field = field,
    data = data,
    call = call
  )
}

# Convenience stub used by not-yet-implemented public verbs.
vmx_abort_unimplemented <- function(what = NULL, call = rlang::caller_env()) {
  what <- what %||% "This function"
  vmx_abort(
    sprintf("%s is part of the vmxr skeleton and is not implemented yet.", what),
    class = "vmx_unimplemented_error",
    call = call
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
