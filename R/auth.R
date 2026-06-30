# PAT resolution and identity probes.

#' Confirm the configured PAT and report identity
#'
#' @param client A `vmx_client`.
#' @return A list with the authenticated user, email, and related fields.
#' @export
vmx_whoami <- function(client = vmx_client()) {
  vmx_abort_unimplemented("vmx_whoami()")
}

#' Connectivity probe
#'
#' @param client A `vmx_client`.
#' @return `TRUE` on success, otherwise a classed error.
#' @export
vmx_health <- function(client = vmx_client()) {
  vmx_abort_unimplemented("vmx_health()")
}
