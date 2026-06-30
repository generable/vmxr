# Low-level httr2 request/response plumbing: auth header, retries, JSON
# parsing, cursor pagination. The generated bindings and the ergonomic verbs
# both route through here.

#' Build a base request for a client
#'
#' @param client A `vmx_client`.
#' @param path Request path appended to the client's `base_url`.
#' @keywords internal
#' @noRd
vmx_req <- function(client, path) {
  vmx_abort_unimplemented("vmx_req()")
}

#' Perform a request and parse the JSON body
#'
#' Sends the request, maps HTTP errors onto the `vmx_error` hierarchy, and
#' returns the parsed list.
#'
#' @keywords internal
#' @noRd
vmx_perform <- function(req, ...) {
  vmx_abort_unimplemented("vmx_perform()")
}

#' Follow `next_cursor` pagination and bind pages into one tibble
#'
#' @keywords internal
#' @noRd
vmx_paginate <- function(client, path, query = list(), ...) {
  vmx_abort_unimplemented("vmx_paginate()")
}
