# Low-level httr2 request/response plumbing: bearer auth, JSON parsing, error
# mapping onto the vmx_error hierarchy, and cursor pagination. The resource
# verbs in R/*.R all route through here.

# API version prefix appended to every client base_url (matches the CLI).
.vmx_api_prefix <- "/api/v1"

#' Build a base request for a client
#'
#' Attaches the `/api/v1` prefix, bearer auth, and a user agent. HTTP error
#' status is handled by [vmx_perform()] (not httr2's default), so the request
#' is configured not to raise on 4xx/5xx.
#'
#' @param client A `vmx_client`.
#' @param path Request path beginning with `/` (e.g. `"/treatments"`).
#' @keywords internal
#' @noRd
vmx_req <- function(client, path) {
  # Resolve the bearer token *now*, per request, via the client's provider --
  # a reused client thus re-reads the OIDC cache and silently refreshes a
  # near-expired access token instead of sending a stale frozen one (GEN-2344).
  httr2::request(paste0(client$base_url, .vmx_api_prefix, path)) |>
    httr2::req_auth_bearer_token(client$token_provider()) |>
    httr2::req_user_agent("vmxr (https://github.com/generable/vmxr)") |>
    httr2::req_error(is_error = function(resp) FALSE)
}

#' Attach query params, dropping NULLs and lower-casing logicals
#' @keywords internal
#' @noRd
vmx_req_query <- function(req, params) {
  params <- vmx_compact(params)
  params <- lapply(params, function(v) if (is.logical(v)) tolower(as.character(v)) else v)
  if (length(params)) req <- httr2::req_url_query(req, !!!params)
  req
}

#' Perform a request, map errors, and parse the JSON body
#' @return The parsed response body (a list), or `NULL` for empty bodies.
#' @keywords internal
#' @noRd
vmx_perform <- function(req) {
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      vmx_abort(
        paste0("Request to the VeloMetrix API failed: ", conditionMessage(e)),
        class = "vmx_api_error", parent = e
      )
    }
  )
  if (httr2::resp_status(resp) >= 400) {
    vmx_handle_http_error(resp)
  }
  if (isTRUE(httr2::resp_has_body(resp))) {
    httr2::resp_body_json(resp, simplifyVector = FALSE)
  } else {
    invisible(NULL)
  }
}

#' Translate a >=400 response into a classed vmx condition
#'
#' Reads the canonical `{error: {code, reason, message, field}}` envelope when
#' present, falling back to the HTTP status. 401/403 raise `vmx_auth_error`;
#' everything else raises `vmx_api_error`.
#' @keywords internal
#' @noRd
vmx_handle_http_error <- function(resp) {
  status <- httr2::resp_status(resp)
  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )
  err <- if (is.list(body) && is.list(body$error)) body$error else NULL
  reason <- err$reason %||% err$code %||% "internal_error"
  detail <- err$message %||% httr2::resp_status_desc(resp)
  class <- if (status %in% c(401L, 403L)) "vmx_auth_error" else "vmx_api_error"
  vmx_abort(
    sprintf("VeloMetrix API error (HTTP %d): %s", status, detail),
    class = class,
    status = status,
    reason = reason,
    field = err$field,
    data = err %||% body
  )
}

# -- verbs -------------------------------------------------------------------

#' GET and return the parsed body
#' @keywords internal
#' @noRd
vmx_get <- function(client, path, params = list()) {
  vmx_perform(vmx_req_query(vmx_req(client, path), params))
}

#' POST a JSON body and return the parsed response
#' @keywords internal
#' @noRd
vmx_post <- function(client, path, body = NULL) {
  req <- httr2::req_method(vmx_req(client, path), "POST")
  if (!is.null(body)) req <- httr2::req_body_json(req, body)
  vmx_perform(req)
}

#' PUT a JSON body and return the parsed response
#' @keywords internal
#' @noRd
vmx_put <- function(client, path, body = NULL) {
  req <- httr2::req_method(vmx_req(client, path), "PUT")
  if (!is.null(body)) req <- httr2::req_body_json(req, body)
  vmx_perform(req)
}

#' PATCH a JSON body and return the parsed response
#' @keywords internal
#' @noRd
vmx_patch <- function(client, path, body = NULL) {
  req <- httr2::req_method(vmx_req(client, path), "PATCH")
  if (!is.null(body)) req <- httr2::req_body_json(req, body)
  vmx_perform(req)
}

#' Follow `next_cursor` pagination, returning all items as a flat list
#'
#' @param limit Optional cap on the total number of items returned.
#' @keywords internal
#' @noRd
vmx_paginate <- function(client, path, params = list(), limit = NULL) {
  query <- vmx_compact(params)
  items <- list()
  repeat {
    page <- vmx_get(client, path, query)
    items <- c(items, page$items %||% list())
    if (!is.null(limit) && length(items) >= limit) {
      items <- items[seq_len(limit)]
      break
    }
    cursor <- page$next_cursor
    if (!is.character(cursor) || length(cursor) != 1L || !nzchar(cursor)) break
    query$cursor <- cursor
  }
  items
}

#' Drop NULL-valued elements of a list
#' @keywords internal
#' @noRd
vmx_compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}
