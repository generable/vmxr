# Typed S3 resource objects, id resolution, and tibble conversion for
# collection endpoints. Single resources are typed S3 objects (a parsed list
# with a `vmx_<noun>` class); collections become tibbles.

#' Construct a typed vmx resource object
#' @param data Parsed response list.
#' @param subclass The `vmx_<noun>` class (e.g. `"vmx_treatment"`).
#' @param id_field Name of the id field within `data`, stashed for printing /
#'   id resolution.
#' @keywords internal
#' @noRd
new_vmx_resource <- function(data, subclass, id_field) {
  structure(
    data,
    vmx_id_field = id_field,
    class = c(subclass, "vmx_resource")
  )
}

#' Pull the canonical id out of a vmx resource
#' @keywords internal
#' @noRd
vmx_resource_id <- function(x) {
  field <- attr(x, "vmx_id_field")
  if (!is.null(field) && !is.null(x[[field]])) {
    return(x[[field]])
  }
  # Fall back to the first `*_id` element.
  ids <- grep("_id$", names(x), value = TRUE)
  if (length(ids)) x[[ids[[1]]]] else NULL
}

#' Resolve an argument that may be an id string or a vmx resource object
#'
#' Implements the "ids or objects" convention with client-side prefix
#' validation (matching the CLI's fail-fast behaviour).
#'
#' @param x An id string (e.g. `"tmt_..."`) or a `vmx_resource`.
#' @param prefix Expected id prefix without the underscore (e.g. `"tmt"`);
#'   `NULL` skips validation.
#' @param arg Argument name, for error messages.
#' @return A single id string.
#' @keywords internal
#' @noRd
vmx_id <- function(x, prefix = NULL, arg = "id") {
  id <- if (inherits(x, "vmx_resource")) vmx_resource_id(x) else x
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    vmx_abort(
      sprintf("`%s` must be a single id string or a vmx object.", arg),
      class = "vmx_usage_error"
    )
  }
  if (!is.null(prefix) && !startsWith(id, paste0(prefix, "_"))) {
    vmx_abort(
      sprintf("`%s` should be a '%s_*' id, got '%s'.", arg, prefix, id),
      class = "vmx_usage_error"
    )
  }
  id
}

#' Resolve an optional id argument (`NULL` passes through)
#' @keywords internal
#' @noRd
vmx_opt_id <- function(x, prefix = NULL, arg = "id") {
  if (is.null(x)) NULL else vmx_id(x, prefix, arg)
}

#' @export
print.vmx_resource <- function(x, ...) {
  cls <- class(x)[[1]]
  id <- vmx_resource_id(x)
  cli::cli_text("{.cls <{cls}>}{if (!is.null(id)) paste0(' ', id) else ''}")
  scalars <- Filter(function(v) !is.null(v) && length(v) == 1 && !is.list(v), x)
  scalars <- scalars[setdiff(names(scalars), attr(x, "vmx_id_field"))]
  if (length(scalars)) {
    cli::cli_bullets(stats::setNames(
      sprintf("{.field %s}: %s", names(scalars), unlist(scalars)),
      rep("*", length(scalars))
    ))
  }
  invisible(x)
}

#' @export
as_tibble.vmx_resource <- function(x, ...) {
  vmx_flatten_row(unclass(x))
}

#' Convert a list of resource dicts into a tibble (one row each)
#' @keywords internal
#' @noRd
vmx_items_to_tibble <- function(items) {
  if (!length(items)) return(tibble::tibble())
  rows <- lapply(items, vmx_flatten_row)
  vctrs::vec_rbind(!!!rows)
}

#' Flatten one resource dict into a single-row tibble
#'
#' Scalars become columns; one level of named sub-objects is flattened with a
#' `parent_child` prefix; anything else (arrays, deeper nesting) becomes a
#' list-column. `NULL` scalars become `NA`.
#' @keywords internal
#' @noRd
vmx_flatten_row <- function(item) {
  flat <- list()
  for (nm in names(item)) {
    val <- item[[nm]]
    if (is.null(val)) {
      flat[[nm]] <- NA
    } else if (is.list(val) && !is.null(names(val)) && length(val)) {
      for (sub in names(val)) {
        flat[[paste0(nm, "_", sub)]] <- val[[sub]] %||% NA
      }
    } else if (length(val) == 1 && !is.list(val)) {
      flat[[nm]] <- val
    } else {
      flat[[nm]] <- list(val)
    }
  }
  tibble::as_tibble(flat)
}
