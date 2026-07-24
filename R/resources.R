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
  if (!is.list(data) || is.null(names(data)) || anyDuplicated(names(data))) {
    vmx_abort_response(
      sprintf("resource <%s> must be a named object.", subclass),
      field = id_field
    )
  }
  vmx_response_scalar(
    vmx_response_field(data, id_field, paste0(subclass, ".", id_field)),
    paste0(subclass, ".", id_field),
    type = "character",
    nonempty = TRUE
  )
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
  if (!is.null(field)) {
    return(vmx_response_scalar(
      vmx_response_field(x, field, paste0(class(x)[[1]], ".", field)),
      paste0(class(x)[[1]], ".", field),
      type = "character",
      nonempty = TRUE
    ))
  }
  # Fall back to the first `*_id` element.
  ids <- grep("_id$", names(x), value = TRUE)
  if (!length(ids)) {
    vmx_abort_response(
      sprintf("resource <%s> has no declared identifier.", class(x)[[1]]),
      field = "id"
    )
  }
  vmx_response_scalar(x[[ids[[1]]]], ids[[1]], type = "character", nonempty = TRUE)
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

#' Convert a `DvTable` envelope (columns + row-objects) into a typed tibble
#'
#' Coerces each column per the server-declared type; column metadata is kept on
#' the `"columns"` attribute.
#' @keywords internal
#' @noRd
vmx_dvtable_to_tibble <- function(tbl) {
  cols <- vmx_response_field(tbl, "columns", "data-version table.columns")
  rows <- vmx_response_field(tbl, "rows", "data-version table.rows")
  if (!is.list(cols) || !is.null(names(cols)) ||
      !is.list(rows) || !is.null(names(rows))) {
    vmx_abort_response(
      "data-version table 'columns' and 'rows' must be arrays.",
      field = "columns"
    )
  }
  names_ <- vapply(seq_along(cols), function(i) {
    col <- cols[[i]]
    vmx_response_scalar(
      vmx_response_field(col, "name", sprintf("data-version table.columns[%d].name", i)),
      sprintf("data-version table.columns[%d].name", i),
      type = "character",
      nonempty = TRUE
    )
  }, character(1))
  if (anyDuplicated(names_)) {
    vmx_abort_response(
      "data-version table contains duplicate column names.",
      field = "columns"
    )
  }
  types <- vapply(seq_along(cols), function(i) {
    type <- vmx_response_scalar(
      vmx_response_field(cols[[i]], "type", sprintf("data-version table.columns[%d].type", i)),
      sprintf("data-version table.columns[%d].type", i),
      type = "character",
      nonempty = TRUE
    )
    if (!type %in% c("string", "number", "integer", "boolean", "categorical")) {
      vmx_abort_response(
        "data-version table contains an unknown declared column type.",
        field = "columns.type"
      )
    }
    type
  }, character(1))
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    if (!is.list(row) || is.null(names(row)) ||
        any(!nzchar(names(row))) || anyDuplicated(names(row)) ||
        !setequal(names(row), names_)) {
      vmx_abort_response(
        "data-version table row fields do not exactly match the declared columns.",
        field = "rows"
      )
    }
  }
  values <- lapply(seq_along(names_), function(j) {
    lapply(seq_along(rows), function(i) {
      row <- rows[[i]]
      row[[names_[[j]]]]
    })
  })
  data <- lapply(seq_along(cols), function(i) {
    vmx_coerce_col(values[[i]], types[[i]])
  })
  out <- tibble::as_tibble(stats::setNames(data, names_))
  attr(out, "columns") <- cols
  out
}

# Coerce a list of parsed cell values to a typed vector (server type -> R type).
vmx_coerce_col <- function(vals, type) {
  is_na <- function(x) is.null(x) || length(x) == 0
  switch(
    type %||% "string",
    number = vapply(vals, function(x) {
      if (is_na(x)) return(NA_real_)
      vmx_response_scalar(x, "data-version table numeric cell", type = "numeric")
    }, numeric(1)),
    integer = vapply(vals, function(x) {
      if (is_na(x)) return(NA_integer_)
      value <- vmx_response_scalar(
        x, "data-version table integer cell", type = "numeric"
      )
      integer_value <- suppressWarnings(as.integer(value))
      if (is.na(integer_value) || value != integer_value) {
        vmx_abort_response(
          "data-version table integer cell is outside the supported integer range or is not an integer.",
          field = "rows"
        )
      }
      integer_value
    }, integer(1)),
    boolean = vapply(vals, function(x) {
      if (is_na(x)) return(NA)
      vmx_response_scalar(x, "data-version table boolean cell", type = "logical")
    }, logical(1)),
    vapply(vals, function(x) {
      if (is_na(x)) return(NA_character_)
      vmx_response_scalar(x, "data-version table string cell", type = "character")
    }, character(1))
  )
}

#' Get the opaque cursor for the next API page
#'
#' Collection functions return exactly one server-owned page as a tibble. Use
#' this helper to obtain the cursor to pass back through the collection
#' function's `cursor` argument.
#'
#' @param x A tibble returned by a vmxr collection function.
#' @return The next cursor as a string, or `NULL` on the last page.
#' @export
vmx_next_cursor <- function(x) {
  attr(x, "next_cursor", exact = TRUE)
}

#' Test whether an API page has a following page
#'
#' @inheritParams vmx_next_cursor
#' @return A single logical value.
#' @export
vmx_has_next_page <- function(x) {
  isTRUE(attr(x, "has_next_page", exact = TRUE))
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
  if (!is.list(items) || !is.null(names(items))) {
    vmx_abort_response("pagination field 'items' must be an array.", field = "items")
  }
  if (!length(items)) return(tibble::tibble())
  rows <- lapply(items, vmx_flatten_row)
  vctrs::vec_rbind(!!!rows)
}

#' Convert one canonical API page into a tibble
#'
#' The cursor remains opaque and is attached to the returned tibble rather than
#' decoded or followed automatically.
#' @keywords internal
#' @noRd
vmx_page_to_tibble <- function(page, context = "collection response") {
  if (!is.list(page) || is.null(names(page))) {
    vmx_abort_response(sprintf("%s must be an object.", context), field = "page")
  }
  items <- vmx_response_field(page, "items", paste0(context, ".items"))
  next_cursor <- vmx_response_field(
    page, "next_cursor", paste0(context, ".next_cursor"),
    allow_null = TRUE
  )
  if (!is.null(next_cursor)) {
    next_cursor <- vmx_response_scalar(
      next_cursor,
      paste0(context, ".next_cursor"),
      type = "character",
      nonempty = TRUE
    )
  }
  has_next_page <- vmx_response_scalar(
    vmx_response_field(page, "has_next_page", paste0(context, ".has_next_page")),
    paste0(context, ".has_next_page"),
    type = "logical"
  )
  if (!identical(has_next_page, !is.null(next_cursor))) {
    vmx_abort_response(
      sprintf("%s has inconsistent 'has_next_page' and 'next_cursor'.", context),
      field = "has_next_page"
    )
  }
  out <- vmx_items_to_tibble(items)
  attr(out, "next_cursor") <- next_cursor
  attr(out, "has_next_page") <- has_next_page
  out
}

#' Flatten one resource dict into a single-row tibble
#'
#' Scalars become columns; one level of named sub-objects is flattened with a
#' `parent_child` prefix; anything else (arrays, deeper nesting) becomes a
#' list-column. `NULL` scalars become `NA`.
#' @keywords internal
#' @noRd
vmx_flatten_row <- function(item) {
  if (!is.list(item) || is.null(names(item)) || !length(item) ||
      any(!nzchar(names(item))) || anyDuplicated(names(item))) {
    vmx_abort_response("collection item must be a named object.", field = "items")
  }
  flat <- list()
  put <- function(name, value) {
    if (name %in% names(flat)) {
      vmx_abort_response(
        "collection item contains fields that collide when flattened.",
        field = name
      )
    }
    flat[[name]] <<- value
  }
  for (nm in names(item)) {
    val <- item[[nm]]
    if (is.null(val)) {
      put(nm, NA)
    } else if (is.list(val) && !is.null(names(val)) && length(val)) {
      if (any(!nzchar(names(val))) || anyDuplicated(names(val))) {
        vmx_abort_response(
          "collection item contains an invalid nested object.",
          field = nm
        )
      }
      for (sub in names(val)) {
        nested <- val[[sub]]
        put(paste0(nm, "_", sub), if (is.null(nested)) {
          NA
        } else if (!is.list(nested) && length(nested) == 1L) {
          nested
        } else {
          list(nested)
        })
      }
    } else if (length(val) == 1 && !is.list(val)) {
      put(nm, val)
    } else {
      put(nm, list(val))
    }
  }
  tibble::as_tibble(flat)
}

# ---- Successful-response validation ---------------------------------------

# Pull a required named response field. `allow_null` distinguishes an explicit
# JSON null from an absent field by checking the object's names first.
vmx_response_field <- function(x, name, path = name, allow_null = FALSE) {
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x)) ||
      !name %in% names(x)) {
    vmx_abort_response(sprintf("required field '%s' is missing.", path), field = path)
  }
  value <- x[[name]]
  if (is.null(value) && !isTRUE(allow_null)) {
    vmx_abort_response(sprintf("required field '%s' is null.", path), field = path)
  }
  value
}

# Validate and coerce a JSON scalar without accepting an array and silently
# taking its first member.
vmx_response_scalar <- function(x, path, type = c("character", "numeric", "logical"),
                                nonempty = FALSE) {
  type <- match.arg(type)
  if (is.list(x) || length(x) != 1L || is.na(x)) {
    vmx_abort_response(sprintf("field '%s' must be one %s value.", path, type), field = path)
  }
  valid <- switch(
    type,
    character = is.character(x),
    numeric = is.numeric(x) && is.finite(x),
    logical = is.logical(x)
  )
  if (!isTRUE(valid)) {
    vmx_abort_response(sprintf("field '%s' must be one %s value.", path, type), field = path)
  }
  if (isTRUE(nonempty) && type == "character" && !nzchar(trimws(x))) {
    vmx_abort_response(sprintf("field '%s' must not be blank.", path), field = path)
  }
  switch(type, character = as.character(x), numeric = as.numeric(x), logical = as.logical(x))
}

# Validate and coerce a JSON array of scalars. Nullable arrays retain JSON null
# cells as typed NA; required estimate arrays reject them.
vmx_response_vector <- function(x, path, type = c("character", "numeric", "logical"),
                                size = NULL, nullable = FALSE) {
  type <- match.arg(type)
  if (!is.list(x) || !is.null(names(x))) {
    vmx_abort_response(sprintf("field '%s' must be an array.", path), field = path)
  }
  if (!is.null(size) && length(x) != size) {
    vmx_abort_response(
      sprintf("field '%s' has length %d; expected %d.", path, length(x), size),
      field = path
    )
  }
  missing_value <- switch(type, character = NA_character_, numeric = NA_real_, logical = NA)
  values <- lapply(seq_along(x), function(i) {
    value <- x[[i]]
    missing <- is.null(value) || length(value) == 0L ||
      (!is.list(value) && length(value) == 1L && is.na(value))
    if (missing) {
      if (isTRUE(nullable)) return(missing_value)
      vmx_abort_response(
        sprintf("field '%s' contains a null value.", path),
        field = path
      )
    }
    vmx_response_scalar(value, path, type = type)
  })
  switch(
    type,
    character = vapply(values, identity, character(1)),
    numeric = vapply(values, identity, numeric(1)),
    logical = vapply(values, identity, logical(1))
  )
}

# Verify that a response belongs to the resource requested by the caller before
# associating or reshaping its data.
vmx_validate_response_id <- function(x, field, expected, context) {
  actual <- vmx_response_scalar(
    vmx_response_field(x, field, paste0(context, ".", field)),
    paste0(context, ".", field),
    type = "character",
    nonempty = TRUE
  )
  if (!identical(actual, expected)) {
    vmx_abort_response(
      sprintf("%s field '%s' does not match the requested resource.", context, field),
      field = field
    )
  }
  invisible(actual)
}
