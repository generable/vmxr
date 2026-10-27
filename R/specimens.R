# Specimen selections for NCA, modeling options and model builds (API contract
# §5.6.2). Every NCA, modeling-options and model-build request names its PK
# specimen scope explicitly, and every PD marker names one specimen or All
# specimens. vmxr never picks a specimen for the caller: the DataVersion's
# recommended PK specimen is shown as a hint in error messages, never applied.

#' Select all specimens
#'
#' A sentinel for an explicit "All specimens" selection. Pass it as
#' `pk_specimen` to [vmx_nca()], [vmx_modeling_options()] or
#' [vmx_model_build()] to pool PK observations across every specimen, or as the
#' `specimen` of [vmx_pd_marker()] to pool one PD marker across its specimens.
#'
#' All specimens is never a default; a call that needs a specimen and gets none
#' fails rather than choosing one.
#'
#' @return A `vmx_all_specimens` object.
#' @seealso [vmx_pd_marker()]
#' @examples
#' vmx_all_specimens()
#' @export
vmx_all_specimens <- function() {
  structure(list(), class = "vmx_all_specimens")
}

#' @export
print.vmx_all_specimens <- function(x, ...) {
  cat("<vmx_all_specimens>\n")
  invisible(x)
}

#' Select a PD marker in one specimen or in all specimens
#'
#' Builds one PD marker selection for [vmx_modeling_options()] and
#' [vmx_model_build()]. Each selection names the marker's `gen_uuid` and either
#' one specimen (for example `"serum"`) or [vmx_all_specimens()]. Two specimens
#' of the same marker may be selected as two separate entries; selecting one
#' marker both in a named specimen and in all specimens is an error.
#'
#' @param gen_uuid The PD marker's `gen_uuid`, as listed in the data version's
#'   `pd_markers`.
#' @param specimen One specimen type string (a value of the marker's
#'   `specimen_type`, which may be `"unspecified"`), or [vmx_all_specimens()].
#'   Required.
#' @param direction The run-scoped response direction, `"increasing"` or
#'   `"decreasing"`. Required by [vmx_model_build()]; [vmx_modeling_options()]
#'   ignores it, so the same selections can be previewed and then built.
#' @return A `vmx_pd_marker` object.
#' @examples
#' vmx_pd_marker("8ffb3792-8bc9-4d38-b123-fc3e2170f6ee", "serum", "decreasing")
#' vmx_pd_marker(
#'   "8ffb3792-8bc9-4d38-b123-fc3e2170f6ee", vmx_all_specimens(), "decreasing"
#' )
#' @export
vmx_pd_marker <- function(gen_uuid, specimen, direction = NULL) {
  gen_uuid <- vmx_nonempty_strings(gen_uuid, "gen_uuid", exactly_one = TRUE)
  if (missing(specimen) || is.null(specimen)) {
    vmx_abort(
      paste0(
        "`specimen` is required: give one specimen type (e.g. \"serum\") or ",
        "vmx_all_specimens()."
      ),
      class = "vmx_usage_error"
    )
  }
  scope <- vmx_specimen_scope(specimen, "specimen")
  if (!is.null(direction)) {
    direction <- vmx_nonempty_strings(direction, "direction", exactly_one = TRUE)
    if (!direction %in% c("increasing", "decreasing")) {
      vmx_abort(
        "`direction` must be \"increasing\" or \"decreasing\".",
        class = "vmx_usage_error"
      )
    }
  }
  structure(
    c(list(gen_uuid = gen_uuid), scope, list(direction = direction)),
    class = "vmx_pd_marker"
  )
}

#' @export
print.vmx_pd_marker <- function(x, ...) {
  specimen <- if (isTRUE(x$all_specimens)) "all specimens" else x$specimen_type
  direction <- if (is.null(x$direction)) "" else paste0(", ", x$direction)
  cat(sprintf("<vmx_pd_marker %s [%s%s]>\n", x$gen_uuid, specimen, direction))
  invisible(x)
}

# Wire form of one specimen choice: `{specimen_type}` or `{all_specimens: true}`.
vmx_specimen_scope <- function(x, arg) {
  if (inherits(x, "vmx_all_specimens")) {
    return(list(all_specimens = TRUE))
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    vmx_abort(
      sprintf(
        "`%s` must be one specimen type string or vmx_all_specimens().", arg
      ),
      class = "vmx_usage_error"
    )
  }
  list(specimen_type = x)
}

# `PkSpecimenSelection` request body for a required `pk_specimen` argument. A
# missing or NULL selection fails, naming the specimens the data version offers.
vmx_pk_specimen_body <- function(pk_specimen, data_version_id, client) {
  if (is.null(pk_specimen)) {
    vmx_abort_missing_pk_specimen(data_version_id, client)
  }
  vmx_specimen_scope(pk_specimen, "pk_specimen")
}

# Normalise `pd_marker` (NULL, one vmx_pd_marker, or a list of them) into the
# wire `MarkerSelection[]`, with or without the run-scoped direction.
vmx_pd_marker_body <- function(pd_marker, direction) {
  if (is.null(pd_marker)) return(list())
  if (inherits(pd_marker, "vmx_pd_marker")) pd_marker <- list(pd_marker)
  if (is.character(pd_marker)) {
    vmx_abort(
      paste0(
        "`pd_marker` no longer accepts marker UUID strings or the ",
        "\"uuid:direction\" shorthand: each PD marker must name a specimen. ",
        "Use vmx_pd_marker(gen_uuid, specimen, direction)."
      ),
      class = "vmx_usage_error"
    )
  }
  if (!is.list(pd_marker) || !is.null(names(pd_marker)) ||
      !all(vapply(pd_marker, inherits, logical(1), "vmx_pd_marker"))) {
    vmx_abort(
      "`pd_marker` must be NULL, one vmx_pd_marker(), or a list of them.",
      class = "vmx_usage_error"
    )
  }
  out <- lapply(pd_marker, function(m) {
    m <- unclass(m)
    if (isTRUE(direction) && is.null(m$direction)) {
      vmx_abort(
        sprintf(
          "PD marker %s needs a direction (\"increasing\" or \"decreasing\").",
          m$gen_uuid
        ),
        class = "vmx_usage_error"
      )
    }
    if (!isTRUE(direction)) m$direction <- NULL
    vmx_compact(m)
  })
  ids <- vapply(out, `[[`, character(1), "gen_uuid")
  keys <- vapply(out, function(m) {
    paste0(m$gen_uuid, "\r", if (isTRUE(m$all_specimens)) "" else m$specimen_type)
  }, character(1))
  if (anyDuplicated(keys)) {
    vmx_abort(
      "`pd_marker` selects the same marker and specimen more than once.",
      class = "vmx_usage_error"
    )
  }
  pooled <- unique(ids[vapply(out, function(m) isTRUE(m$all_specimens), logical(1))])
  if (any(duplicated(ids) & ids %in% pooled)) {
    vmx_abort(
      paste0(
        "`pd_marker` selects one marker both in all specimens and in a named ",
        "specimen; choose one or the other."
      ),
      class = "vmx_usage_error"
    )
  }
  out
}

# Opt in to the marker-scopes representation (§5.6.2) so runs and fits that pool
# a PD marker across specimens can be created, read and listed.
.vmx_marker_scopes <- list(representation = "marker_scopes")

# The PK specimens a data version offers, plus its recommendation (a hint only).
# Returns NULL when the lookup itself fails, so an error message can degrade
# gracefully instead of masking the original problem.
vmx_pk_specimen_choices <- function(data_version_id, client) {
  ma <- tryCatch(
    vmx_get(client, paste0("/data-versions/", data_version_id, "/marker-analysis")),
    error = function(e) NULL
  )
  if (!is.list(ma)) return(NULL)
  pk_markers <- if (is.list(ma$marker_analysis)) ma$marker_analysis$pk_markers else NULL
  specimens <- unique(unlist(lapply(pk_markers, function(m) {
    s <- if (is.list(m)) m$specimen_type else NULL
    if (is.character(s) && length(s) == 1L && !is.na(s)) s else NULL
  })))
  recommended <- if (is.list(ma$recommended_pk_specimen)) {
    ma$recommended_pk_specimen$value
  } else {
    NULL
  }
  if (!is.character(recommended) || length(recommended) != 1L) recommended <- NULL
  list(specimens = specimens %||% character(0), recommended = recommended)
}

vmx_pk_specimen_hint <- function(data_version_id, client) {
  choices <- vmx_pk_specimen_choices(data_version_id, client)
  if (is.null(choices)) {
    return(paste0(
      "Pass `pk_specimen = \"<specimen type>\"` or ",
      "`pk_specimen = vmx_all_specimens()`."
    ))
  }
  if (!length(choices$specimens)) {
    return(paste0(
      "This data version records no individual PK specimens; only ",
      "`pk_specimen = vmx_all_specimens()` is available."
    ))
  }
  hint <- sprintf(
    "PK specimens available on %s: %s, or vmx_all_specimens().",
    data_version_id,
    paste0("\"", choices$specimens, "\"", collapse = ", ")
  )
  if (!is.null(choices$recommended)) {
    hint <- paste0(
      hint,
      sprintf(
        " The data version recommends \"%s\"; vmxr does not apply it for you.",
        choices$recommended
      )
    )
  }
  hint
}

vmx_abort_missing_pk_specimen <- function(data_version_id, client,
                                          call = rlang::caller_env()) {
  vmx_abort(
    paste0(
      "`pk_specimen` is required: choose one specimen or all specimens. ",
      vmx_pk_specimen_hint(data_version_id, client)
    ),
    class = "vmx_usage_error",
    call = call
  )
}

# API reasons for rejected specimen selections, each with an R-side next step.
.vmx_specimen_reasons <- c(
  invalid_pk_specimen = "The PK specimen selection was rejected.",
  invalid_pd_markers = paste0(
    "Each PD marker must name one specimen or vmx_all_specimens() (see ",
    "vmx_pd_marker()), may appear once per specimen, and must not be selected ",
    "both in all specimens and in a named specimen."
  ),
  marker_analysis_unavailable = paste0(
    "This data version has no usable per-specimen marker analysis, so only ",
    "vmx_all_specimens() selections are available for PK and PD."
  ),
  marker_scopes_representation_required = paste0(
    "This resource selects a PD marker across all specimens and can only be ",
    "read with the marker-scopes representation."
  )
)

# Run `expr` (a create/preview request) and re-raise the API's specimen
# rejections as `vmx_specimen_error`s that say what to change. Other errors pass
# through untouched.
vmx_with_specimen_errors <- function(expr, data_version_id, client,
                                     call = rlang::caller_env()) {
  tryCatch(expr, vmx_api_error = function(e) {
    reason <- e$reason
    if (!is.character(reason) || length(reason) != 1L ||
        !reason %in% names(.vmx_specimen_reasons)) {
      stop(e)
    }
    hint <- .vmx_specimen_reasons[[reason]]
    if (identical(reason, "invalid_pk_specimen")) {
      hint <- paste(hint, vmx_pk_specimen_hint(data_version_id, client))
    }
    vmx_abort(
      paste(conditionMessage(e), hint),
      class = c("vmx_specimen_error", "vmx_api_error"),
      status = e$status,
      reason = reason,
      field = e$field,
      data = e$data,
      parent = e,
      call = call
    )
  })
}

# Validate a `PkSpecimenSelection` from a marker-selection read.
vmx_validate_pk_specimen_response <- function(x, path) {
  if (!is.list(x) || is.null(names(x))) {
    vmx_abort_response(sprintf("field '%s' must be an object.", path), field = path)
  }
  if ("all_specimens" %in% names(x)) {
    if (!isTRUE(x$all_specimens) || length(x) != 1L) {
      vmx_abort_response(
        sprintf("field '%s' must be {all_specimens: true} or {specimen_type}.", path),
        field = path
      )
    }
  } else {
    vmx_response_scalar(
      vmx_response_field(x, "specimen_type", paste0(path, ".specimen_type")),
      paste0(path, ".specimen_type"),
      type = "character",
      nonempty = TRUE
    )
  }
  invisible(x)
}

# Validate one accepted PD selection: `{gen_uuid}` plus `specimen_type` or
# `all_specimens: true` (a saved UUID-only selection is accepted when
# `uuid_only` is TRUE: runs and fits accepted before PD specimens were
# selectable may report one, and it is returned as-is, not reinterpreted).
vmx_validate_pd_selection_response <- function(x, path, uuid_only = FALSE) {
  if (!is.list(x) || is.null(names(x))) {
    vmx_abort_response(sprintf("field '%s' must be an object.", path), field = path)
  }
  vmx_response_scalar(
    vmx_response_field(x, "gen_uuid", paste0(path, ".gen_uuid")),
    paste0(path, ".gen_uuid"),
    type = "character",
    nonempty = TRUE
  )
  if ("all_specimens" %in% names(x)) {
    if (!isTRUE(x$all_specimens) || "specimen_type" %in% names(x)) {
      vmx_abort_response(
        sprintf("field '%s' must select one specimen or all specimens.", path),
        field = path
      )
    }
  } else if ("specimen_type" %in% names(x)) {
    vmx_response_scalar(
      x$specimen_type, paste0(path, ".specimen_type"),
      type = "character", nonempty = TRUE
    )
  } else if (!isTRUE(uuid_only)) {
    vmx_abort_response(
      sprintf("field '%s' must select one specimen or all specimens.", path),
      field = path
    )
  }
  invisible(x)
}

#' Accepted specimen selections of an NCA, model-build run or model fit
#'
#' Reads the selections the API accepted for a resource
#' (`GET .../marker-selection`). These are what the analysis actually used and
#' are never recomputed from the data version's current metadata.
#'
#' * `vmx_nca_marker_selection()` returns `list(pk_specimen)`.
#' * `vmx_model_build_marker_selection()` returns `list(pk_specimen,
#'   pd_markers)`, where each PD entry carries `gen_uuid`, either
#'   `specimen_type` or `all_specimens = TRUE`, and `direction`. A run
#'   accepted before PD specimens were selectable may report only `gen_uuid`
#'   and `direction`.
#' * `vmx_model_fit_marker_selection()` returns `list(pk_specimen, pd_marker)`;
#'   `pd_marker` is `NULL` for a PK fit, and for a fit saved before specimen
#'   selection it may carry only `gen_uuid`.
#'
#' `pk_specimen` is `list(specimen_type = "...")` or
#' `list(all_specimens = TRUE)`. An analysis accepted before PK specimens were
#' selectable reports all specimens.
#'
#' @param nca An NCA id (`nca_...`) or `vmx_nca_analysis`.
#' @param run A build-run id (`run_...`) or `vmx_model_build_run`.
#' @param fit A fit id (`mf_...`) or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A named list, as described above.
#' @name vmx_marker_selection
NULL

#' @rdname vmx_marker_selection
#' @export
vmx_nca_marker_selection <- function(nca, client = vmx_client()) {
  nca_id <- vmx_id(nca, "nca")
  out <- vmx_get(client, paste0("/nca-analyses/", nca_id, "/marker-selection"))
  vmx_validate_pk_specimen_response(
    vmx_response_field(out, "pk_specimen", "NCA marker selection.pk_specimen"),
    "NCA marker selection.pk_specimen"
  )
  out
}

#' @rdname vmx_marker_selection
#' @export
vmx_model_build_marker_selection <- function(run, client = vmx_client()) {
  run_id <- vmx_id(run, "run")
  out <- vmx_get(client, paste0("/model-build-runs/", run_id, "/marker-selection"))
  context <- "model-build marker selection"
  vmx_validate_pk_specimen_response(
    vmx_response_field(out, "pk_specimen", paste0(context, ".pk_specimen")),
    paste0(context, ".pk_specimen")
  )
  pd <- vmx_response_field(out, "pd_markers", paste0(context, ".pd_markers"))
  if (!is.list(pd) || !is.null(names(pd))) {
    vmx_abort_response(
      sprintf("field '%s.pd_markers' must be an array.", context),
      field = "pd_markers"
    )
  }
  for (i in seq_along(pd)) {
    path <- sprintf("%s.pd_markers[%d]", context, i)
    vmx_validate_pd_selection_response(pd[[i]], path, uuid_only = TRUE)
    direction <- vmx_response_scalar(
      vmx_response_field(pd[[i]], "direction", paste0(path, ".direction")),
      paste0(path, ".direction"),
      type = "character"
    )
    if (!direction %in% c("increasing", "decreasing")) {
      vmx_abort_response(
        sprintf("field '%s.direction' must be increasing or decreasing.", path),
        field = paste0(path, ".direction")
      )
    }
  }
  out
}

#' @rdname vmx_marker_selection
#' @export
vmx_model_fit_marker_selection <- function(fit, client = vmx_client()) {
  fit_id <- vmx_id(fit, "mf")
  out <- vmx_get(client, paste0("/model-fits/", fit_id, "/marker-selection"))
  context <- "model-fit marker selection"
  vmx_validate_pk_specimen_response(
    vmx_response_field(out, "pk_specimen", paste0(context, ".pk_specimen")),
    paste0(context, ".pk_specimen")
  )
  pd <- vmx_response_field(
    out, "pd_marker", paste0(context, ".pd_marker"), allow_null = TRUE
  )
  if (!is.null(pd)) {
    vmx_validate_pd_selection_response(
      pd, paste0(context, ".pd_marker"), uuid_only = TRUE
    )
  }
  out
}
