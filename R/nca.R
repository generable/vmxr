# NCA — non-compartmental analysis verbs.

#' List NCA analyses
#' @param data_version Optional data-version (`dv_...`) filter.
#' @param study Optional study (`std_...`) filter.
#' @param treatment Optional treatment (`tmt_...`) filter.
#' @param status Optional status filter (`queued`/`running`/`completed`/
#'   `degraded`/`failed`).
#' @param time_basis Optional time-basis filter.
#' @param client A `vmx_client`.
#' @return A tibble containing all matching analyses.
#' @export
vmx_nca_analyses <- function(data_version = NULL, study = NULL, treatment = NULL,
                             status = NULL, time_basis = NULL,
                             client = vmx_client()) {
  params <- list(
    data_version_id = vmx_opt_id(data_version, "dv", "data_version"),
    study_id = vmx_opt_id(study, "std", "study"),
    treatment_id = vmx_opt_id(treatment, "tmt", "treatment"),
    status = status,
    time_basis = time_basis
  )
  vmx_paginate(client, "/nca-analyses", params)
}

#' Run an NCA analysis
#'
#' Creates the analysis (`POST /nca-analyses`) and, by default, blocks until it
#' settles. `time_basis` is one of `"observed"`, `"nominal"`, or
#' `"nominal_from_observed_dose"` (validated server-side against the
#' DataVersion's available bases).
#'
#' @param data_version A data-version id (`dv_...`) or `vmx_data_version`.
#' @param time_basis The time basis to compute on.
#' @param idempotency_key,retried_from Optional create fields.
#' @param wait If `TRUE` (default), block until the analysis is terminal.
#' @param ... Polling controls forwarded to [vmx_wait()] when `wait = TRUE`
#'   (e.g. `timeout`, `interval`, `progress`).
#' @param client A `vmx_client`.
#' @return A `vmx_nca_analysis`.
#' @export
vmx_nca <- function(data_version, time_basis, idempotency_key = NULL,
                    retried_from = NULL, wait = TRUE, ...,
                    client = vmx_client()) {
  time_basis <- vmx_nonempty_strings(
    time_basis, "time_basis", exactly_one = TRUE
  )
  if (!is.null(idempotency_key)) {
    vmx_id_like_scalar(idempotency_key, "idempotency_key")
  }
  body <- vmx_compact(list(
    data_version_id = vmx_id(data_version, "dv", "data_version"),
    time_basis = time_basis,
    idempotency_key = idempotency_key,
    retried_from = vmx_opt_id(retried_from, "nca", "retried_from")
  ))
  data <- vmx_post(client, "/nca-analyses", body)
  vmx_validate_response_id(
    data, "data_version_id", body$data_version_id, "NCA creation"
  )
  nca <- new_vmx_resource(data, "vmx_nca_analysis", "nca_id")
  if (isTRUE(wait)) vmx_wait(nca, client = client, ...) else nca
}

#' Fetch one NCA analysis
#' @param id An NCA id (`nca_...`) or `vmx_nca_analysis`.
#' @param client A `vmx_client`.
#' @return A `vmx_nca_analysis`.
#' @export
vmx_nca_get <- function(id, client = vmx_client()) {
  nca_id <- vmx_id(id, "nca")
  data <- vmx_get(client, paste0("/nca-analyses/", nca_id))
  vmx_validate_response_id(data, "nca_id", nca_id, "NCA analysis")
  new_vmx_resource(data, "vmx_nca_analysis", "nca_id")
}

#' NCA result table (PK parameters, per subject and result interval)
#'
#' Reads the NCA result and returns a tidy tibble of point estimates, handling
#' both server shapes transparently:
#'
#' * **API 0.3** returns a cursor-paged collection whose `items[]` are
#'   `NcaResultItem`s — one per result interval of interest. Every page is
#'   followed to the end via `next_cursor` and the items are assembled without
#'   truncation. Each row is one subject within one interval, and the per-interval
#'   dimension is carried in the `item_index`, `label`, `interval_start_hours`,
#'   and `interval_end_hours` columns (the two `*_hours` columns are `NA` when the
#'   server did not resolve a time interval for that subject).
#' * **API 0.2.2** returns a single flat object with top-level per-subject
#'   parallel arrays and one `point_estimates` map. Each row is one subject (there
#'   is no per-interval dimension, so the interval columns are absent).
#'
#' In both shapes the `point_estimates` map (metric -> per-subject values,
#' parallel to `gen_subject_uuid`) becomes one column per PK quantity, and
#' `not_estimable_reasons` — the per-subject map of metric -> reason strings — is
#' surfaced as the `not_estimable_reasons` list-column, so a null point estimate
#' carries its reason. Quantity metadata (display names, units, explanations) is
#' attached as the `"quantities"` attribute and the compact unit lookup as
#' `"units"`; for 0.3 these are the de-duplicated union across items.
#'
#' @param nca An NCA id or `vmx_nca_analysis`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_nca_result <- function(nca, client = vmx_client()) {
  nca_id <- vmx_id(nca, "nca")
  res <- vmx_get(client, paste0("/nca-analyses/", nca_id, "/result"))
  vmx_validate_response_id(res, "nca_id", nca_id, "NCA result")
  # 0.3 carries the per-interval collection under `items[]`; 0.2.2 puts the
  # subject arrays at the top level and has no `items` key.
  if ("items" %in% names(res)) {
    vmx_nca_result_paged(res, nca_id, client)
  } else {
    vmx_nca_result_flat(res)
  }
}

# Column names reserved for subject identity and the per-interval dimension; a
# backend-named point-estimate metric may not collide with any of them.
.vmx_nca_reserved_cols <- c(
  "item_index", "label", "interval_start_hours", "interval_end_hours",
  "subject_id", "gen_subject_uuid", "not_estimable_reasons"
)

# Reshape one subject-parallel estimate block -- the flat 0.2.2 body, or a single
# 0.3 `NcaResultItem` -- into a per-subject tibble: `subject_id`,
# `gen_subject_uuid`, one column per point-estimate metric, and a
# `not_estimable_reasons` list-column. `point_estimates`, `subject_id`, and
# `not_estimable_reasons` all align to `gen_subject_uuid`.
# @keywords internal
# @noRd
vmx_nca_reshape_block <- function(block, context) {
  gen_subject_uuid <- vmx_response_vector(
    vmx_response_field(block, "gen_subject_uuid", paste0(context, ".gen_subject_uuid")),
    paste0(context, ".gen_subject_uuid"),
    type = "character"
  )
  if (anyDuplicated(gen_subject_uuid)) {
    vmx_abort_response(
      sprintf("field '%s.gen_subject_uuid' contains duplicate subject keys.", context),
      field = "gen_subject_uuid"
    )
  }
  n <- length(gen_subject_uuid)
  subject_id <- vmx_response_vector(
    vmx_response_field(block, "subject_id", paste0(context, ".subject_id")),
    paste0(context, ".subject_id"),
    type = "character",
    size = n
  )
  estimates <- vmx_response_field(
    block, "point_estimates", paste0(context, ".point_estimates")
  )
  if (!is.list(estimates) || is.null(names(estimates)) ||
      any(!nzchar(names(estimates))) || anyDuplicated(names(estimates))) {
    vmx_abort_response(
      sprintf("field '%s.point_estimates' must be an object.", context),
      field = "point_estimates"
    )
  }
  reserved <- intersect(names(estimates), .vmx_nca_reserved_cols)
  if (length(reserved)) {
    vmx_abort_response(
      "NCA metric name conflicts with a subject-identity or interval column.",
      field = paste0("point_estimates.", reserved[[1]])
    )
  }
  cols <- list(
    subject_id = subject_id,
    gen_subject_uuid = gen_subject_uuid
  )
  for (metric in names(estimates)) {
    cols[[metric]] <- vmx_response_vector(
      estimates[[metric]],
      paste0(context, ".point_estimates.", metric),
      type = "numeric",
      size = n,
      nullable = TRUE
    )
  }
  cols[["not_estimable_reasons"]] <- vmx_nca_reasons_column(block, context, n)
  tibble::as_tibble(cols)
}

# Per-subject `not_estimable_reasons` list-column. The field is nullable and, when
# present, is an array aligned to `gen_subject_uuid`; each element is a
# metric -> reason-strings map (an empty object means "no reason"). A missing or
# null field yields an all-empty column so the returned shape stays stable.
# @keywords internal
# @noRd
vmx_nca_reasons_column <- function(block, context, n) {
  if (!"not_estimable_reasons" %in% names(block)) {
    return(rep(list(NULL), n))
  }
  reasons <- block[["not_estimable_reasons"]]
  if (is.null(reasons)) {
    return(rep(list(NULL), n))
  }
  if (!is.list(reasons) || !is.null(names(reasons)) || length(reasons) != n) {
    vmx_abort_response(
      sprintf(
        "field '%s.not_estimable_reasons' must be an array aligned to gen_subject_uuid.",
        context
      ),
      field = "not_estimable_reasons"
    )
  }
  reasons
}

# Reader for the flat pre-0.3 (0.2.2) `NcaResult`: top-level per-subject parallel
# arrays and one `point_estimates` map. One row per subject; the 0.2.2 server
# carries only a scalar dosing-interval selector (surfaced through the
# `time_basis` / metadata attributes), so there is no per-interval dimension.
# @keywords internal
# @noRd
vmx_nca_result_flat <- function(res) {
  out <- vmx_nca_reshape_block(res, "NCA result")
  metadata <- res[setdiff(
    names(res),
    c("gen_subject_uuid", "subject_id", "point_estimates", "not_estimable_reasons")
  )]
  attr(out, "vmx_metadata") <- metadata
  for (name in c(
    "nca_id", "data_version_id", "status", "time_basis", "units",
    "quantities", "excluded_subjects", "worker_version", "trigger_source",
    "retried_from"
  )) {
    if (name %in% names(res)) attr(out, name) <- res[[name]]
  }
  out
}

# Follow the 0.3 result cursor to the end, returning every `NcaResultItem` across
# all pages plus the first page's envelope (job-wide metadata). Cursors stay
# opaque; a repeated cursor fails loudly instead of looping forever.
# @keywords internal
# @noRd
vmx_nca_collect_items <- function(res, nca_id, client) {
  path <- paste0("/nca-analyses/", nca_id, "/result")
  page <- res
  envelope <- res
  items <- list()
  seen_cursors <- character()
  repeat {
    vmx_validate_response_id(page, "nca_id", nca_id, "NCA result")
    page_items <- vmx_response_field(page, "items", "NCA result.items")
    if (!is.list(page_items) || !is.null(names(page_items))) {
      vmx_abort_response("field 'NCA result.items' must be an array.", field = "items")
    }
    items <- c(items, page_items)
    next_cursor <- vmx_response_field(
      page, "next_cursor", "NCA result.next_cursor", allow_null = TRUE
    )
    if (!is.null(next_cursor)) {
      next_cursor <- vmx_response_scalar(
        next_cursor, "NCA result.next_cursor", type = "character", nonempty = TRUE
      )
    }
    has_next_page <- vmx_response_scalar(
      vmx_response_field(page, "has_next_page", "NCA result.has_next_page"),
      "NCA result.has_next_page",
      type = "logical"
    )
    if (!identical(has_next_page, !is.null(next_cursor))) {
      vmx_abort_response(
        "NCA result has inconsistent 'has_next_page' and 'next_cursor'.",
        field = "has_next_page"
      )
    }
    if (is.null(next_cursor)) break
    if (next_cursor %in% seen_cursors) {
      vmx_abort_response(
        "NCA result returned a repeated pagination cursor.",
        field = "next_cursor"
      )
    }
    seen_cursors <- c(seen_cursors, next_cursor)
    page <- vmx_get(client, path, list(cursor = next_cursor))
  }
  list(items = items, envelope = envelope)
}

# Per-subject interval bounds from a 0.3 item's `resolved_time_intervals_hours`.
# The field is nullable and, when present, aligns to `gen_subject_uuid`; each
# element is null or `{start_time_hours, end_time_hours}`. Returns two numeric
# vectors of length `n` (`NA` where the interval is unresolved for that subject).
# @keywords internal
# @noRd
vmx_nca_interval_bounds <- function(item, context, n) {
  empty <- list(start = rep(NA_real_, n), end = rep(NA_real_, n))
  if (!"resolved_time_intervals_hours" %in% names(item)) {
    return(empty)
  }
  intervals <- item[["resolved_time_intervals_hours"]]
  if (is.null(intervals)) {
    return(empty)
  }
  if (!is.list(intervals) || !is.null(names(intervals)) || length(intervals) != n) {
    vmx_abort_response(
      sprintf(
        "field '%s.resolved_time_intervals_hours' must be an array aligned to gen_subject_uuid.",
        context
      ),
      field = "resolved_time_intervals_hours"
    )
  }
  start <- rep(NA_real_, n)
  end <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    el <- intervals[[i]]
    if (is.null(el)) next
    el_path <- sprintf("%s.resolved_time_intervals_hours[%d]", context, i)
    if (!is.list(el) || is.null(names(el))) {
      vmx_abort_response(
        sprintf("%s must be an object.", el_path),
        field = "resolved_time_intervals_hours"
      )
    }
    start[[i]] <- vmx_response_scalar(
      vmx_response_field(el, "start_time_hours", paste0(el_path, ".start_time_hours")),
      paste0(el_path, ".start_time_hours"),
      type = "numeric"
    )
    end[[i]] <- vmx_response_scalar(
      vmx_response_field(el, "end_time_hours", paste0(el_path, ".end_time_hours")),
      paste0(el_path, ".end_time_hours"),
      type = "numeric"
    )
  }
  list(start = start, end = end)
}

# Reader for the 0.3 cursor-paged result collection. Assembles one row per
# subject per `NcaResultItem`, preserving the per-interval dimension in the
# `item_index` / `label` / `interval_*_hours` columns.
# @keywords internal
# @noRd
vmx_nca_result_paged <- function(res, nca_id, client) {
  collected <- vmx_nca_collect_items(res, nca_id, client)
  items <- collected$items
  envelope <- collected$envelope

  rows <- vector("list", length(items))
  item_indices <- integer(length(items))
  for (i in seq_along(items)) {
    item <- items[[i]]
    context <- sprintf("NCA result.items[%d]", i)
    if (!is.list(item) || is.null(names(item))) {
      vmx_abort_response(sprintf("%s must be an object.", context), field = "items")
    }
    idx_raw <- vmx_response_scalar(
      vmx_response_field(item, "item_index", paste0(context, ".item_index")),
      paste0(context, ".item_index"),
      type = "numeric"
    )
    if (idx_raw < 1 || idx_raw != round(idx_raw)) {
      vmx_abort_response(
        sprintf("%s.item_index must be a positive integer.", context),
        field = "item_index"
      )
    }
    item_index <- as.integer(idx_raw)
    item_indices[[i]] <- item_index
    label <- vmx_response_scalar(
      vmx_response_field(item, "label", paste0(context, ".label")),
      paste0(context, ".label"),
      type = "character",
      nonempty = TRUE
    )
    block <- vmx_nca_reshape_block(item, context)
    bounds <- vmx_nca_interval_bounds(item, context, nrow(block))
    rows[[i]] <- tibble::tibble(
      item_index = item_index,
      label = label,
      interval_start_hours = bounds$start,
      interval_end_hours = bounds$end,
      !!!as.list(block)
    )
  }

  # The collection is contiguous and 1-based across all pages (contract §5.6);
  # a gap or duplicate here means a page was dropped or double-counted.
  if (anyDuplicated(item_indices) ||
      !identical(sort(item_indices), seq_len(length(item_indices)))) {
    vmx_abort_response(
      "NCA result item_index values are not a contiguous 1-based sequence across pages.",
      field = "item_index"
    )
  }

  if (length(rows)) {
    out <- vctrs::vec_rbind(!!!rows)
    # Order by item_index (stable radix sort on the integer key preserves the
    # per-subject order within an item). The contract guarantees ascending order
    # across pages; sorting keeps the result correct even if a server violates it.
    out <- out[order(out$item_index), , drop = FALSE]
    lead <- c(
      "item_index", "label", "interval_start_hours", "interval_end_hours",
      "subject_id", "gen_subject_uuid"
    )
    metrics <- setdiff(names(out), c(lead, "not_estimable_reasons"))
    out <- out[c(lead, metrics, "not_estimable_reasons")]
  } else {
    out <- tibble::tibble(
      item_index = integer(),
      label = character(),
      interval_start_hours = numeric(),
      interval_end_hours = numeric(),
      subject_id = character(),
      gen_subject_uuid = character(),
      not_estimable_reasons = list()
    )
  }

  vmx_nca_attach_paged_metadata(out, envelope, items)
}

# Attach 0.3 job-wide envelope metadata plus the de-duplicated per-item
# quantity/unit descriptors and the job-wide excluded-subject complement.
# @keywords internal
# @noRd
vmx_nca_attach_paged_metadata <- function(out, envelope, items) {
  for (name in c(
    "nca_id", "data_version_id", "status", "worker_version", "trigger_source",
    "retried_from", "stale_data_version", "current_data_version_id",
    "rerun_warning", "inputs"
  )) {
    if (name %in% names(envelope)) attr(out, name) <- envelope[[name]]
  }
  # 0.3 nests the time basis under `inputs`; expose it like the 0.2.2 attribute.
  inputs <- envelope[["inputs"]]
  if (is.list(inputs) && "time_basis" %in% names(inputs)) {
    attr(out, "time_basis") <- inputs[["time_basis"]]
  }
  # quantities/units are per-item in 0.3; expose the de-duplicated union that
  # describes the assembled metric columns (names are unique keys).
  quantities <- list()
  seen_q <- character()
  units <- list()
  for (item in items) {
    for (q in item[["quantities"]] %||% list()) {
      nm <- if (is.list(q)) q[["name"]] else NULL
      if (is.character(nm) && length(nm) == 1L && !nm %in% seen_q) {
        quantities[[length(quantities) + 1L]] <- q
        seen_q <- c(seen_q, nm)
      }
    }
    u <- item[["units"]]
    if (is.list(u)) {
      for (nm in names(u)) if (!nm %in% names(units)) units[[nm]] <- u[[nm]]
    }
  }
  attr(out, "quantities") <- quantities
  attr(out, "units") <- units
  # excluded_subjects is the identical job-wide complement on every item.
  if (length(items)) attr(out, "excluded_subjects") <- items[[1]][["excluded_subjects"]]
  attr(out, "vmx_metadata") <- envelope[setdiff(
    names(envelope), c("items", "next_cursor", "has_next_page")
  )]
  out
}
