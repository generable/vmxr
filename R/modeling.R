# Modeling — catalog, options, build-runs, fits, diagnostics.

#' Model catalog
#'
#' `GET /model-catalog` returns categories of models; this flattens them into
#' one tibble with a `category` column.
#'
#' @param data_version Optional data-version to tailor the catalog to.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_model_catalog <- function(data_version = NULL, client = vmx_client()) {
  catalog <- vmx_get(client, "/model-catalog",
                     list(data_version_id = vmx_opt_id(data_version, "dv", "data_version")))
  if (!is.list(catalog) ||
      (length(catalog) &&
        (is.null(names(catalog)) || any(!nzchar(names(catalog))) ||
          anyDuplicated(names(catalog))))) {
    vmx_abort_response(
      "model catalog must be an object keyed by category.",
      field = "model_catalog"
    )
  }
  rows <- list()
  for (category in names(catalog)) {
    models <- catalog[[category]]
    if (!is.list(models) || !is.null(names(models))) {
      vmx_abort_response(
        "each model catalog category must contain an array of models.",
        field = category
      )
    }
    for (model in models) {
      if (is.list(model) && "category" %in% names(model)) {
        vmx_abort_response(
          "model catalog entry conflicts with the client category column.",
          field = "category"
        )
      }
      row <- vmx_flatten_row(model)
      row$category <- category
      rows[[length(rows) + 1L]] <- row
    }
  }
  if (!length(rows)) return(tibble::tibble())
  vctrs::vec_rbind(!!!rows)
}

#' Describe a model
#' @param model_name Catalog model name.
#' @param client A `vmx_client`.
#' @return A named list.
#' @export
vmx_model_describe <- function(model_name, client = vmx_client()) {
  model_name <- vmx_nonempty_strings(
    model_name, "model_name", exactly_one = TRUE
  )
  out <- vmx_post(
    client, "/model-catalog/model-description",
    list(model_catalog_name = model_name)
  )
  if (!is.list(out) || is.null(names(out)) || any(!nzchar(names(out))) ||
      anyDuplicated(names(out))) {
    vmx_abort_response(
      "model description must be an object of text fields.",
      field = "model_description"
    )
  }
  for (field in names(out)) {
    vmx_response_scalar(
      out[[field]],
      paste0("model description.", field),
      type = "character"
    )
  }
  out
}

#' Preview modeling options for a data version
#' @param data_version A data-version id or `vmx_data_version`.
#' @param time_basis Time basis.
#' @param pd_marker PD marker gen_uuid(s). `NULL` or `character(0)` explicitly
#'   previews a PK-only build; pass marker UUIDs to preview PD modeling.
#' @param covariate Optional covariate name(s).
#' @param client A `vmx_client`.
#' @return A list (the selection preview).
#' @export
vmx_modeling_options <- function(data_version, time_basis, pd_marker = NULL,
                                 covariate = NULL, client = vmx_client()) {
  time_basis <- vmx_nonempty_strings(
    time_basis, "time_basis", exactly_one = TRUE
  )
  markers <- pd_marker %||% character(0)
  if (!is.character(markers) || anyNA(markers) ||
      any(!nzchar(trimws(markers))) || anyDuplicated(markers)) {
    vmx_abort("`pd_marker` must contain non-empty marker UUID strings.",
              class = "vmx_usage_error")
  }
  covariates <- if (is.null(covariate)) {
    NULL
  } else {
    vmx_nonempty_strings(covariate, "covariate", unique = TRUE)
  }
  body <- list(
    data_version_id = vmx_id(data_version, "dv", "data_version"),
    time_basis = time_basis,
    # The API defines null/omitted as "all markers". Send [] deliberately so
    # the default preflight matches vmx_model_build()'s PK-only default.
    pd_markers = as.list(markers)
  )
  if (!is.null(covariates)) body$covariates <- as.list(covariates)
  out <- vmx_post(client, "/modeling-options", body)
  vmx_validate_response_id(
    out, "data_version_id", body$data_version_id, "modeling options"
  )
  out
}

#' Start a model build run (optionally wait)
#'
#' `pd_marker` uses the design's `"GEN_uuid:increasing"` / `":decreasing"`
#' shorthand; it is parsed into the API's `{gen_uuid, direction}` form.
#'
#' @param data_version A data-version id or `vmx_data_version`.
#' @param time_basis Time basis.
#' @param pd_marker Optional `"GEN_uuid:increasing"` / `":decreasing"`
#'   string(s). `NULL` or `character(0)` requests a PK-only build.
#' @param covariate Optional covariate name(s).
#' @param idempotency_key Optional idempotency key.
#' @param retried_from Optional prior run to retry from.
#' @param wait If `TRUE`, block until the run settles.
#' @param ... Polling controls forwarded to [vmx_wait()] when `wait = TRUE`
#'   (e.g. `timeout`, `interval`, `progress`).
#' @param client A `vmx_client`.
#' @return A `vmx_model_build_run`.
#' @export
vmx_model_build <- function(data_version, time_basis, pd_marker = NULL,
                            covariate = NULL, idempotency_key = NULL,
                            retried_from = NULL, wait = FALSE, ...,
                            client = vmx_client()) {
  time_basis <- vmx_nonempty_strings(
    time_basis, "time_basis", exactly_one = TRUE
  )
  covariates <- if (is.null(covariate)) {
    character(0)
  } else {
    vmx_nonempty_strings(covariate, "covariate", unique = TRUE)
  }
  body <- list(
    data_version_id = vmx_id(data_version, "dv", "data_version"),
    time_basis = time_basis,
    covariates = as.list(covariates)
  )
  pdm <- vmx_pd_markers(pd_marker)
  body$pd_markers <- pdm
  if (!is.null(idempotency_key)) {
    vmx_id_like_scalar(idempotency_key, "idempotency_key")
    body$idempotency_key <- idempotency_key
  }
  if (!is.null(retried_from)) {
    body$retried_from <- vmx_id(retried_from, "run", "retried_from")
  }

  data <- vmx_post(client, "/model-build-runs", body)
  vmx_validate_response_id(
    data, "data_version_id", body$data_version_id, "model-build creation"
  )
  run <- new_vmx_resource(
    data, "vmx_model_build_run", "run_id"
  )
  if (isTRUE(wait)) vmx_wait(run, client = client, ...) else run
}

# Parse "GEN_uuid:direction" shorthand into the API's marker objects.
vmx_pd_markers <- function(x) {
  if (is.null(x)) return(list())
  if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x)))) {
    vmx_abort("`pd_marker` must be a character vector.", class = "vmx_usage_error")
  }
  out <- lapply(x, function(s) {
    parts <- strsplit(s, ":", fixed = TRUE)[[1]]
    if (length(parts) != 2L || !nzchar(parts[[1]]) ||
        !parts[[2]] %in% c("increasing", "decreasing")) {
      vmx_abort(
        sprintf("pd_marker '%s' must be 'GEN_uuid:increasing' or 'GEN_uuid:decreasing'.", s),
        class = "vmx_usage_error"
      )
    }
    list(gen_uuid = parts[[1]], direction = parts[[2]])
  })
  ids <- vapply(out, `[[`, character(1), "gen_uuid")
  if (anyDuplicated(ids)) {
    vmx_abort("`pd_marker` contains a duplicate marker UUID.",
              class = "vmx_usage_error")
  }
  out
}

#' List model build runs
#' @param data_version,study,treatment,status Optional filters.
#' @param client A `vmx_client`.
#' @return A tibble containing all matching model-build runs.
#' @export
vmx_model_build_runs <- function(data_version = NULL, study = NULL,
                                 treatment = NULL, status = NULL,
                                 client = vmx_client()) {
  params <- list(
    data_version_id = vmx_opt_id(data_version, "dv", "data_version"),
    study_id = vmx_opt_id(study, "std", "study"),
    treatment_id = vmx_opt_id(treatment, "tmt", "treatment"),
    status = status
  )
  vmx_paginate(client, "/model-build-runs", params)
}

#' Build-run status
#' @param run A build-run id (`run_...`) or object.
#' @param client A `vmx_client`.
#' @return A `vmx_model_build_run`.
#' @export
vmx_model_build_status <- function(run, client = vmx_client()) {
  run_id <- vmx_id(run, "run")
  data <- vmx_get(client, paste0("/model-build-runs/", run_id, "/status"))
  vmx_validate_response_id(data, "run_id", run_id, "model-build status")
  new_vmx_resource(data, "vmx_model_build_run", "run_id")
}

#' Build-run logs
#' @param run A build-run id or object.
#' @param order Newest-first (`"desc"`) or oldest-first (`"asc"`).
#' @param client A `vmx_client`.
#' @return A tibble containing all log lines in the requested order.
#' @export
vmx_model_build_logs <- function(run, client = vmx_client(),
                                 order = c("desc", "asc")) {
  order <- match.arg(order)
  vmx_paginate(
    client,
    paste0("/model-build-runs/", vmx_id(run, "run"), "/logs"),
    list(order = order)
  )
}

#' Build-run results
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return A list (fits summary, modeling population, PK structure selection).
#' @export
vmx_model_build_results <- function(run, client = vmx_client()) {
  run_id <- vmx_id(run, "run")
  out <- vmx_get(client, paste0("/model-build-runs/", run_id, "/results"))
  vmx_validate_response_id(out, "run_id", run_id, "model-build results")
  out
}

#' Markdown export of a build run
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return The export markdown as a length-1 character vector.
#' @export
vmx_model_build_export <- function(run, client = vmx_client()) {
  run_id <- vmx_id(run, "run")
  req <- httr2::req_headers(
    vmx_req(client, paste0("/model-build-runs/", run_id, "/export")),
    Accept = "application/json"
  )
  out <- vmx_perform(req)
  vmx_validate_response_id(out, "run_id", run_id, "model-build export")
  vmx_response_scalar(
    vmx_response_field(out, "content", "model-build export.content"),
    "model-build export.content",
    type = "character"
  )
}

#' Build-run report status (signed HTML report URL when ready)
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return A list with `status` and, when ready, `url`.
#' @export
vmx_model_build_report <- function(run, client = vmx_client()) {
  run_id <- vmx_id(run, "run")
  out <- vmx_get(client, paste0("/model-build-runs/", run_id, "/report"))
  vmx_validate_response_id(out, "run_id", run_id, "model-build report")
  out
}

#' Request build-run report generation
#'
#' `POST /model-build-runs/{run_id}/report` queues HTML report generation.
#'
#' @param run A build-run id or object.
#' @param subject_plot_mode One of `"all"` or `"none"`.
#' @param client A `vmx_client`.
#' @return A list with report status.
#' @export
vmx_model_build_report_create <- function(run, subject_plot_mode = c("all", "none"),
                                          client = vmx_client()) {
  subject_plot_mode <- match.arg(subject_plot_mode)
  run_id <- vmx_id(run, "run")
  out <- vmx_post(
    client,
    paste0("/model-build-runs/", run_id, "/report"),
    list(subject_plot_mode = subject_plot_mode)
  )
  vmx_validate_response_id(out, "run_id", run_id, "model-build report request")
  out
}

#' Cancel a build run
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return A `vmx_model_build_run`.
#' @export
vmx_model_build_cancel <- function(run, client = vmx_client()) {
  run_id <- vmx_id(run, "run")
  data <- vmx_post(client, paste0("/model-build-runs/", run_id, "/cancel"))
  vmx_validate_response_id(data, "run_id", run_id, "model-build cancellation")
  new_vmx_resource(data, "vmx_model_build_run", "run_id")
}

#' Build-run events (SSE stream)
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return Not yet implemented.
#' @export
vmx_model_build_events <- function(run, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_events()")
}

#' Download build-run artifacts
#' @param run A build-run id or object.
#' @param dest Destination directory.
#' @param client A `vmx_client`.
#' @return Not yet implemented.
#' @export
vmx_model_build_artifacts <- function(run, dest = ".", client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_build_artifacts()")
}

# ---- Fits ------------------------------------------------------------------

#' List model fits
#' @param run Optional model-build run filter.
#' @param data_version Optional data-version filter.
#' @param treatment Optional treatment filter.
#' @param study Optional study filter.
#' @param model_type Optional model-type filter.
#' @param marker_name Optional marker-name filter.
#' @param source_pk_model_fit Optional source PK model-fit filter.
#' @param status Optional model-fit status filter.
#' @param client A `vmx_client`.
#' @return A tibble containing all matching model fits.
#' @export
vmx_model_fits <- function(run = NULL, data_version = NULL, model_type = NULL,
                           marker_name = NULL, status = NULL,
                           client = vmx_client(), treatment = NULL, study = NULL,
                           source_pk_model_fit = NULL) {
  params <- list(
    run_id = vmx_opt_id(run, "run", "run"),
    data_version_id = vmx_opt_id(data_version, "dv", "data_version"),
    treatment_id = vmx_opt_id(treatment, "tmt", "treatment"),
    study_id = vmx_opt_id(study, "std", "study"),
    model_type = model_type,
    marker_name = marker_name,
    source_pk_model_fit_id = vmx_opt_id(
      source_pk_model_fit, "mf", "source_pk_model_fit"
    ),
    status = status
  )
  vmx_paginate(client, "/model-fits", params)
}

#' Fetch one model fit's details
#' @param id A fit id (`mf_...`) or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A `vmx_model_fit` (metadata / model / inference).
#' @export
vmx_model_fit <- function(id, client = vmx_client()) {
  mf <- vmx_id(id, "mf")
  data <- vmx_get(client, paste0("/model-fits/", mf, "/details"))
  metadata <- vmx_response_field(data, "metadata", "model-fit details.metadata")
  vmx_validate_response_id(
    metadata, "model_fit_id", mf, "model-fit details metadata"
  )
  data$model_fit_id <- mf
  new_vmx_resource(data, "vmx_model_fit", "model_fit_id")
}

#' Model-fit postprocessor status
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A list with postprocessor status.
#' @export
vmx_model_fit_postprocessor_status <- function(fit, client = vmx_client()) {
  fit_id <- vmx_id(fit, "mf")
  out <- vmx_get(client, paste0("/model-fits/", fit_id, "/postprocessor-status"))
  vmx_validate_response_id(out, "model_fit_id", fit_id, "postprocessor status")
  out
}

#' Subject-level parameter estimates (tidy, long)
#'
#' One row per subject x estimate. `value_statistic`, `interval_kind`, and
#' `interval_level` preserve the server-selected estimate semantics;
#' `interval_lower` and `interval_upper` are the corresponding bounds. Tagged
#' estimate metadata is retained as columns rather than interpreted by vmxr.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_fit_subject_estimates <- function(fit, client = vmx_client()) {
  fit_id <- vmx_id(fit, "mf")
  d <- vmx_get(client, paste0("/model-fits/", fit_id, "/subject-estimates"))
  vmx_validate_response_id(d, "model_fit_id", fit_id, "subject estimates")
  gen_subject_uuid <- vmx_response_vector(
    vmx_response_field(d, "gen_subject_uuid", "subject estimates.gen_subject_uuid"),
    "subject estimates.gen_subject_uuid",
    type = "character"
  )
  if (anyDuplicated(gen_subject_uuid)) {
    vmx_abort_response(
      "field 'subject estimates.gen_subject_uuid' contains duplicate subject keys.",
      field = "gen_subject_uuid"
    )
  }
  n <- length(gen_subject_uuid)
  subject_id <- vmx_response_vector(
    vmx_response_field(d, "subject_id", "subject estimates.subject_id"),
    "subject estimates.subject_id",
    type = "character",
    size = n
  )
  estimates <- vmx_estimate_rows(d, "subject estimates")
  rows <- lapply(seq_along(estimates), function(i) {
    core <- vmx_estimate_core(
      estimates[[i]],
      sprintf("subject estimates.estimates[%d]", i),
      size = n,
      tagged = TRUE
    )
    base <- tibble::tibble(
      subject_id = subject_id,
      gen_subject_uuid = gen_subject_uuid,
      value = core$value,
      interval_lower = core$interval_lower,
      interval_upper = core$interval_upper,
      value_statistic = rep(core$value_statistic, n),
      interval_kind = rep(core$interval_kind, n),
      interval_level = rep(core$interval_level, n)
    )
    vctrs::vec_cbind(
      base,
      vmx_estimate_metadata(
        estimates[[i]],
        n,
        sprintf("subject estimates.estimates[%d]", i)
      )
    )
  })
  out <- if (length(rows)) {
    vctrs::vec_rbind(!!!rows)
  } else {
    vmx_empty_estimate_tibble(subject = TRUE)
  }
  attr(out, "model_fit_id") <- fit_id
  if ("schema_version" %in% names(d)) attr(out, "schema_version") <- d$schema_version
  out
}

#' Global (population) parameter estimates (tidy)
#'
#' One row per estimate, preserving the server-selected point statistic,
#' interval kind, interval level, and tagged estimate metadata.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_fit_global_estimates <- function(fit, client = vmx_client()) {
  fit_id <- vmx_id(fit, "mf")
  d <- vmx_get(client, paste0("/model-fits/", fit_id, "/global-estimates"))
  vmx_validate_response_id(d, "model_fit_id", fit_id, "global estimates")
  estimates <- vmx_estimate_rows(d, "global estimates")
  rows <- lapply(seq_along(estimates), function(i) {
    core <- vmx_estimate_core(
      estimates[[i]],
      sprintf("global estimates.estimates[%d]", i),
      size = NULL,
      tagged = TRUE
    )
    base <- tibble::tibble(
      value = core$value,
      interval_lower = core$interval_lower,
      interval_upper = core$interval_upper,
      value_statistic = core$value_statistic,
      interval_kind = core$interval_kind,
      interval_level = core$interval_level
    )
    vctrs::vec_cbind(
      base,
      vmx_estimate_metadata(
        estimates[[i]],
        1L,
        sprintf("global estimates.estimates[%d]", i)
      )
    )
  })
  out <- if (length(rows)) {
    vctrs::vec_rbind(!!!rows)
  } else {
    vmx_empty_estimate_tibble(subject = FALSE)
  }
  attr(out, "model_fit_id") <- fit_id
  if ("schema_version" %in% names(d)) attr(out, "schema_version") <- d$schema_version
  out
}

#' Observed-vs-predicted diagnostic (tidy)
#'
#' Reshapes the PK block into one row per observation. The returned prediction
#' columns retain the server-selected point statistic and interval semantics.
#' A named list of equivalently reshaped PD-marker tibbles is attached as the
#' `"pd_markers"` attribute; units and marker references remain attributes on
#' their respective tibbles.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A tibble (one row per PK observation).
#' @export
vmx_fit_obs_vs_pred <- function(fit, client = vmx_client()) {
  fit_id <- vmx_id(fit, "mf")
  art <- vmx_get(client, paste0("/model-fits/", fit_id, "/obs-vs-pred"))
  vmx_validate_response_id(art, "model_fit_id", fit_id, "observed-vs-predicted")
  pk <- vmx_response_field(art, "pk", "observed-vs-predicted.pk")
  out <- vmx_obs_vs_pred_block(
    pk,
    "observed-vs-predicted.pk",
    observed_field = "observed_concentration",
    predicted_field = "predicted_concentration",
    pk = TRUE
  )
  pd_payload <- vmx_response_field(
    art, "pd_markers", "observed-vs-predicted.pd_markers"
  )
  if (!is.list(pd_payload) || is.null(names(pd_payload)) ||
      any(!nzchar(names(pd_payload))) || anyDuplicated(names(pd_payload))) {
    vmx_abort_response(
      "field 'observed-vs-predicted.pd_markers' must be an object keyed by marker name.",
      field = "pd_markers"
    )
  }
  pd_markers <- lapply(seq_along(pd_payload), function(i) {
    marker_name <- names(pd_payload)[[i]]
    vmx_obs_vs_pred_block(
      pd_payload[[i]],
      sprintf("observed-vs-predicted.pd_markers[%d]", i),
      observed_field = "observed",
      predicted_field = "predicted",
      pk = FALSE,
      marker_name = marker_name
    )
  })
  names(pd_markers) <- names(pd_payload)
  attr(out, "pd_markers") <- pd_markers
  attr(out, "model_fit_id") <- fit_id
  out
}

#' Visual predictive check artifact
#'
#' Returns the parsed Visual Predictive Check artifact. Its subject and
#' dose-group channels contain model-implied response trajectories with the
#' server-provided point statistic and interval. The nested wire shape is
#' retained verbatim.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A list (the parsed artifact).
#' @export
vmx_fit_vpc <- function(fit, client = vmx_client()) {
  fit_id <- vmx_id(fit, "mf")
  out <- vmx_get(client, paste0("/model-fits/", fit_id, "/vpc"))
  vmx_validate_response_id(out, "model_fit_id", fit_id, "VPC")
  out
}

# ---- Estimate response validation -----------------------------------------

vmx_estimate_rows <- function(payload, context) {
  rows <- vmx_response_field(payload, "estimates", paste0(context, ".estimates"))
  if (!is.list(rows) || !is.null(names(rows))) {
    vmx_abort_response(
      sprintf("field '%s.estimates' must be an array.", context),
      field = "estimates"
    )
  }
  rows
}

vmx_estimate_core <- function(est, context, size = NULL, tagged = FALSE) {
  if (!is.list(est) || is.null(names(est)) || anyDuplicated(names(est))) {
    vmx_abort_response(sprintf("%s must be an object.", context), field = context)
  }
  if (isTRUE(tagged)) {
    for (field in c("kind", "name", "display_name", "model_type", "unit")) {
      vmx_response_scalar(
        vmx_response_field(est, field, paste0(context, ".", field)),
        paste0(context, ".", field),
        type = "character",
        nonempty = TRUE
      )
    }
    model_type <- est$model_type
    if (!model_type %in% c("pk", "pd")) {
      vmx_abort_response(
        sprintf("field '%s.model_type' must be 'pk' or 'pd'.", context),
        field = paste0(context, ".model_type")
      )
    }
    if (identical(model_type, "pd")) {
      vmx_response_scalar(
        vmx_response_field(est, "marker_name", paste0(context, ".marker_name")),
        paste0(context, ".marker_name"),
        type = "character",
        nonempty = TRUE
      )
    }
  }
  value_statistic <- vmx_response_scalar(
    vmx_response_field(est, "value_statistic", paste0(context, ".value_statistic")),
    paste0(context, ".value_statistic"),
    type = "character",
    nonempty = TRUE
  )
  interval <- vmx_response_field(est, "interval", paste0(context, ".interval"))
  if (!is.list(interval) || is.null(names(interval))) {
    vmx_abort_response(
      sprintf("field '%s.interval' must be an object.", context),
      field = paste0(context, ".interval")
    )
  }
  interval_kind <- vmx_response_scalar(
    vmx_response_field(interval, "kind", paste0(context, ".interval.kind")),
    paste0(context, ".interval.kind"),
    type = "character",
    nonempty = TRUE
  )
  interval_level <- vmx_response_scalar(
    vmx_response_field(interval, "level", paste0(context, ".interval.level")),
    paste0(context, ".interval.level"),
    type = "numeric"
  )
  if (interval_level <= 0 || interval_level > 1) {
    vmx_abort_response(
      sprintf("field '%s.interval.level' must be in (0, 1].", context),
      field = paste0(context, ".interval.level")
    )
  }
  if (is.null(size)) {
    value <- vmx_response_scalar(
      vmx_response_field(est, "value", paste0(context, ".value")),
      paste0(context, ".value"),
      type = "numeric"
    )
    lower <- vmx_response_scalar(
      vmx_response_field(interval, "lower", paste0(context, ".interval.lower")),
      paste0(context, ".interval.lower"),
      type = "numeric"
    )
    upper <- vmx_response_scalar(
      vmx_response_field(interval, "upper", paste0(context, ".interval.upper")),
      paste0(context, ".interval.upper"),
      type = "numeric"
    )
  } else {
    value <- vmx_response_vector(
      vmx_response_field(est, "value", paste0(context, ".value")),
      paste0(context, ".value"),
      type = "numeric",
      size = size
    )
    lower <- vmx_response_vector(
      vmx_response_field(interval, "lower", paste0(context, ".interval.lower")),
      paste0(context, ".interval.lower"),
      type = "numeric",
      size = size
    )
    upper <- vmx_response_vector(
      vmx_response_field(interval, "upper", paste0(context, ".interval.upper")),
      paste0(context, ".interval.upper"),
      type = "numeric",
      size = size
    )
  }
  if (any(lower > upper)) {
    vmx_abort_response(
      sprintf("field '%s.interval' has a lower bound above its upper bound.", context),
      field = paste0(context, ".interval")
    )
  }
  list(
    value = value,
    interval_lower = lower,
    interval_upper = upper,
    value_statistic = value_statistic,
    interval_kind = interval_kind,
    interval_level = interval_level
  )
}

# Preserve every additive TaggedEstimate metadata field. Known fields remain
# ordinary columns; future scalar fields do too, while nested metadata becomes
# a list-column rather than being discarded.
vmx_estimate_metadata <- function(est, size, context) {
  core_fields <- c("value", "value_statistic", "interval")
  reserved <- c(
    "subject_id", "gen_subject_uuid", "value", "interval_lower",
    "interval_upper", "value_statistic", "interval_kind", "interval_level"
  )
  fields <- setdiff(names(est), core_fields)
  collision <- intersect(fields, reserved)
  if (length(collision)) {
    vmx_abort_response(
      sprintf("%s metadata conflicts with a tidy estimate column.", context),
      field = collision[[1]]
    )
  }
  columns <- list()
  for (field in fields) {
    value <- est[[field]]
    if (is.null(value)) next
    columns[[field]] <- if (!is.list(value) && length(value) == 1L && !is.na(value)) {
      rep(value, size)
    } else {
      rep(list(value), size)
    }
  }
  tibble::as_tibble(columns)
}

vmx_empty_estimate_tibble <- function(subject) {
  out <- tibble::tibble(
    value = numeric(0),
    interval_lower = numeric(0),
    interval_upper = numeric(0),
    value_statistic = character(0),
    interval_kind = character(0),
    interval_level = numeric(0),
    kind = character(0),
    name = character(0),
    display_name = character(0),
    model_type = character(0),
    unit = character(0)
  )
  if (isTRUE(subject)) {
    out <- vctrs::vec_cbind(
      tibble::tibble(
        subject_id = character(0),
        gen_subject_uuid = character(0)
      ),
      out
    )
  }
  out
}

vmx_obs_vs_pred_block <- function(block, context, observed_field,
                                  predicted_field, pk, marker_name = NULL) {
  if (!is.list(block) || is.null(names(block))) {
    vmx_abort_response(sprintf("%s must be an object.", context), field = context)
  }
  gen_measurement_uuid <- vmx_response_vector(
    vmx_response_field(block, "gen_measurement_uuid", paste0(context, ".gen_measurement_uuid")),
    paste0(context, ".gen_measurement_uuid"),
    type = "character"
  )
  if (anyDuplicated(gen_measurement_uuid)) {
    vmx_abort_response(
      sprintf("field '%s.gen_measurement_uuid' contains duplicate measurement keys.", context),
      field = "gen_measurement_uuid"
    )
  }
  n <- length(gen_measurement_uuid)
  gen_subject_uuid <- vmx_response_vector(
    vmx_response_field(block, "gen_subject_uuid", paste0(context, ".gen_subject_uuid")),
    paste0(context, ".gen_subject_uuid"),
    type = "character",
    size = n
  )
  subject_id <- vmx_response_vector(
    vmx_response_field(block, "subject_id", paste0(context, ".subject_id")),
    paste0(context, ".subject_id"),
    type = "character",
    size = n
  )
  time <- vmx_response_vector(
    vmx_response_field(block, "time", paste0(context, ".time")),
    paste0(context, ".time"),
    type = "numeric",
    size = n
  )
  observed <- vmx_response_vector(
    vmx_response_field(block, observed_field, paste0(context, ".", observed_field)),
    paste0(context, ".", observed_field),
    type = "numeric",
    size = n,
    nullable = TRUE
  )
  predicted <- vmx_response_field(
    block, predicted_field, paste0(context, ".", predicted_field)
  )
  estimate <- vmx_estimate_core(
    predicted,
    paste0(context, ".", predicted_field),
    size = n,
    tagged = FALSE
  )
  out <- tibble::tibble(
    gen_measurement_uuid = gen_measurement_uuid,
    gen_subject_uuid = gen_subject_uuid,
    subject_id = subject_id,
    time = time
  )
  out[[observed_field]] <- observed
  if (isTRUE(pk)) {
    for (field in c("is_bloq", "is_aloq")) {
      out[[field]] <- vmx_response_vector(
        vmx_response_field(block, field, paste0(context, ".", field)),
        paste0(context, ".", field),
        type = "logical",
        size = n
      )
    }
    for (field in c("lloq", "uloq")) {
      if (field %in% names(block)) {
        out[[field]] <- vmx_response_vector(
          block[[field]],
          paste0(context, ".", field),
          type = "numeric",
          size = n,
          nullable = TRUE
        )
      }
    }
  } else {
    marker <- vmx_response_field(block, "marker", paste0(context, ".marker"))
    marker_gen_uuid <- vmx_response_scalar(
      vmx_response_field(marker, "gen_uuid", paste0(context, ".marker.gen_uuid")),
      paste0(context, ".marker.gen_uuid"),
      type = "character",
      nonempty = TRUE
    )
    marker_value <- vmx_response_scalar(
      vmx_response_field(marker, "name", paste0(context, ".marker.name")),
      paste0(context, ".marker.name"),
      type = "character",
      nonempty = TRUE
    )
    if (!identical(marker_value, marker_name)) {
      vmx_abort_response(
        sprintf("field '%s.marker.name' does not match its pd_markers map key.", context),
        field = paste0(context, ".marker.name")
      )
    }
    attr(out, "marker") <- list(
      gen_uuid = marker_gen_uuid,
      name = marker_value
    )
  }
  out$predicted_value <- estimate$value
  out$predicted_interval_lower <- estimate$interval_lower
  out$predicted_interval_upper <- estimate$interval_upper
  out$predicted_value_statistic <- rep(estimate$value_statistic, n)
  out$predicted_interval_kind <- rep(estimate$interval_kind, n)
  out$predicted_interval_level <- rep(estimate$interval_level, n)
  units <- vmx_response_field(block, "units", paste0(context, ".units"))
  required_units <- if (isTRUE(pk)) {
    c("time", "concentration")
  } else {
    c("time", "observed", "predicted")
  }
  if (!is.list(units) || is.null(names(units)) || anyDuplicated(names(units)) ||
      !all(required_units %in% names(units))) {
    vmx_abort_response(
      sprintf("field '%s.units' is missing required unit labels.", context),
      field = paste0(context, ".units")
    )
  }
  for (field in required_units) {
    vmx_response_scalar(
      units[[field]],
      paste0(context, ".units.", field),
      type = "character",
      nonempty = TRUE
    )
  }
  attr(out, "units") <- units
  out
}
