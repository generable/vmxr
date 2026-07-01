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
  rows <- list()
  for (category in names(catalog)) {
    for (model in catalog[[category]]) {
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
  vmx_post(client, "/model-catalog/model-description",
           list(model_catalog_name = model_name))
}

#' Preview modeling options for a data version
#' @param data_version A data-version id or `vmx_data_version`.
#' @param time_basis Time basis.
#' @param pd_marker Optional PD marker gen_uuid(s) (character vector).
#' @param covariate Optional covariate name(s).
#' @param client A `vmx_client`.
#' @return A list (the selection preview).
#' @export
vmx_modeling_options <- function(data_version, time_basis, pd_marker = NULL,
                                 covariate = NULL, client = vmx_client()) {
  body <- vmx_compact(list(
    data_version_id = vmx_id(data_version, "dv", "data_version"),
    time_basis = time_basis,
    pd_markers = if (!is.null(pd_marker)) as.list(pd_marker),
    covariates = if (!is.null(covariate)) as.list(covariate)
  ))
  vmx_post(client, "/modeling-options", body)
}

#' Start a model build run (optionally wait)
#'
#' `pd_marker` uses the design's `"GEN_uuid:increasing"` / `":decreasing"`
#' shorthand; it is parsed into the API's `{gen_uuid, direction}` form.
#'
#' @param data_version A data-version id or `vmx_data_version`.
#' @param time_basis Time basis.
#' @param pd_marker Optional `"GEN_uuid:increasing"` / `":decreasing"` string(s).
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
  body <- list(
    data_version_id = vmx_id(data_version, "dv", "data_version"),
    time_basis = time_basis,
    covariates = if (is.null(covariate)) list() else as.list(covariate)
  )
  pdm <- vmx_pd_markers(pd_marker)
  if (!is.null(pdm)) body$pd_markers <- pdm
  if (!is.null(idempotency_key)) body$idempotency_key <- idempotency_key
  if (!is.null(retried_from)) body$retried_from <- retried_from

  run <- new_vmx_resource(vmx_post(client, "/model-build-runs", body),
                          "vmx_model_build_run", "run_id")
  if (isTRUE(wait)) vmx_wait(run, client = client, ...) else run
}

# Parse "GEN_uuid:direction" shorthand into the API's marker objects.
vmx_pd_markers <- function(x) {
  if (is.null(x)) return(NULL)
  lapply(x, function(s) {
    parts <- strsplit(s, ":", fixed = TRUE)[[1]]
    if (length(parts) != 2L || !parts[[2]] %in% c("increasing", "decreasing")) {
      vmx_abort(
        sprintf("pd_marker '%s' must be 'GEN_uuid:increasing' or 'GEN_uuid:decreasing'.", s),
        class = "vmx_usage_error"
      )
    }
    list(gen_uuid = parts[[1]], direction = parts[[2]])
  })
}

#' List model build runs
#' @param data_version,study,treatment,status Optional filters.
#' @param client A `vmx_client`.
#' @return A tibble.
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
  vmx_items_to_tibble(vmx_paginate(client, "/model-build-runs", params))
}

#' Build-run status
#' @param run A build-run id (`run_...`) or object.
#' @param client A `vmx_client`.
#' @return A `vmx_model_build_run`.
#' @export
vmx_model_build_status <- function(run, client = vmx_client()) {
  data <- vmx_get(client, paste0("/model-build-runs/", vmx_id(run, "run"), "/status"))
  new_vmx_resource(data, "vmx_model_build_run", "run_id")
}

#' Build-run logs
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return A tibble of log lines.
#' @export
vmx_model_build_logs <- function(run, client = vmx_client()) {
  vmx_items_to_tibble(vmx_paginate(client, paste0("/model-build-runs/", vmx_id(run, "run"), "/logs")))
}

#' Build-run results
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return A list (fits summary, modeling population, PK structure selection).
#' @export
vmx_model_build_results <- function(run, client = vmx_client()) {
  vmx_get(client, paste0("/model-build-runs/", vmx_id(run, "run"), "/results"))
}

#' Markdown export of a build run
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return The export markdown as a length-1 character vector.
#' @export
vmx_model_build_export <- function(run, client = vmx_client()) {
  req <- httr2::req_headers(
    vmx_req(client, paste0("/model-build-runs/", vmx_id(run, "run"), "/export")),
    Accept = "application/json"
  )
  vmx_perform(req)$content
}

#' Build-run report status (signed HTML report URL when ready)
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return A list with `status` and, when ready, `url`.
#' @export
vmx_model_build_report <- function(run, client = vmx_client()) {
  vmx_get(client, paste0("/model-build-runs/", vmx_id(run, "run"), "/report"))
}

#' Cancel a build run
#' @param run A build-run id or object.
#' @param client A `vmx_client`.
#' @return A `vmx_model_build_run`.
#' @export
vmx_model_build_cancel <- function(run, client = vmx_client()) {
  data <- vmx_post(client, paste0("/model-build-runs/", vmx_id(run, "run"), "/cancel"))
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
#' @param run,data_version,model_type,marker_name,status Optional filters.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_model_fits <- function(run = NULL, data_version = NULL, model_type = NULL,
                           marker_name = NULL, status = NULL,
                           client = vmx_client()) {
  params <- list(
    run_id = vmx_opt_id(run, "run", "run"),
    data_version_id = vmx_opt_id(data_version, "dv", "data_version"),
    model_type = model_type,
    marker_name = marker_name,
    status = status
  )
  vmx_items_to_tibble(vmx_paginate(client, "/model-fits", params))
}

#' Fetch one model fit's details
#' @param id A fit id (`mf_...`) or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A `vmx_model_fit` (metadata / model / inference).
#' @export
vmx_model_fit <- function(id, client = vmx_client()) {
  mf <- vmx_id(id, "mf")
  data <- vmx_get(client, paste0("/model-fits/", mf, "/details"))
  data$model_fit_id <- mf
  new_vmx_resource(data, "vmx_model_fit", "model_fit_id")
}

#' Subject-level parameter estimates (tidy, long)
#'
#' One row per subject x parameter, with the posterior point estimate (`value`)
#' and credible interval (`ci_lower`/`ci_upper`).
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_fit_subject_estimates <- function(fit, client = vmx_client()) {
  d <- vmx_get(client, paste0("/model-fits/", vmx_id(fit, "mf"), "/subject-estimates"))
  subject_id <- vmx_chr(d$subject_id)
  gen_subject_uuid <- vmx_chr(d$gen_subject_uuid)
  n <- length(subject_id)
  rows <- lapply(d$estimates, function(est) {
    tibble::tibble(
      subject_id = subject_id,
      gen_subject_uuid = gen_subject_uuid,
      name = est$name %||% NA_character_,
      display_name = est$display_name %||% NA_character_,
      unit = est$unit %||% NA_character_,
      value = vmx_pad(vmx_num(est$value), n),
      ci_lower = vmx_pad(vmx_num(est$interval$lower), n),
      ci_upper = vmx_pad(vmx_num(est$interval$upper), n),
      value_statistic = est$value_statistic %||% NA_character_,
      kind = est$kind %||% NA_character_,
      model_type = est$model_type %||% NA_character_
    )
  })
  vctrs::vec_rbind(!!!rows)
}

#' Global (population) parameter estimates (tidy)
#'
#' One row per parameter, with the point estimate and credible interval.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_fit_global_estimates <- function(fit, client = vmx_client()) {
  d <- vmx_get(client, paste0("/model-fits/", vmx_id(fit, "mf"), "/global-estimates"))
  rows <- lapply(d$estimates, function(est) {
    tibble::tibble(
      name = est$name %||% NA_character_,
      display_name = est$display_name %||% NA_character_,
      unit = est$unit %||% NA_character_,
      value = vmx_num1(est$value),
      ci_lower = vmx_num1(est$interval$lower),
      ci_upper = vmx_num1(est$interval$upper),
      level = vmx_num1(est$interval$level),
      value_statistic = est$value_statistic %||% NA_character_,
      kind = est$kind %||% NA_character_,
      model_type = est$model_type %||% NA_character_,
      description = est$description %||% NA_character_
    )
  })
  vctrs::vec_rbind(!!!rows)
}

#' Observed-vs-predicted diagnostic artifact
#'
#' Returns the parsed obs-vs-pred artifact (`pk` and `pd_markers` blocks of
#' parallel arrays plus predicted-quantile bands). Tibble reshaping of the plot
#' bands is deferred; see the package NEWS.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A list (the parsed artifact).
#' @export
vmx_fit_obs_vs_pred <- function(fit, client = vmx_client()) {
  vmx_get(client, paste0("/model-fits/", vmx_id(fit, "mf"), "/obs-vs-pred"))
}

#' Visual predictive check artifact
#'
#' Returns the parsed VPC artifact (per dose-group and per-subject quantile
#' bands over time grids). Tibble reshaping is deferred; see the package NEWS.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A list (the parsed artifact).
#' @export
vmx_fit_vpc <- function(fit, client = vmx_client()) {
  vmx_get(client, paste0("/model-fits/", vmx_id(fit, "mf"), "/vpc"))
}

# Pad a numeric vector to length n with NA (guards against absent CI arrays).
vmx_pad <- function(v, n) if (length(v) == n) v else rep(NA_real_, n)
# Scalar coercion with NA for JSON null / absent.
vmx_num1 <- function(v) if (length(v) == 0) NA_real_ else as.numeric(v[[1]])
