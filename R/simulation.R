# Simulation — dosing inputs and simulation jobs.

#' Create a dosing input for a fit
#'
#' `POST /model-fits/{mf_id}/simulation-dosing-inputs`.
#'
#' @param fit A fit id (`mf_...`) or `vmx_model_fit`.
#' @param dosing_text The dosing regimen text.
#' @param scenario_name One or more scenario names.
#' @param wait If `TRUE`, block until parsing succeeds or fails.
#' @param ... Polling controls forwarded to [vmx_wait()].
#' @param client A `vmx_client`.
#' @return A `vmx_dosing_input` (carries `dosing_input_id`).
#' @export
vmx_dosing_input <- function(fit, dosing_text, scenario_name,
                             client = vmx_client(), wait = FALSE, ...) {
  dosing_text <- vmx_nonempty_strings(dosing_text, "dosing_text", exactly_one = TRUE)
  scenario_name <- vmx_nonempty_strings(
    scenario_name, "scenario_name", unique = TRUE
  )
  body <- list(dosing_text = dosing_text, scenario_names = as.list(scenario_name))
  data <- vmx_post(client, paste0("/model-fits/", vmx_id(fit, "mf", "fit"),
                                  "/simulation-dosing-inputs"), body)
  input <- new_vmx_resource(data, "vmx_dosing_input", "dosing_input_id")
  vmx_dosing_input_id(input)
  if (isTRUE(wait)) vmx_wait(input, client = client, ...) else input
}

#' Dosing-input status
#' @param dosing_input A dosing-input id or `vmx_dosing_input`.
#' @param client A `vmx_client`.
#' @return A `vmx_dosing_input`.
#' @export
vmx_dosing_input_status <- function(dosing_input, client = vmx_client()) {
  input_id <- vmx_dosing_input_id(dosing_input)
  data <- vmx_get(client, paste0("/simulation-dosing-inputs/", input_id))
  vmx_validate_response_id(data, "dosing_input_id", input_id, "dosing-input status")
  new_vmx_resource(data, "vmx_dosing_input", "dosing_input_id")
}

#' Simulate existing (observed) subjects
#'
#' `POST /model-fits/{mf_id}/existing-subject-simulation-jobs`.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param dosing_input A dosing-input id or `vmx_dosing_input`.
#' @param subjects Subjects to simulate: a data.frame/tibble with
#'   `gen_subject_uuid` + `subject_name` columns, or a list of such records.
#' @param min_timepoints Optional minimum number of simulated timepoints.
#' @param idempotency_key,retried_from Optional create fields.
#' @param wait If `TRUE`, block until the job settles.
#' @param ... Polling controls forwarded to [vmx_wait()].
#' @param client A `vmx_client`.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_existing_subject <- function(fit, dosing_input, subjects,
                                     idempotency_key = NULL, retried_from = NULL,
                                     min_timepoints = NULL,
                                     wait = FALSE, ..., client = vmx_client()) {
  body <- vmx_compact(list(
    dosing_input_id = vmx_dosing_input_id(dosing_input),
    subjects = vmx_rows_to_records(subjects, c("gen_subject_uuid", "subject_name")),
    min_timepoints = min_timepoints,
    idempotency_key = idempotency_key,
    retried_from = retried_from
  ))
  vmx_create_sim_job(fit, "existing-subject-simulation-jobs", body, wait, client, ...)
}

#' Simulate existing subjects from dosing text
#'
#' `POST /model-fits/{mf_id}/existing-subject-simulation-jobs/from-text`.
#'
#' @inheritParams vmx_sim_existing_subject
#' @param dosing_text The dosing regimen text.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_existing_subject_from_text <- function(fit, dosing_text, subjects,
                                               idempotency_key = NULL, retried_from = NULL,
                                               min_timepoints = NULL,
                                               wait = FALSE, ..., client = vmx_client()) {
  dosing_text <- vmx_nonempty_strings(dosing_text, "dosing_text", exactly_one = TRUE)
  body <- vmx_compact(list(
    dosing_text = dosing_text,
    subjects = vmx_rows_to_records(subjects, c("gen_subject_uuid", "subject_name")),
    min_timepoints = min_timepoints,
    idempotency_key = idempotency_key,
    retried_from = retried_from
  ))
  vmx_create_sim_job(fit, "existing-subject-simulation-jobs/from-text", body, wait, client, ...)
}

#' Simulate hypothetical subjects
#'
#' `POST /model-fits/{mf_id}/hypothetical-subject-simulation-jobs`.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param dosing_input A dosing-input id or `vmx_dosing_input`.
#' @param subjects A data.frame/tibble with a `subject_name` column plus one
#'   column per covariate, or a list of `{subject_name, covariates}` records.
#' @param min_timepoints Optional minimum number of simulated timepoints.
#' @param idempotency_key,retried_from Optional create fields.
#' @param wait If `TRUE`, block until the job settles.
#' @param ... Polling controls forwarded to [vmx_wait()].
#' @param client A `vmx_client`.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_hypothetical_subject <- function(fit, dosing_input, subjects,
                                         idempotency_key = NULL, retried_from = NULL,
                                         min_timepoints = NULL,
                                         wait = FALSE, ..., client = vmx_client()) {
  body <- vmx_compact(list(
    dosing_input_id = vmx_dosing_input_id(dosing_input),
    subjects = vmx_hypothetical_records(subjects),
    min_timepoints = min_timepoints,
    idempotency_key = idempotency_key,
    retried_from = retried_from
  ))
  vmx_create_sim_job(fit, "hypothetical-subject-simulation-jobs", body, wait, client, ...)
}

#' Simulate hypothetical subjects from dosing text
#'
#' `POST /model-fits/{mf_id}/hypothetical-subject-simulation-jobs/from-text`.
#'
#' @inheritParams vmx_sim_hypothetical_subject
#' @param dosing_text The dosing regimen text.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_hypothetical_subject_from_text <- function(fit, dosing_text, subjects,
                                                   idempotency_key = NULL, retried_from = NULL,
                                                   min_timepoints = NULL,
                                                   wait = FALSE, ..., client = vmx_client()) {
  dosing_text <- vmx_nonempty_strings(dosing_text, "dosing_text", exactly_one = TRUE)
  body <- vmx_compact(list(
    dosing_text = dosing_text,
    subjects = vmx_hypothetical_records(subjects),
    min_timepoints = min_timepoints,
    idempotency_key = idempotency_key,
    retried_from = retried_from
  ))
  vmx_create_sim_job(fit, "hypothetical-subject-simulation-jobs/from-text", body, wait, client, ...)
}

#' Simulate a population scenario
#'
#' `POST /model-fits/{mf_id}/population-simulation-jobs`.
#'
#' @param fit A fit id or `vmx_model_fit`.
#' @param dosing_input A dosing-input id or `vmx_dosing_input`.
#' @param scenario_name The population scenario name.
#' @param min_timepoints Optional minimum number of simulated timepoints.
#' @param idempotency_key,retried_from Optional create fields.
#' @param wait If `TRUE`, block until the job settles.
#' @param ... Polling controls forwarded to [vmx_wait()].
#' @param client A `vmx_client`.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_population <- function(fit, dosing_input, scenario_name,
                               idempotency_key = NULL, retried_from = NULL,
                               min_timepoints = NULL,
                               wait = FALSE, ..., client = vmx_client()) {
  scenario_name <- vmx_nonempty_strings(scenario_name, "scenario_name", exactly_one = TRUE)
  body <- vmx_compact(list(
    dosing_input_id = vmx_dosing_input_id(dosing_input),
    scenario_name = scenario_name,
    min_timepoints = min_timepoints,
    idempotency_key = idempotency_key,
    retried_from = retried_from
  ))
  vmx_create_sim_job(fit, "population-simulation-jobs", body, wait, client, ...)
}

#' Simulate a population scenario from dosing text
#'
#' `POST /model-fits/{mf_id}/population-simulation-jobs/from-text`.
#'
#' @inheritParams vmx_sim_population
#' @param dosing_text The dosing regimen text.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_population_from_text <- function(fit, dosing_text, scenario_name,
                                         idempotency_key = NULL, retried_from = NULL,
                                         min_timepoints = NULL,
                                         wait = FALSE, ..., client = vmx_client()) {
  dosing_text <- vmx_nonempty_strings(dosing_text, "dosing_text", exactly_one = TRUE)
  scenario_name <- vmx_nonempty_strings(scenario_name, "scenario_name", exactly_one = TRUE)
  body <- vmx_compact(list(
    dosing_text = dosing_text,
    scenario_name = scenario_name,
    min_timepoints = min_timepoints,
    idempotency_key = idempotency_key,
    retried_from = retried_from
  ))
  vmx_create_sim_job(fit, "population-simulation-jobs/from-text", body, wait, client, ...)
}

#' List simulation jobs for a model fit
#' @param fit A fit id or `vmx_model_fit`.
#' @param client A `vmx_client`.
#' @return A tibble containing all simulation jobs for the fit.
#' @export
vmx_sim_jobs <- function(fit, client = vmx_client()) {
  vmx_paginate(
    client,
    paste0("/model-fits/", vmx_id(fit, "mf", "fit"), "/simulation-jobs")
  )
}

#' Simulation job status
#' @param job A job id (`simjob_...`) or `vmx_simulation_job`.
#' @param client A `vmx_client`.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_status <- function(job, client = vmx_client()) {
  job_id <- vmx_id(job, "simjob", "job")
  data <- vmx_get(client, paste0("/simulation-jobs/", job_id))
  vmx_validate_response_id(data, "simulation_job_id", job_id, "simulation status")
  new_vmx_resource(data, "vmx_simulation_job", "simulation_job_id")
}

#' Simulation result
#'
#' `GET /simulation-jobs/{id}/result`. Returns the parsed result payload
#' containing model-implied response trajectories with the server-provided
#' point statistic and interval. The nested wire shape is retained verbatim.
#'
#' @param job A job id or `vmx_simulation_job`.
#' @param grouping_variable Optional server-side grouping.
#' @param client A `vmx_client`.
#' @return A list (the parsed result).
#' @export
vmx_sim_result <- function(job, grouping_variable = NULL, client = vmx_client()) {
  job_id <- vmx_id(job, "simjob", "job")
  if (!is.null(grouping_variable)) {
    grouping_variable <- vmx_nonempty_strings(
      grouping_variable, "grouping_variable", exactly_one = TRUE
    )
  }
  out <- vmx_get(
    client,
    paste0("/simulation-jobs/", job_id, "/result"),
    list(grouping_variable = grouping_variable)
  )
  vmx_validate_sim_result(out, job, grouping_variable)
  out
}

#' Cancel a simulation job
#' @param job A job id or `vmx_simulation_job`.
#' @param client A `vmx_client`.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_cancel <- function(job, client = vmx_client()) {
  job_id <- vmx_id(job, "simjob", "job")
  data <- vmx_post(client, paste0("/simulation-jobs/", job_id, "/cancel"))
  vmx_validate_response_id(data, "simulation_job_id", job_id, "simulation cancellation")
  new_vmx_resource(data, "vmx_simulation_job", "simulation_job_id")
}

# -- internals ---------------------------------------------------------------

vmx_create_sim_job <- function(fit, endpoint, body, wait, client, ...) {
  fit_id <- vmx_id(fit, "mf", "fit")
  if ("min_timepoints" %in% names(body)) {
    body$min_timepoints <- vmx_sim_min_timepoints(body$min_timepoints)
  }
  if ("idempotency_key" %in% names(body)) {
    vmx_id_like_scalar(body$idempotency_key, "idempotency_key")
  }
  if ("retried_from" %in% names(body)) {
    body$retried_from <- vmx_id(
      body$retried_from, "simjob", "retried_from"
    )
  }
  data <- vmx_post(
    client, paste0("/model-fits/", fit_id, "/", endpoint), body
  )
  vmx_validate_response_id(
    data, "model_fit_id", fit_id, "simulation creation"
  )
  job <- new_vmx_resource(data, "vmx_simulation_job", "simulation_job_id")
  if (isTRUE(wait)) vmx_wait(job, client = client, ...) else job
}

vmx_dosing_input_id <- function(x) {
  id <- if (inherits(x, "vmx_resource")) vmx_resource_id(x) else x
  vmx_id(id, "simdose", "dosing_input")
}

# data.frame -> validated list of records with exactly the named columns.
vmx_rows_to_records <- function(x, cols) {
  if (is.data.frame(x)) {
    missing <- setdiff(cols, names(x))
    extra <- setdiff(names(x), cols)
    if (length(missing) || length(extra)) {
      vmx_abort(
        sprintf(
          "`subjects` must have exactly these columns: %s.",
          paste(cols, collapse = ", ")
        ),
        class = "vmx_usage_error"
      )
    }
    if (!nrow(x)) {
      vmx_abort("`subjects` must contain at least one row.",
                class = "vmx_usage_error")
    }
    return(lapply(seq_len(nrow(x)), function(i) {
      stats::setNames(
        lapply(cols, function(cn) vmx_subject_string(x[[cn]][[i]], paste0("subjects$", cn))),
        cols
      )
    }))
  }
  if (!is.list(x) || !length(x)) {
    vmx_abort("`subjects` must be a non-empty data frame or list of records.",
              class = "vmx_usage_error")
  }
  lapply(seq_along(x), function(i) {
    record <- x[[i]]
    if (!is.list(record) || is.null(names(record)) ||
        !setequal(names(record), cols) || anyDuplicated(names(record))) {
      vmx_abort(
        sprintf(
          "`subjects[[%d]]` must have exactly these fields: %s.",
          i, paste(cols, collapse = ", ")
        ),
        class = "vmx_usage_error"
      )
    }
    stats::setNames(
      lapply(cols, function(cn) {
        vmx_subject_string(record[[cn]], sprintf("subjects[[%d]]$%s", i, cn))
      }),
      cols
    )
  })
}

# data.frame with subject_name + covariate columns -> {subject_name, covariates}
vmx_hypothetical_records <- function(x) {
  if (is.data.frame(x)) {
    if (!"subject_name" %in% names(x) || anyDuplicated(names(x))) {
      vmx_abort(
        "`subjects` must have one `subject_name` column plus optional covariate columns.",
        class = "vmx_usage_error"
      )
    }
    if (!nrow(x)) {
      vmx_abort("`subjects` must contain at least one row.",
                class = "vmx_usage_error")
    }
    covcols <- setdiff(names(x), "subject_name")
    return(lapply(seq_len(nrow(x)), function(i) {
      list(
        subject_name = vmx_subject_string(
          x[["subject_name"]][[i]], "subjects$subject_name"
        ),
        covariates = lapply(covcols, function(cn) {
          vmx_covariate_value(x[[cn]][[i]], paste0("subjects$", cn))
        }) |> stats::setNames(covcols)
      )
    }))
  }
  if (!is.list(x) || !length(x)) {
    vmx_abort("`subjects` must be a non-empty data frame or list of records.",
              class = "vmx_usage_error")
  }
  lapply(seq_along(x), function(i) {
    record <- x[[i]]
    if (!is.list(record) || is.null(names(record)) ||
        !setequal(names(record), c("subject_name", "covariates")) ||
        anyDuplicated(names(record))) {
      vmx_abort(
        sprintf(
          "`subjects[[%d]]` must have exactly `subject_name` and `covariates`.",
          i
        ),
        class = "vmx_usage_error"
      )
    }
    covariates <- record$covariates
    if (!is.list(covariates) ||
        (length(covariates) && (is.null(names(covariates)) ||
          anyDuplicated(names(covariates)) || any(!nzchar(names(covariates)))))) {
      vmx_abort(
        sprintf("`subjects[[%d]]$covariates` must be a named list.", i),
        class = "vmx_usage_error"
      )
    }
    list(
      subject_name = vmx_subject_string(
        record$subject_name, sprintf("subjects[[%d]]$subject_name", i)
      ),
      covariates = lapply(seq_along(covariates), function(j) {
        vmx_covariate_value(
          covariates[[j]],
          sprintf("subjects[[%d]]$covariates$%s", i, names(covariates)[[j]])
        )
      }) |> stats::setNames(names(covariates))
    )
  })
}

vmx_subject_string <- function(x, arg) {
  if (is.factor(x)) x <- as.character(x)
  vmx_nonempty_strings(x, arg, exactly_one = TRUE)
  as.character(x)
}

vmx_covariate_value <- function(x, arg) {
  if (is.factor(x)) x <- as.character(x)
  valid_type <- is.character(x) || is.logical(x) || is.numeric(x)
  if (!valid_type || length(x) != 1L || is.na(x) ||
      (is.numeric(x) && !is.finite(x))) {
    vmx_abort(
      sprintf("`%s` must be one non-missing string, number, or logical value.", arg),
      class = "vmx_usage_error"
    )
  }
  x
}

vmx_sim_min_timepoints <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x != floor(x) || x < 10L || x > 600L) {
    vmx_abort(
      "`min_timepoints` must be one integer between 10 and 600.",
      class = "vmx_usage_error"
    )
  }
  as.integer(x)
}

vmx_validate_sim_result <- function(x, job, grouping_variable) {
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x))) {
    vmx_abort_response(
      "simulation result must be an object.",
      field = "simulation_result"
    )
  }
  for (field in c(
    "schema_version", "simulation_version_id", "model_fit_id", "model_type",
    "time_basis", "simulation_kind"
  )) {
    value <- vmx_response_scalar(
      vmx_response_field(x, field, paste0("simulation result.", field)),
      paste0("simulation result.", field),
      type = "character",
      nonempty = TRUE
    )
    if (field == "simulation_version_id") {
      vmx_id(value, "sv", field)
    } else if (field == "model_fit_id") {
      vmx_id(value, "mf", field)
    }
  }
  if (!x$model_type %in% c("pk", "pd")) {
    vmx_abort_response(
      "field 'simulation result.model_type' must be 'pk' or 'pd'.",
      field = "model_type"
    )
  }
  if (!x$simulation_kind %in%
      c("existing_subject", "hypothetical_subject", "population")) {
    vmx_abort_response(
      "field 'simulation result.simulation_kind' is unknown.",
      field = "simulation_kind"
    )
  }
  series <- vmx_response_field(x, "series", "simulation result.series")
  quantities <- vmx_response_field(
    x, "quantities", "simulation result.quantities"
  )
  if (!is.list(series) || !is.null(names(series))) {
    vmx_abort_response(
      "field 'simulation result.series' must be an array.",
      field = "series"
    )
  }
  if (!is.list(quantities) ||
      (length(quantities) && is.null(names(quantities))) ||
      anyDuplicated(names(quantities))) {
    vmx_abort_response(
      "field 'simulation result.quantities' must be an object.",
      field = "quantities"
    )
  }
  for (i in seq_along(series)) {
    if (!is.list(series[[i]]) || is.null(names(series[[i]])) ||
        anyDuplicated(names(series[[i]]))) {
      vmx_abort_response(
        "each simulation result series entry must be an object.",
        field = "series"
      )
    }
  }
  if (inherits(job, "vmx_resource") && !is.null(job[["model_fit_id"]])) {
    vmx_validate_response_id(
      x,
      "model_fit_id",
      vmx_id(job[["model_fit_id"]], "mf", "job$model_fit_id"),
      "simulation result"
    )
  }
  if (!is.null(grouping_variable)) {
    returned_grouping <- vmx_response_scalar(
      vmx_response_field(
        x, "grouping_variable", "simulation result.grouping_variable"
      ),
      "simulation result.grouping_variable",
      type = "character",
      nonempty = TRUE
    )
    if (!identical(returned_grouping, grouping_variable)) {
      vmx_abort_response(
        "simulation result grouping does not match the requested grouping.",
        field = "grouping_variable"
      )
    }
  }
  invisible(x)
}
