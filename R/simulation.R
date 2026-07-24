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
  scenario_name <- vmx_nonempty_strings(scenario_name, "scenario_name")
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
#' @param cursor Opaque cursor returned by [vmx_next_cursor()].
#' @param limit Server page-size hint (1--200).
#' @param client A `vmx_client`.
#' @return One server-owned page as a tibble.
#' @export
vmx_sim_jobs <- function(fit, client = vmx_client(),
                         cursor = NULL, limit = NULL) {
  vmx_get_page(
    client,
    paste0("/model-fits/", vmx_id(fit, "mf", "fit"), "/simulation-jobs"),
    list(cursor = cursor, limit = limit)
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
  vmx_get(client, paste0("/simulation-jobs/", vmx_id(job, "simjob", "job"), "/result"),
          list(grouping_variable = grouping_variable))
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
  data <- vmx_post(client, paste0("/model-fits/", vmx_id(fit, "mf", "fit"), "/", endpoint), body)
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

vmx_nonempty_strings <- function(x, arg, exactly_one = FALSE) {
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x) || !length(x) || anyNA(x) ||
      any(!nzchar(trimws(x))) || (isTRUE(exactly_one) && length(x) != 1L)) {
    count <- if (isTRUE(exactly_one)) "one non-empty string" else "one or more non-empty strings"
    vmx_abort(sprintf("`%s` must be %s.", arg, count), class = "vmx_usage_error")
  }
  as.character(x)
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
