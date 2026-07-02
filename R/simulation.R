# Simulation — dosing inputs and simulation jobs.

#' Create a dosing input for a fit
#'
#' `POST /model-fits/{mf_id}/simulation-dosing-inputs`.
#'
#' @param fit A fit id (`mf_...`) or `vmx_model_fit`.
#' @param dosing_text The dosing regimen text.
#' @param scenario_name One or more scenario names.
#' @param client A `vmx_client`.
#' @return A `vmx_dosing_input` (carries `dosing_input_id`).
#' @export
vmx_dosing_input <- function(fit, dosing_text, scenario_name,
                             client = vmx_client()) {
  body <- list(dosing_text = dosing_text, scenario_names = as.list(scenario_name))
  data <- vmx_post(client, paste0("/model-fits/", vmx_id(fit, "mf", "fit"),
                                  "/simulation-dosing-inputs"), body)
  new_vmx_resource(data, "vmx_dosing_input", "dosing_input_id")
}

#' Dosing-input status
#' @param dosing_input A dosing-input id or `vmx_dosing_input`.
#' @param client A `vmx_client`.
#' @return A `vmx_dosing_input`.
#' @export
vmx_dosing_input_status <- function(dosing_input, client = vmx_client()) {
  data <- vmx_get(client, paste0("/simulation-dosing-inputs/", vmx_dosing_input_id(dosing_input)))
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
#' @return A tibble.
#' @export
vmx_sim_jobs <- function(fit, client = vmx_client()) {
  vmx_items_to_tibble(vmx_paginate(client, paste0("/model-fits/", vmx_id(fit, "mf", "fit"), "/simulation-jobs")))
}

#' Simulation job status
#' @param job A job id (`simjob_...`) or `vmx_simulation_job`.
#' @param client A `vmx_client`.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_status <- function(job, client = vmx_client()) {
  data <- vmx_get(client, paste0("/simulation-jobs/", vmx_id(job, "simjob", "job")))
  new_vmx_resource(data, "vmx_simulation_job", "simulation_job_id")
}

#' Simulation result
#'
#' `GET /simulation-jobs/{id}/result`. Returns the parsed result payload
#' (subject/time series with prediction bands). Tibble reshaping is deferred
#' pending confirmation of the artifact shape; see the package NEWS.
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
  data <- vmx_post(client, paste0("/simulation-jobs/", vmx_id(job, "simjob", "job"), "/cancel"))
  new_vmx_resource(data, "vmx_simulation_job", "simulation_job_id")
}

# -- internals ---------------------------------------------------------------

vmx_create_sim_job <- function(fit, endpoint, body, wait, client, ...) {
  data <- vmx_post(client, paste0("/model-fits/", vmx_id(fit, "mf", "fit"), "/", endpoint), body)
  job <- new_vmx_resource(data, "vmx_simulation_job", "simulation_job_id")
  if (isTRUE(wait)) vmx_wait(job, client = client, ...) else job
}

vmx_dosing_input_id <- function(x) {
  if (inherits(x, "vmx_resource")) return(vmx_resource_id(x))
  vmx_id(x, NULL, "dosing_input")
}

# data.frame -> list of records with the named columns; pass a list through.
vmx_rows_to_records <- function(x, cols) {
  if (is.data.frame(x)) {
    lapply(seq_len(nrow(x)), function(i) lapply(cols, function(cn) x[[cn]][[i]]) |> stats::setNames(cols))
  } else {
    x
  }
}

# data.frame with subject_name + covariate columns -> {subject_name, covariates}
vmx_hypothetical_records <- function(x) {
  if (is.data.frame(x)) {
    covcols <- setdiff(names(x), "subject_name")
    lapply(seq_len(nrow(x)), function(i) {
      list(
        subject_name = x[["subject_name"]][[i]],
        covariates = lapply(covcols, function(cn) x[[cn]][[i]]) |> stats::setNames(covcols)
      )
    })
  } else {
    x
  }
}
