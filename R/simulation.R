# Simulation — dosing inputs and simulation jobs.

#' Create a dosing input for a fit
#' @param fit A fit id or `vmx_model_fit`.
#' @param dosing_text Dosing regimen text.
#' @param scenario_name Scenario label.
#' @param client A `vmx_client`.
#' @return A `vmx_dosing_input`.
#' @export
vmx_dosing_input <- function(fit, dosing_text, scenario_name,
                             client = vmx_client()) {
  vmx_abort_unimplemented("vmx_dosing_input()")
}

#' Simulate an existing subject
#' @param fit A fit id or `vmx_model_fit`.
#' @param spec Simulation spec.
#' @param wait If `TRUE`, block until the job settles.
#' @param client A `vmx_client`.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_existing_subject <- function(fit, spec, wait = FALSE,
                                     client = vmx_client()) {
  vmx_abort_unimplemented("vmx_sim_existing_subject()")
}

#' Simulate a hypothetical subject
#' @inheritParams vmx_sim_existing_subject
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_hypothetical_subject <- function(fit, spec, wait = FALSE,
                                         client = vmx_client()) {
  vmx_abort_unimplemented("vmx_sim_hypothetical_subject()")
}

#' Simulate a population
#' @inheritParams vmx_sim_existing_subject
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_population <- function(fit, spec, wait = FALSE, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_sim_population()")
}

#' Simulation job status
#' @param job A job id or `vmx_simulation_job`.
#' @param client A `vmx_client`.
#' @return A `vmx_simulation_job`.
#' @export
vmx_sim_status <- function(job, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_sim_status()")
}

#' Simulation result table
#' @param job A job id or `vmx_simulation_job`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_sim_result <- function(job, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_sim_result()")
}

#' Cancel a simulation job
#' @param job A job id or `vmx_simulation_job`.
#' @param client A `vmx_client`.
#' @export
vmx_sim_cancel <- function(job, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_sim_cancel()")
}
