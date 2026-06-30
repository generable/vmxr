# Prep status & answers — the async formatting pipeline.

#' Prep status for a dataset
#' @param dataset A dataset id or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A `vmx_prep_status` (state, `data_version_id` when settled).
#' @export
vmx_prep_status <- function(dataset, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_prep_status()")
}

#' Questions raised by prep (when awaiting input)
#' @param dataset A dataset id or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A tibble of pending questions.
#' @export
vmx_prep_questions <- function(dataset, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_prep_questions()")
}

#' Answer prep questions and resume formatting
#' @param dataset A dataset id or `vmx_dataset`.
#' @param answers A named list, data.frame, or path to an answers file.
#' @param client A `vmx_client`.
#' @export
vmx_prep_answer <- function(dataset, answers, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_prep_answer()")
}
