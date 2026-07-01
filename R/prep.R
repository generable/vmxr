# Prep status & answers — the async formatting pipeline.

#' Prep status for a dataset
#'
#' Calls `GET /datasets/{ds_id}/prep-status`.
#'
#' @param dataset A dataset id (`ds_...`) or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A `vmx_prep_status` (`status`, and `data_version_id` once settled).
#' @export
vmx_prep_status <- function(dataset, client = vmx_client()) {
  id <- vmx_id(dataset, "ds", arg = "dataset")
  data <- vmx_get(client, paste0("/datasets/", id, "/prep-status"))
  new_vmx_resource(data, "vmx_prep_status", "dataset_id")
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
