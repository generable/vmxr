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
  ps <- if (inherits(dataset, "vmx_prep_status")) dataset else vmx_prep_status(dataset, client = client)
  fields <- ps$prompt$fields
  if (is.null(fields) || !length(fields)) {
    return(tibble::tibble(field = character(0), question = character(0), required = logical(0)))
  }
  rows <- lapply(fields, function(f) {
    tibble::tibble(
      field = f$field %||% NA_character_,
      question = f$question %||% NA_character_,
      required = isTRUE(f$required),
      format = f$format %||% NA_character_,
      options = list(f$options),
      rationale = f$rationale %||% NA_character_
    )
  })
  vctrs::vec_rbind(!!!rows)
}

#' Answer prep questions and resume formatting
#' @param dataset A dataset id or `vmx_dataset`.
#' @param answers A named list mapping each prompt `field` to its answer value.
#' @param client A `vmx_client`.
#' @export
vmx_prep_answer <- function(dataset, answers, client = vmx_client()) {
  if (!is.list(answers) || is.null(names(answers))) {
    vmx_abort("`answers` must be a named list of field -> value.", class = "vmx_usage_error")
  }
  id <- vmx_id(dataset, "ds", arg = "dataset")
  data <- vmx_post(client, paste0("/datasets/", id, "/prep-answers"), as.list(answers))
  new_vmx_resource(data, "vmx_prep_status", "dataset_id")
}
