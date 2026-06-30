# Datasets & upload — the workflow entry point.

#' Upload files to a study
#'
#' Streamed multipart upload. With `wait = TRUE`, blocks through the prep
#' pipeline (see [vmx_wait()]).
#'
#' @param study A study id or `vmx_study`.
#' @param files Character vector of local file paths.
#' @param mode One of `"initial"` (auto-formats a default DataVersion),
#'   `"incremental"`, or `"replacement"`.
#' @param treatment Optional treatment; inferred from `study` when possible.
#' @param config Optional gecodata v2 `project.yaml` path (warm-start).
#' @param wait If `TRUE`, block until prep settles.
#' @param client A `vmx_client`.
#' @return A `vmx_dataset` (status `"uploaded"`).
#' @export
vmx_upload <- function(study, files,
                       mode = c("initial", "incremental", "replacement"),
                       treatment = NULL, config = NULL, wait = FALSE,
                       client = vmx_client()) {
  mode <- match.arg(mode)
  std_id <- vmx_id(study, "std", arg = "study")
  tmt_id <- if (!is.null(treatment)) {
    vmx_id(treatment, "tmt", arg = "treatment")
  } else {
    vmx_study_treatment_id(study, client)
  }

  missing <- files[!file.exists(files)]
  if (length(missing)) {
    vmx_abort(
      sprintf("File(s) not found: %s", paste(missing, collapse = ", ")),
      class = "vmx_usage_error"
    )
  }

  parts <- list(treatment_id = tmt_id, study_id = std_id, mode = mode)
  if (!is.null(config)) {
    parts$config_yaml <- curl::form_file(config, type = "application/yaml")
  }
  # Repeated `files` form field — a list with duplicate names, spliced in.
  file_parts <- stats::setNames(
    lapply(files, function(p) curl::form_file(p, type = "application/octet-stream")),
    rep("files", length(files))
  )

  req <- httr2::req_method(vmx_req(client, "/datasets"), "POST") |>
    httr2::req_body_multipart(!!!parts, !!!file_parts)
  ds <- new_vmx_resource(vmx_perform(req), "vmx_dataset", "dataset_id")

  if (isTRUE(wait)) vmx_wait(ds, client = client) else ds
}

#' Resolve the treatment id that owns a study
#'
#' Uses the object's `treatment_id` when `study` is a `vmx_study`/`vmx_dataset`,
#' otherwise fetches the study.
#' @keywords internal
#' @noRd
vmx_study_treatment_id <- function(study, client) {
  if (inherits(study, "vmx_resource") && !is.null(study[["treatment_id"]])) {
    return(study[["treatment_id"]])
  }
  std_id <- vmx_id(study, "std", arg = "study")
  tmt_id <- vmx_get(client, paste0("/studies/", std_id))[["treatment_id"]]
  if (is.null(tmt_id)) {
    vmx_abort(
      sprintf("Could not resolve the treatment for study '%s'; pass `treatment=`.", std_id),
      class = "vmx_usage_error"
    )
  }
  tmt_id
}

#' List datasets
#' @param study Optional study filter.
#' @param treatment Optional treatment filter.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_datasets <- function(study = NULL, treatment = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_datasets()")
}

#' Fetch one dataset
#' @param id A dataset id (`ds_...`) or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A `vmx_dataset`.
#' @export
vmx_dataset <- function(id, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_dataset()")
}

#' List the files in a dataset
#' @param dataset A dataset id or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_dataset_files <- function(dataset, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_dataset_files()")
}

#' List the tags on a dataset
#' @param dataset A dataset id or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_dataset_tags <- function(dataset, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_dataset_tags()")
}

#' Cancel a dataset's in-flight processing
#' @param dataset A dataset id or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A `vmx_dataset`.
#' @export
vmx_dataset_cancel <- function(dataset, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_dataset_cancel()")
}

#' Download a dataset's files
#' @param dataset A dataset id or `vmx_dataset`.
#' @param dest Destination directory.
#' @param client A `vmx_client`.
#' @return Character vector of written file paths.
#' @export
vmx_dataset_download <- function(dataset, dest = ".", client = vmx_client()) {
  vmx_abort_unimplemented("vmx_dataset_download()")
}

#' Ignore an upload
#' @param upload An upload id or object.
#' @param client A `vmx_client`.
#' @export
vmx_upload_ignore <- function(upload, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_upload_ignore()")
}

#' Unignore an upload
#' @param upload An upload id or object.
#' @param client A `vmx_client`.
#' @export
vmx_upload_unignore <- function(upload, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_upload_unignore()")
}
