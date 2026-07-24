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
#' @param ... Polling controls forwarded to [vmx_wait()] when `wait = TRUE`.
#' @param client A `vmx_client`.
#' @return A `vmx_dataset` (status `"uploaded"`).
#' @export
vmx_upload <- function(study, files,
                       mode = c("initial", "incremental", "replacement"),
                       treatment = NULL, config = NULL, wait = FALSE,
                       client = vmx_client(), ...) {
  mode <- match.arg(mode)
  files <- vmx_nonempty_strings(files, "files", unique = TRUE)
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
    if (!file.exists(config)) {
      vmx_abort(
        sprintf("Config file not found: %s", config),
        class = "vmx_usage_error"
      )
    }
    parts$config_yaml <- paste(readLines(config, warn = FALSE), collapse = "\n")
  }
  # Repeated `files` form field — a list with duplicate names, spliced in.
  file_parts <- stats::setNames(
    lapply(files, function(p) curl::form_file(p, type = "application/octet-stream")),
    rep("files", length(files))
  )

  req <- httr2::req_method(vmx_req(client, "/datasets"), "POST") |>
    httr2::req_body_multipart(!!!parts, !!!file_parts)
  data <- vmx_perform(req)
  vmx_validate_response_id(data, "study_id", std_id, "dataset upload")
  vmx_validate_response_id(data, "treatment_id", tmt_id, "dataset upload")
  ds <- new_vmx_resource(data, "vmx_dataset", "dataset_id")

  if (isTRUE(wait)) vmx_wait(ds, client = client, ...) else ds
}

#' Resolve the treatment id that owns a study
#'
#' Uses the object's `treatment_id` when `study` is a `vmx_study`/`vmx_dataset`,
#' otherwise fetches the study.
#' @keywords internal
#' @noRd
vmx_study_treatment_id <- function(study, client) {
  if (inherits(study, "vmx_resource") && !is.null(study[["treatment_id"]])) {
    return(vmx_id(study[["treatment_id"]], "tmt", "study$treatment_id"))
  }
  std_id <- vmx_id(study, "std", arg = "study")
  response <- vmx_get(client, paste0("/studies/", std_id))
  vmx_validate_response_id(response, "study_id", std_id, "study")
  vmx_id(
    vmx_response_field(response, "treatment_id", "study.treatment_id"),
    "tmt",
    "study$treatment_id"
  )
}

#' List datasets
#' @param study Optional study filter.
#' @param treatment Optional treatment filter.
#' @param cursor Opaque cursor returned by [vmx_next_cursor()].
#' @param limit Server page-size hint (1--200).
#' @param client A `vmx_client`.
#' @return One server-owned page as a tibble.
#' @export
vmx_datasets <- function(study = NULL, treatment = NULL, client = vmx_client(),
                         cursor = NULL, limit = NULL) {
  params <- list(
    study_id = vmx_opt_id(study, "std", "study"),
    treatment_id = vmx_opt_id(treatment, "tmt", "treatment"),
    cursor = cursor,
    limit = limit
  )
  vmx_get_page(client, "/datasets", params)
}

#' Fetch one dataset
#' @param id A dataset id (`ds_...`) or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A `vmx_dataset`.
#' @export
vmx_dataset <- function(id, client = vmx_client()) {
  dataset_id <- vmx_id(id, "ds")
  data <- vmx_get(client, paste0("/datasets/", dataset_id))
  vmx_validate_response_id(data, "dataset_id", dataset_id, "dataset")
  new_vmx_resource(data, "vmx_dataset", "dataset_id")
}

#' List the files in a dataset
#' @param dataset A dataset id or `vmx_dataset`.
#' @param cursor Opaque cursor returned by [vmx_next_cursor()].
#' @param limit Server page-size hint (1--200).
#' @param client A `vmx_client`.
#' @return One server-owned page as a tibble.
#' @export
vmx_dataset_files <- function(dataset, client = vmx_client(),
                              cursor = NULL, limit = NULL) {
  id <- vmx_id(dataset, "ds", arg = "dataset")
  vmx_get_page(
    client,
    paste0("/datasets/", id, "/files"),
    list(cursor = cursor, limit = limit)
  )
}

#' The tags on a dataset
#'
#' Returns the dataset's allowlisted tag map as a two-column (`key`, `value`)
#' tibble. Reads the tags off a `vmx_dataset` object when given one, else
#' fetches the dataset.
#'
#' @param dataset A dataset id or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A tibble with `key` and `value` columns.
#' @export
vmx_dataset_tags <- function(dataset, client = vmx_client()) {
  tags <- if (inherits(dataset, "vmx_dataset") && !is.null(dataset[["tags"]])) {
    dataset[["tags"]]
  } else {
    vmx_dataset(vmx_id(dataset, "ds", arg = "dataset"), client = client)[["tags"]]
  }
  if (is.null(tags) || !length(tags)) {
    return(tibble::tibble(key = character(0), value = character(0)))
  }
  tibble::tibble(key = names(tags), value = vmx_chr(unname(tags)))
}

#' Cancel a dataset's in-flight format job
#'
#' `POST /datasets/{ds_id}/cancel` — terminates the format job and mints a
#' cancelled DataVersion.
#'
#' @param dataset A dataset id or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return The updated `vmx_prep_status`.
#' @export
vmx_dataset_cancel <- function(dataset, client = vmx_client()) {
  dataset_id <- vmx_id(dataset, "ds", arg = "dataset")
  data <- vmx_post(client, paste0("/datasets/", dataset_id, "/cancel"))
  vmx_validate_response_id(
    data, "dataset_id", dataset_id, "dataset cancellation"
  )
  new_vmx_resource(data, "vmx_prep_status", "dataset_id")
}

#' Download a dataset's files
#'
#' Not implemented: the API's dataset-files listing does not expose per-file
#' download URLs. Use [vmx_data_version_export()] to pull a curated bundle.
#'
#' @param dataset A dataset id or `vmx_dataset`.
#' @param dest Destination directory.
#' @param client A `vmx_client`.
#' @return Not yet implemented.
#' @export
vmx_dataset_download <- function(dataset, dest = ".", client = vmx_client()) {
  vmx_abort_unimplemented("vmx_dataset_download()")
}

#' Ignore an upload within a dataset
#'
#' `POST /datasets/{ds_id}/ignore-upload`. Soft-ignores one delivery.
#'
#' @param dataset A dataset id or `vmx_dataset`.
#' @param upload The upload id (`upl_...`) to ignore.
#' @param client A `vmx_client`.
#' @return The updated `vmx_prep_status`.
#' @export
vmx_upload_ignore <- function(dataset, upload, client = vmx_client()) {
  vmx_set_upload_ignored(dataset, upload, TRUE, client)
}

#' Reverse a prior upload ignore
#'
#' `POST /datasets/{ds_id}/unignore-upload`.
#'
#' @inheritParams vmx_upload_ignore
#' @return The updated `vmx_prep_status`.
#' @export
vmx_upload_unignore <- function(dataset, upload, client = vmx_client()) {
  vmx_set_upload_ignored(dataset, upload, FALSE, client)
}

#' @keywords internal
#' @noRd
vmx_set_upload_ignored <- function(dataset, upload, ignored, client) {
  endpoint <- if (ignored) "ignore-upload" else "unignore-upload"
  upload_id <- vmx_id(upload, "upl", "upload")
  dataset_id <- vmx_id(dataset, "ds", arg = "dataset")
  data <- vmx_post(
    client,
    paste0("/datasets/", dataset_id, "/", endpoint),
    list(upload_id = upload_id)
  )
  vmx_validate_response_id(
    data, "dataset_id", dataset_id, "dataset upload update"
  )
  new_vmx_resource(data, "vmx_prep_status", "dataset_id")
}
