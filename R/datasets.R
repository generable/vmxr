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
  vmx_abort_unimplemented("vmx_upload()")
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
