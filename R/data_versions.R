# Data versions — curated, model-ready data plus the modeling access layer.

#' List data versions
#' @param treatment Optional treatment filter.
#' @param study Optional study filter.
#' @param include_archived Include archived versions.
#' @param eligible_for_modeling Optional modeling-eligibility filter.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_data_versions <- function(treatment = NULL, study = NULL,
                              include_archived = FALSE,
                              eligible_for_modeling = NULL,
                              client = vmx_client()) {
  vmx_abort_unimplemented("vmx_data_versions()")
}

#' Fetch one data version
#' @param id A data-version id (`dv_...`) or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A `vmx_data_version`.
#' @export
vmx_data_version <- function(id, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_data_version()")
}

#' Create a data version
#' @param dataset A dataset id or `vmx_dataset`.
#' @param uploads Uploads to include.
#' @param prior_config Optional prior config for warm-start.
#' @param client A `vmx_client`.
#' @return A `vmx_data_version`.
#' @export
vmx_data_version_create <- function(dataset, uploads, prior_config = NULL,
                                    client = vmx_client()) {
  vmx_abort_unimplemented("vmx_data_version_create()")
}

#' The curated data-version table
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_data_version_table <- function(dv, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_data_version_table()")
}

#' Export a data version
#' @param dv A data-version id or `vmx_data_version`.
#' @param dest Optional destination; when `NULL`, returns the data.
#' @param client A `vmx_client`.
#' @export
vmx_data_version_export <- function(dv, dest = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_data_version_export()")
}

#' Archive a data version
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @export
vmx_data_version_archive <- function(dv, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_data_version_archive()")
}

#' Unarchive a data version
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @export
vmx_data_version_unarchive <- function(dv, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_data_version_unarchive()")
}

# ---- Modeling data access (nlmixr2 / Stan-Torsten) -------------------------

#' Fetch model-ready tidy tables for a data version
#'
#' Returns a `vmx_model_data` bundle with `$subjects`, `$pk`, `$pd`, and
#' `$meta` (units, LLOQ, time basis, analyte/marker manifest, id map, content
#' hash).
#'
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A `vmx_model_data` object.
#' @export
vmx_model_data <- function(dv, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_model_data()")
}

#' Subjects table
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A tibble, one row per subject.
#' @export
vmx_subjects <- function(dv, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_subjects()")
}

#' PK observations and events
#' @param dv A data-version id or `vmx_data_version`.
#' @param analyte Optional analyte filter.
#' @param format `"tidy"` or `"nonmem"`.
#' @param blq Censoring scheme: `"flag"`, `"drop"`, `"loq_half"`, or `"m3"`.
#' @param units `"as_reported"` or `"si"`.
#' @param time_basis `"actual"` or `"nominal"`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_pk <- function(dv, analyte = NULL,
                   format = c("tidy", "nonmem"),
                   blq = c("flag", "drop", "loq_half", "m3"),
                   units = c("as_reported", "si"),
                   time_basis = c("actual", "nominal"),
                   client = vmx_client()) {
  format <- match.arg(format)
  blq <- match.arg(blq)
  units <- match.arg(units)
  time_basis <- match.arg(time_basis)
  vmx_abort_unimplemented("vmx_pk()")
}

#' PD observations
#' @param dv A data-version id or `vmx_data_version`.
#' @param marker Optional PD marker filter.
#' @param format `"tidy"` or `"nonmem"`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_pd <- function(dv, marker = NULL, format = c("tidy", "nonmem"),
                   client = vmx_client()) {
  format <- match.arg(format)
  vmx_abort_unimplemented("vmx_pd()")
}

#' NONMEM-layout data.frame for nlmixr2 / rxode2
#' @param dv A data-version id or `vmx_data_version`.
#' @param analyte Analyte to assemble.
#' @param client A `vmx_client`.
#' @return A data.frame ready for `nlmixr2()`.
#' @export
vmx_nlmixr_data <- function(dv, analyte = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_nlmixr_data()")
}

#' Ragged-array data list for Stan / Torsten
#'
#' Assembles the per-subject `start[i]`/`end[i]` index ranges and the `iObs`
#' observation index that Torsten expects.
#'
#' @param dv A data-version id or `vmx_data_version`.
#' @param analyte Analyte to assemble.
#' @param client A `vmx_client`.
#' @return A named list suitable for `cmdstanr`'s `data=`.
#' @export
vmx_torsten_data <- function(dv, analyte = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_torsten_data()")
}
