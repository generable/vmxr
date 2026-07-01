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
  params <- list(
    treatment_id = vmx_opt_id(treatment, "tmt", "treatment"),
    study_id = vmx_opt_id(study, "std", "study"),
    include_archived = include_archived,
    eligible_for_modeling = eligible_for_modeling
  )
  vmx_items_to_tibble(vmx_paginate(client, "/data-versions", params))
}

#' Fetch one data version
#' @param id A data-version id (`dv_...`) or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A `vmx_data_version`.
#' @export
vmx_data_version <- function(id, client = vmx_client()) {
  data <- vmx_get(client, paste0("/data-versions/", vmx_id(id, "dv")))
  new_vmx_resource(data, "vmx_data_version", "data_version_id")
}

#' Create a data version
#'
#' Starts a format job over an explicit upload composition
#' (`POST /datasets/{ds_id}/data-versions`). Returns the in-flight prep-status;
#' poll it with [vmx_wait()].
#'
#' @param dataset A dataset id (`ds_...`) or `vmx_dataset`.
#' @param uploads Character vector of upload ids (`upl_...`) to format over.
#' @param prior_config Optional prior data-version (`dv_...`) whose config seeds
#'   this job (the config-update lineage pointer).
#' @param client A `vmx_client`.
#' @return A `vmx_prep_status` for the new job.
#' @export
vmx_data_version_create <- function(dataset, uploads, prior_config = NULL,
                                    client = vmx_client()) {
  body <- vmx_compact(list(
    upload_ids = as.list(uploads),
    prior_config_data_version_id = vmx_opt_id(prior_config, "dv", "prior_config")
  ))
  data <- vmx_post(client, paste0("/datasets/", vmx_id(dataset, "ds", "dataset"),
                                  "/data-versions"), body)
  new_vmx_resource(data, "vmx_prep_status", "dataset_id")
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
#'
#' Fetches the signed-URL export envelope (`GET /data-versions/{id}/export`).
#' When `dest` is supplied the bundle is streamed to that path; otherwise the
#' parsed envelope (including the signed `download_url`) is returned.
#'
#' @param dv A data-version id or `vmx_data_version`.
#' @param dest Optional local file path to stream the bundle to.
#' @param client A `vmx_client`.
#' @return The export envelope (list), or, when `dest` is set, `dest` invisibly.
#' @export
vmx_data_version_export <- function(dv, dest = NULL, client = vmx_client()) {
  envelope <- vmx_get(client, paste0("/data-versions/", vmx_id(dv, "dv"), "/export"))
  if (is.null(dest)) {
    return(envelope)
  }
  url <- envelope$download_url %||% envelope$url
  if (is.null(url)) {
    vmx_abort("Export envelope did not contain a download URL.", class = "vmx_api_error")
  }
  # Anonymous request: the signed URL carries its own credentials; sending the
  # API bearer to GCS would leak the token and is rejected anyway.
  httr2::request(url) |>
    httr2::req_perform(path = dest)
  invisible(dest)
}

#' Archive a data version
#' @param dv A data-version id or `vmx_data_version`.
#' @param reason Optional free-text reason.
#' @param client A `vmx_client`.
#' @return The updated `vmx_data_version`.
#' @export
vmx_data_version_archive <- function(dv, reason = NULL, client = vmx_client()) {
  vmx_set_dv_archive(dv, TRUE, reason, client)
}

#' Unarchive a data version
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return The updated `vmx_data_version`.
#' @export
vmx_data_version_unarchive <- function(dv, client = vmx_client()) {
  vmx_set_dv_archive(dv, FALSE, NULL, client)
}

#' @keywords internal
#' @noRd
vmx_set_dv_archive <- function(dv, archived, reason, client) {
  body <- vmx_compact(list(archived = archived, reason = reason))
  data <- vmx_patch(client, paste0("/data-versions/", vmx_id(dv, "dv"), "/archive"), body)
  new_vmx_resource(data, "vmx_data_version", "data_version_id")
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
