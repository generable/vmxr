# Data versions — curated, model-ready data plus the modeling access layer.

#' List data versions
#' @param treatment Optional treatment filter.
#' @param study Optional study filter.
#' @param include_archived Include archived versions.
#' @param eligible_for_modeling Optional modeling-eligibility filter.
#' @param client A `vmx_client`.
#' @return A tibble containing all matching data versions.
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
  vmx_paginate(client, "/data-versions", params)
}

#' Fetch one data version
#' @param id A data-version id (`dv_...`) or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A `vmx_data_version`.
#' @export
vmx_data_version <- function(id, client = vmx_client()) {
  data_version_id <- vmx_id(id, "dv")
  data <- vmx_get(client, paste0("/data-versions/", data_version_id))
  vmx_validate_response_id(
    data, "data_version_id", data_version_id, "data version"
  )
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
  dataset_id <- vmx_id(dataset, "ds", "dataset")
  uploads <- vmx_nonempty_strings(
    uploads, "uploads", unique = TRUE
  )
  upload_ids <- vapply(
    uploads,
    vmx_id,
    character(1),
    prefix = "upl",
    arg = "uploads"
  ) |> unname()
  body <- vmx_compact(list(
    upload_ids = as.list(upload_ids),
    prior_config_data_version_id = vmx_opt_id(prior_config, "dv", "prior_config")
  ))
  data <- vmx_post(
    client,
    paste0("/datasets/", dataset_id, "/data-versions"),
    body
  )
  vmx_validate_response_id(
    data, "dataset_id", dataset_id, "data-version creation"
  )
  new_vmx_resource(data, "vmx_prep_status", "dataset_id")
}

#' A prepared data-version table
#'
#' `GET /data-versions/{id}/tables/{domain}` — returns the formatter's prepared
#' `domain` table as a tibble (columns typed per the server's column metadata,
#' which is attached as the `"columns"` attribute). `gen_subject_uuid` is the
#' canonical subject join key.
#'
#' @param dv A data-version id (`dv_...`) or `vmx_data_version`.
#' @param domain One of `"subjects"`, `"pk"`, `"dosing"`, `"pd"`, `"labs"`,
#'   `"covariates"`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_data_version_table <- function(dv, domain = c("subjects", "pk", "dosing", "pd", "labs", "covariates"),
                                   client = vmx_client()) {
  domain <- match.arg(domain)
  dv_id <- vmx_id(dv, "dv")
  tbl <- vmx_get(client, paste0("/data-versions/", dv_id, "/tables/", domain))
  vmx_validate_response_id(tbl, "data_version_id", dv_id, "data-version table")
  returned_domain <- vmx_response_scalar(
    vmx_response_field(tbl, "domain", "data-version table.domain"),
    "data-version table.domain",
    type = "character",
    nonempty = TRUE
  )
  if (!identical(returned_domain, domain)) {
    vmx_abort_response(
      "data-version table field 'domain' does not match the requested domain.",
      field = "domain"
    )
  }
  vmx_dvtable_to_tibble(tbl)
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
  data_version_id <- vmx_id(dv, "dv")
  envelope <- vmx_get(
    client, paste0("/data-versions/", data_version_id, "/export")
  )
  vmx_validate_response_id(
    envelope, "data_version_id", data_version_id, "data-version export"
  )
  url <- vmx_response_scalar(
    vmx_response_field(
      envelope, "download_url", "data-version export.download_url"
    ),
    "data-version export.download_url",
    type = "character",
    nonempty = TRUE
  )
  if (is.null(dest)) {
    return(envelope)
  }
  if (!is.character(dest) || length(dest) != 1L || is.na(dest) ||
      !nzchar(trimws(dest))) {
    vmx_abort(
      "`dest` must be one non-empty file path.",
      class = "vmx_usage_error"
    )
  }
  # Anonymous request: the signed URL carries its own credentials; sending the
  # API bearer to GCS would leak the token and is rejected anyway.
  tryCatch(
    httr2::request(url) |>
      httr2::req_perform(path = dest),
    error = function(e) {
      # Do not attach the transport condition: it may contain the signed URL
      # (and therefore its temporary credentials).
      vmx_abort(
        "Data-version export download failed.",
        class = "vmx_api_error",
        reason = "export_download_failed"
      )
    }
  )
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
  data_version_id <- vmx_id(dv, "dv")
  data <- vmx_patch(
    client, paste0("/data-versions/", data_version_id, "/archive"), body
  )
  vmx_validate_response_id(
    data, "data_version_id", data_version_id, "data-version archive update"
  )
  new_vmx_resource(data, "vmx_data_version", "data_version_id")
}

# ---- Modeling data access (nlmixr2 / Stan-Torsten) -------------------------

#' Subjects table (one row per subject)
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_subjects <- function(dv, client = vmx_client()) {
  vmx_data_version_table(dv, "subjects", client = client)
}

#' PK observations table
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_pk <- function(dv, client = vmx_client()) {
  vmx_data_version_table(dv, "pk", client = client)
}

#' Dosing events table
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_dosing <- function(dv, client = vmx_client()) {
  vmx_data_version_table(dv, "dosing", client = client)
}

#' PD observations table
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_pd <- function(dv, client = vmx_client()) {
  vmx_data_version_table(dv, "pd", client = client)
}

#' Fetch model-ready tidy tables for a data version
#'
#' Returns a `vmx_model_data` bundle with `$subjects`, `$pk`, `$dosing`, and
#' `$pd` (each a tibble, or `NULL` when the DataVersion has no such prepared
#' table), and `$meta` (units, time bases, PD-marker manifest, subject count)
#' read from the DataVersion. Only domains flagged in the DV's
#' `table_availability` are fetched, so absent optional tables don't 404.
#'
#' @param dv A data-version id or `vmx_data_version`.
#' @param client A `vmx_client`.
#' @return A `vmx_model_data` object.
#' @export
vmx_model_data <- function(dv, client = vmx_client()) {
  dv_obj <- if (inherits(dv, "vmx_data_version")) dv else vmx_data_version(vmx_id(dv, "dv"), client = client)
  avail <- vmx_table_availability(dv_obj)
  fetch <- function(domain) {
    if (isTRUE(avail[[domain]])) vmx_data_version_table(dv_obj, domain, client = client) else NULL
  }
  structure(
    list(
      subjects = fetch("subjects"),
      pk = fetch("pk"),
      dosing = fetch("dosing"),
      pd = fetch("pd"),
      meta = list(
        data_version_id = dv_obj$data_version_id,
        units = dv_obj$units,
        time_bases = dv_obj$time_bases,
        recommended_time_basis = dv_obj$recommended_time_basis,
        pd_markers = dv_obj$pd_markers,
        n_subjects = dv_obj$n_subjects,
        table_availability = avail
      )
    ),
    class = "vmx_model_data"
  )
}

vmx_table_availability <- function(dv) {
  avail <- vmx_response_field(
    dv, "table_availability", "data version.table_availability"
  )
  required <- c("subjects", "pk", "dosing", "pd", "labs", "covariates")
  if (!is.list(avail) || is.null(names(avail)) ||
      any(!nzchar(names(avail))) || anyDuplicated(names(avail)) ||
      !all(required %in% names(avail))) {
    vmx_abort_response(
      "field 'data version.table_availability' is missing required domains.",
      field = "table_availability"
    )
  }
  for (domain in names(avail)) {
    vmx_response_scalar(
      avail[[domain]],
      paste0("data version.table_availability.", domain),
      type = "logical"
    )
  }
  avail
}

#' @export
print.vmx_model_data <- function(x, ...) {
  cli::cli_text("{.cls <vmx_model_data>} {x$meta$data_version_id %||% ''}")
  dims <- function(t) if (is.null(t)) "-" else paste0(nrow(t), "x", ncol(t))
  cli::cli_bullets(c(
    "*" = "subjects: {dims(x$subjects)}",
    "*" = "pk: {dims(x$pk)}",
    "*" = "dosing: {dims(x$dosing)}",
    "*" = "pd: {dims(x$pd)}"
  ))
  invisible(x)
}

#' NONMEM-layout data.frame for nlmixr2 / rxode2
#'
#' Not yet implemented. Assembling the NONMEM/`nlmixr2` layout
#' (ID/TIME/DV/AMT/EVID/CMT/MDV/RATE/II/ADDL/SS + covariates) from the `pk` and
#' `dosing` domain tables requires the DataVersion column/manifest contract to
#' be pinned and validated against real data; see the package NEWS. Use
#' [vmx_pk()] / [vmx_data_version_table()] for the tidy tables today.
#'
#' @param dv A data-version id or `vmx_data_version`.
#' @param analyte Analyte to assemble.
#' @param client A `vmx_client`.
#' @return Not yet implemented.
#' @export
vmx_nlmixr_data <- function(dv, analyte = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_nlmixr_data()")
}

#' Ragged-array data list for Stan / Torsten
#'
#' Not yet implemented. The per-subject `start[i]`/`end[i]` index ranges and
#' `iObs` observation index must be derived and verified against real data
#' before shipping (this is the error-prone derivation the design flags); see
#' the package NEWS.
#'
#' @param dv A data-version id or `vmx_data_version`.
#' @param analyte Analyte to assemble.
#' @param client A `vmx_client`.
#' @return Not yet implemented.
#' @export
vmx_torsten_data <- function(dv, analyte = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_torsten_data()")
}
