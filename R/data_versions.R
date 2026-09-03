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
#' API 0.3 projects every analytical table on a time basis and requires the
#' `time_basis` query parameter. When `time_basis` is `NULL`, the
#' DataVersion's recommended basis is used (fetching the DataVersion first if
#' only an id was given). A v0.3 DataVersion with no recommended basis (no
#' basis has PK-eligible subjects) needs an explicit `time_basis`; only a
#' DataVersion without a `time_bases` map (an API 0.2 server) is fetched
#' without the parameter.
#'
#' @param dv A data-version id (`dv_...`) or `vmx_data_version`.
#' @param domain One of `"subjects"`, `"pk"`, `"dosing"`, `"pd"`, `"labs"`,
#'   `"covariates"`.
#' @param time_basis Optional time basis name (e.g. `"observed"`). A basis the
#'   DataVersion does not list as available is refused client-side.
#' @param client A `vmx_client`.
#' @return A tibble. The selected basis is attached as the `"time_basis"`
#'   attribute when one was requested or echoed.
#' @export
vmx_data_version_table <- function(dv, domain = c("subjects", "pk", "dosing", "pd", "labs", "covariates"),
                                   time_basis = NULL, client = vmx_client()) {
  domain <- match.arg(domain)
  dv_id <- vmx_id(dv, "dv")
  if (is.null(time_basis) && !inherits(dv, "vmx_data_version")) {
    dv <- vmx_data_version(dv_id, client = client)
  }
  basis <- vmx_resolve_time_basis(dv, time_basis)
  tbl <- vmx_get(
    client,
    paste0("/data-versions/", dv_id, "/tables/", domain),
    list(time_basis = basis)
  )
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
  echoed <- tbl[["time_basis"]]
  if (!is.null(basis)) {
    if (is.null(echoed) && vmx_dv_has_time_bases(dv)) {
      vmx_abort_response(
        "data-version table did not echo the requested 'time_basis'.",
        field = "time_basis"
      )
    }
    if (!is.null(echoed) && !identical(echoed, basis)) {
      vmx_abort_response(
        "data-version table field 'time_basis' does not match the requested basis.",
        field = "time_basis"
      )
    }
  }
  out <- vmx_dvtable_to_tibble(tbl)
  attr(out, "time_basis") <- echoed %||% basis
  out
}

# TRUE when the DataVersion carries the v0.3 per-basis map (a named, non-empty
# `time_bases` object) — the marker that time-basis projection is in force.
vmx_dv_has_time_bases <- function(dv) {
  bases <- if (inherits(dv, "vmx_resource")) dv$time_bases else NULL
  is.list(bases) && length(bases) > 0L && !is.null(names(bases))
}

#' Resolve the time basis to request for a DataVersion's tables
#'
#' An explicit `time_basis` wins and is checked against the DataVersion's
#' `time_bases` map when one is present (an unlisted or unavailable basis is a
#' usage error). Otherwise the recommended basis is used — API 0.3 serves it
#' as `{value, reason}`, older servers as a bare string — and `NULL` means the
#' DataVersion advertises none.
#' @keywords internal
#' @noRd
vmx_resolve_time_basis <- function(dv, time_basis = NULL) {
  bases <- if (inherits(dv, "vmx_resource")) dv$time_bases else NULL
  if (!is.null(time_basis)) {
    time_basis <- vmx_nonempty_strings(time_basis, "time_basis", exactly_one = TRUE)
    if (is.list(bases) && length(bases) && !is.null(names(bases))) {
      entry <- bases[[time_basis]]
      if (is.null(entry) || !isTRUE(entry$available)) {
        available <- names(Filter(function(b) is.list(b) && isTRUE(b$available), bases))
        vmx_abort(
          sprintf(
            "Time basis '%s' is not available on this DataVersion (available: %s).",
            time_basis,
            if (length(available)) paste(available, collapse = ", ") else "none"
          ),
          class = "vmx_usage_error"
        )
      }
    }
    return(time_basis)
  }
  if (!inherits(dv, "vmx_resource")) return(NULL)
  rec <- dv$recommended_time_basis
  if (is.list(rec)) rec <- rec$value
  if (is.character(rec) && length(rec) == 1L && !is.na(rec) && nzchar(rec)) return(rec)
  if (vmx_dv_has_time_bases(dv)) {
    available <- names(Filter(function(b) is.list(b) && isTRUE(b$available), bases))
    vmx_abort(
      sprintf(
        "This DataVersion recommends no time basis (no basis has PK-eligible subjects); pass `time_basis` explicitly to review it (available: %s).",
        if (length(available)) paste(available, collapse = ", ") else "none"
      ),
      class = "vmx_usage_error"
    )
  }
  NULL
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
#' @param time_basis Optional time basis; see [vmx_data_version_table()].
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_subjects <- function(dv, time_basis = NULL, client = vmx_client()) {
  vmx_data_version_table(dv, "subjects", time_basis = time_basis, client = client)
}

#' PK observations table
#' @param dv A data-version id or `vmx_data_version`.
#' @param time_basis Optional time basis; see [vmx_data_version_table()].
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_pk <- function(dv, time_basis = NULL, client = vmx_client()) {
  vmx_data_version_table(dv, "pk", time_basis = time_basis, client = client)
}

#' Dosing events table
#' @param dv A data-version id or `vmx_data_version`.
#' @param time_basis Optional time basis; see [vmx_data_version_table()].
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_dosing <- function(dv, time_basis = NULL, client = vmx_client()) {
  vmx_data_version_table(dv, "dosing", time_basis = time_basis, client = client)
}

#' PD observations table
#' @param dv A data-version id or `vmx_data_version`.
#' @param time_basis Optional time basis; see [vmx_data_version_table()].
#' @param client A `vmx_client`.
#' @return A tibble.
#' @export
vmx_pd <- function(dv, time_basis = NULL, client = vmx_client()) {
  vmx_data_version_table(dv, "pd", time_basis = time_basis, client = client)
}

#' Fetch model-ready tidy tables for a data version
#'
#' Returns a `vmx_model_data` bundle with `$subjects`, `$pk`, `$dosing`, `$pd`,
#' `$labs` and `$covariates` (each a tibble, or `NULL` when the DataVersion has
#' no such prepared table), and `$meta` (units, time bases, the selected
#' `time_basis`, PD-marker manifest, subject count) read from the DataVersion.
#' Only domains flagged in the DV's `table_availability` are fetched, so absent
#' optional tables don't 404. Every table is projected on the same time basis.
#'
#' @param dv A data-version id or `vmx_data_version`.
#' @param time_basis Optional time basis; defaults to the DataVersion's
#'   recommended basis. See [vmx_data_version_table()].
#' @param client A `vmx_client`.
#' @return A `vmx_model_data` object.
#' @export
vmx_model_data <- function(dv, time_basis = NULL, client = vmx_client()) {
  dv_obj <- if (inherits(dv, "vmx_data_version")) dv else vmx_data_version(vmx_id(dv, "dv"), client = client)
  avail <- vmx_table_availability(dv_obj)
  basis <- vmx_resolve_time_basis(dv_obj, time_basis)
  fetch <- function(domain) {
    if (isTRUE(avail[[domain]])) {
      vmx_data_version_table(dv_obj, domain, time_basis = basis, client = client)
    } else {
      NULL
    }
  }
  structure(
    list(
      subjects = fetch("subjects"),
      pk = fetch("pk"),
      dosing = fetch("dosing"),
      pd = fetch("pd"),
      labs = fetch("labs"),
      covariates = fetch("covariates"),
      meta = list(
        data_version_id = dv_obj$data_version_id,
        units = dv_obj$units,
        time_bases = dv_obj$time_bases,
        recommended_time_basis = dv_obj$recommended_time_basis,
        time_basis = basis,
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
  cli::cli_text("{.cls <vmx_model_data>} {x$meta$data_version_id %||% ''} (time basis: {x$meta$time_basis %||% 'unspecified'})")
  dims <- function(t) if (is.null(t)) "-" else paste0(nrow(t), "x", ncol(t))
  cli::cli_bullets(c(
    "*" = "subjects: {dims(x$subjects)}",
    "*" = "pk: {dims(x$pk)}",
    "*" = "dosing: {dims(x$dosing)}",
    "*" = "pd: {dims(x$pd)}",
    "*" = "labs: {dims(x$labs)}",
    "*" = "covariates: {dims(x$covariates)}"
  ))
  invisible(x)
}

#' Ragged-array data list for Stan / Torsten
#'
#' Not yet implemented. The per-subject `start[i]`/`end[i]` index ranges and
#' `iObs` observation index must be derived and verified against real data
#' before shipping (this is the error-prone derivation the design flags); see
#' the package NEWS. When implemented it should reuse the event assembly in
#' [vmx_nlmixr_data()] (eligibility admission, basis selection, dose and
#' censoring encoding) and only add the ragged indexing on top.
#'
#' @param dv A data-version id or `vmx_data_version`.
#' @param analyte Analyte to assemble.
#' @param client A `vmx_client`.
#' @return Not yet implemented.
#' @export
vmx_torsten_data <- function(dv, analyte = NULL, client = vmx_client()) {
  vmx_abort_unimplemented("vmx_torsten_data()")
}
