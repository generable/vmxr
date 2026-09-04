# nlmixr2 / rxode2 event-dataset assembly from a DataVersion's prepared tables.
#
# The DataVersion contract (vmx-contracts `velometrix-alpha-dataversion-schema.md`
# section 10, `velometrix-alpha-eligibility-spec.md`) is the source of truth for the
# column semantics used here:
#   * every analytical row carries `eligible_for_modeling_before_qc` and
#     `eligible_for_modeling_after_qc`; the after-QC flag is the mandatory
#     admission filter for analyses, and retained-but-ineligible rows (zero-dose
#     placebo administrations, unconvertible units, QC exclusions, ...) are
#     removed by that filter, never by this function's own heuristics;
#   * `time_hours` aliases the selected time basis' hours column;
#   * `dose_mass` is the canonical administered mass in mg; `dose_rate` (mg/h) is
#     the server-owned canonical infusion rate; `route` is a closed vocabulary;
#   * PK `value_float` carries the canonical concentration, `censoring` is
#     `"BLQ"` / `"ALQ"` / null with the row's `lloq` / `uloq`.
#
# Under the default admission rule every one of those fields is guaranteed on an
# admitted row, so a gap is a served-contract violation (`vmx_response_error`).
# Under `before_qc` / `all` the same gap is expected on retained-ineligible rows,
# so such rows are dropped and counted instead.

#' Default route-to-compartment mapping for [vmx_nlmixr_data()]
#'
#' Extravascular routes (`po`, `sc`, `im`) dose into compartment 1 (depot);
#' intravenous routes (`iv-bolus`, `iv-infusion`) dose into compartment 2
#' (central); PK observations are read from compartment 2. Override by passing
#' a named integer vector with the same names to `cmt`.
#' @export
vmx_nlmixr_default_cmt <- function() {
  c(po = 1L, sc = 1L, im = 1L, `iv-bolus` = 2L, `iv-infusion` = 2L, observation = 2L)
}

#' NONMEM-layout event dataset for nlmixr2 / rxode2
#'
#' Assembles one event table from a DataVersion's prepared `pk`, `dosing`,
#' `subjects`, `covariates` and (optionally) `pd` tables, in the layout nlmixr2
#' and rxode2 consume directly: one row per dose event and per observation,
#' ordered by subject and time.
#'
#' Rows are ordered by subject and time; ties on `TIME` are ordered dose
#' before observation, which is how rxode2 and nlmixr2 resolve them, so a
#' pre-dose sample that shares its time with an IV bolus into the observation
#' compartment must carry its own earlier time in the source data.
#'
#' Columns: `ID` (dense integer per subject with admitted rows), `TIME` (hours on the selected
#' time basis), `DV`, `AMT` (mg), `EVID` (0 observation / 1 dose), `MDV`, `CMT`,
#' `RATE` (mg/h, > 0 for infusions), `II`, `ADDL`, `SS` (always 0 — the
#' DataVersion already expands doses per administration), `CENS` / `LIMIT`
#' (censoring: `CENS = 1` with `DV = lloq` and `LIMIT = 0` for BLQ rows,
#' `CENS = -1` with `DV = uloq` and `LIMIT = Inf` for ALQ rows), `DVID`
#' (only when PD markers are included: 1 = PK, 2.. = PD markers, and dose rows
#' carry 1), `subject_id`, and one column per subject-level covariate.
#'
#' Only rows the DataVersion marks as eligible for modeling are kept; the
#' after-QC flag is the contract's mandatory admission filter, and the result
#' is analysis-ready only under that default. `before_qc` and `all` are review
#' modes: they warn, mark the result `analysis_ready = FALSE`, and drop (with a
#' count) any retained-ineligible row that cannot be encoded — a null time, a
#' dose without canonical mass, a route outside the compartment map, an infusion
#' without a canonical rate. Every dropped-row count is on the `"vmx"` attribute
#' so nothing disappears silently.
#'
#' @param dv A data-version id (`dv_...`) or `vmx_data_version`.
#' @param analyte PK analyte to assemble, matched against `biomarker_name` or
#'   `biomarker_code`. Required when the PK table carries more than one analyte.
#' @param time_basis Time basis to project on (e.g. `"observed"`). Defaults to
#'   the DataVersion's recommended basis; a basis the DataVersion does not list
#'   as available is refused, and a DataVersion with no recommended basis needs
#'   an explicit one.
#' @param eligibility Which modeling-eligibility flag admits a row:
#'   `"after_qc"` (default, the contract's admission filter), `"before_qc"`, or
#'   `"all"` (no filter). The latter two are for review only.
#' @param pd_markers PD markers to include as additional observation endpoints:
#'   `NULL`/`FALSE` (default, PK only), `TRUE` (every marker the DataVersion
#'   marks eligible on the selected basis), or a character vector of marker
#'   names (an explicitly named marker that is ineligible is a usage error).
#' @param cmt Named integer vector mapping dosing routes and `"observation"` to
#'   compartments; see [vmx_nlmixr_default_cmt()]. PD markers get compartments
#'   after the highest mapped value, in the order they are included.
#' @param client A `vmx_client`.
#' @return A tibble in NONMEM layout. The `"vmx"` attribute is a list with
#'   `data_version_id`, `time_basis`, `units`, `analyte`, `eligibility`,
#'   `analysis_ready`, `cmt`, `endpoints` (a tibble of `dvid`, `name`, `cmt`,
#'   `unit`), `dropped` (named integer counts), and `n_subjects`.
#' @seealso [vmx_model_data()] for the underlying tidy tables.
#' @export
vmx_nlmixr_data <- function(dv, analyte = NULL, time_basis = NULL,
                            eligibility = c("after_qc", "before_qc", "all"),
                            pd_markers = NULL, cmt = NULL,
                            client = vmx_client()) {
  eligibility <- match.arg(eligibility)
  strict <- identical(eligibility, "after_qc")
  cmt <- vmx_nlmixr_cmt_map(cmt)
  if (!is.null(analyte)) {
    analyte <- vmx_nonempty_strings(analyte, "analyte", exactly_one = TRUE)
  }
  pd_selection <- vmx_nlmixr_pd_selection(pd_markers)
  if (!strict) {
    cli::cli_warn(c(
      "!" = "`eligibility = \"{eligibility}\"` admits rows the DataVersion does not mark eligible for modeling.",
      "i" = "The result is for review only (`analysis_ready = FALSE`); fit models on the default `after_qc` admission."
    ))
  }

  md <- vmx_model_data(dv, time_basis = time_basis, client = client)
  basis <- md$meta$time_basis
  if (is.null(md$pk) || is.null(md$dosing)) {
    vmx_abort(
      "The DataVersion has no prepared `pk` and `dosing` tables; an nlmixr2 dataset needs both.",
      class = "vmx_usage_error"
    )
  }
  if (is.null(md$subjects)) {
    vmx_abort(
      "The DataVersion has no prepared `subjects` table; it is the contract's subject key for every analytical row.",
      class = "vmx_usage_error"
    )
  }
  dropped <- list()

  # ---- subjects (the contract's only subject key) --------------------------
  subjects <- vmx_nlmixr_subject_map(md$subjects)

  # ---- PK observations ----------------------------------------------------
  pk <- vmx_nlmixr_admit(md$pk, eligibility, "pk")
  dropped$pk_ineligible <- pk$n_dropped
  pk <- pk$rows
  pk_analyte <- vmx_nlmixr_select_analyte(pk, analyte, all_rows = md$pk)
  dropped$pk_other_analytes <- nrow(pk) - nrow(pk_analyte$rows)
  pk <- pk_analyte$rows
  pk_obs <- vmx_nlmixr_observations(
    pk,
    basis = basis,
    dvid = 1L,
    cmt = unname(cmt[["observation"]]),
    domain = "pk",
    strict = strict
  )
  dropped$pk_unencodable <- pk_obs$n_dropped

  # ---- doses --------------------------------------------------------------
  dosing <- vmx_nlmixr_admit(md$dosing, eligibility, "dosing")
  dropped$dosing_ineligible <- dosing$n_dropped
  doses <- vmx_nlmixr_doses(dosing$rows, basis = basis, cmt = cmt, strict = strict)
  dropped$dosing_unencodable <- doses$n_dropped

  # ---- PD endpoints (optional) --------------------------------------------
  endpoints <- tibble::tibble(
    dvid = 1L,
    name = pk_analyte$name,
    cmt = unname(cmt[["observation"]]),
    unit = vmx_nlmixr_unit(pk, md$meta$units$pk_concentration)
  )
  pd_parts <- list()
  if (!isFALSE(pd_selection)) {
    if (is.null(md$pd)) {
      vmx_abort(
        "`pd_markers` was requested but the DataVersion has no prepared `pd` table.",
        class = "vmx_usage_error"
      )
    }
    pd <- vmx_nlmixr_admit(md$pd, eligibility, "pd")
    dropped$pd_ineligible <- pd$n_dropped
    pd <- pd$rows
    marker_sel <- vmx_nlmixr_pd_markers(pd, pd_selection, md$meta, basis, eligibility, all_rows = md$pd)
    dropped$pd_markers_ineligible <- marker_sel$n_dropped
    keys <- vmx_nlmixr_marker_key(pd)
    next_cmt <- max(cmt)
    unencodable <- 0L
    for (k in seq_along(marker_sel$markers)) {
      marker <- marker_sel$markers[[k]]
      rows_k <- pd[which(keys == marker), , drop = FALSE]
      next_cmt <- next_cmt + 1L
      obs_k <- vmx_nlmixr_observations(
        rows_k,
        basis = basis,
        dvid = 1L + k,
        cmt = next_cmt,
        domain = "pd",
        strict = strict
      )
      unencodable <- unencodable + obs_k$n_dropped
      pd_parts[[k]] <- obs_k$rows
      endpoints <- rbind(endpoints, tibble::tibble(
        dvid = 1L + k,
        name = marker,
        cmt = next_cmt,
        unit = vmx_nlmixr_unit(rows_k, NA_character_)
      ))
    }
    dropped$pd_unencodable <- unencodable
  }

  # ---- assemble -----------------------------------------------------------
  events <- do.call(rbind, c(list(pk_obs$rows, doses$rows), pd_parts))
  if (nrow(events) == 0L || !any(events$EVID == 0L) || !any(events$EVID == 1L)) {
    pk_flag <- vmx_nlmixr_basis_pk_flag(md$meta, basis, eligibility)
    vmx_abort(
      c(
        sprintf(
          "No %s remain for this DataVersion, analyte, and time basis under `eligibility = \"%s\"`.",
          if (!any(events$EVID == 0L)) "eligible PK observations" else "eligible dose events", eligibility
        ),
        if (isFALSE(pk_flag)) "i" = sprintf("The DataVersion marks PK on basis '%s' as not eligible for modeling; the table is readable for review with `eligibility = \"all\"`.", basis)
      ),
      class = "vmx_usage_error"
    )
  }
  ids <- subjects$id[match(events$gen_subject_uuid, subjects$gen_subject_uuid)]
  if (anyNA(ids)) {
    vmx_abort_response(
      "analytical rows reference a subject absent from the `subjects` table.",
      field = "gen_subject_uuid"
    )
  }
  # Dense 1..n over the subjects that actually have rows, in subjects-table order.
  events$ID <- match(ids, sort(unique(ids)))
  events$subject_id <- subjects$subject_id[match(events$gen_subject_uuid, subjects$gen_subject_uuid)]
  # Ties on TIME are ordered dose-first, matching how rxode2/nlmixr2 resolve
  # them; a pre-dose sample coincident with an IV bolus into the observation
  # compartment therefore needs its own (earlier) time in the source data.
  events <- events[order(events$ID, events$TIME, -events$EVID, events$CMT, events$DVID), , drop = FALSE]

  covariates <- vmx_nlmixr_covariates(md$covariates, eligibility)
  if (!is.null(covariates)) {
    clash <- intersect(setdiff(names(covariates), "gen_subject_uuid"), names(events))
    if (length(clash)) {
      vmx_abort(
        sprintf(
          "%d covariate name(s) collide with reserved event columns (%s).",
          length(clash), paste(clash, collapse = ", ")
        ),
        class = "vmx_usage_error"
      )
    }
    idx <- match(events$gen_subject_uuid, covariates$gen_subject_uuid)
    for (nm in setdiff(names(covariates), "gen_subject_uuid")) {
      events[[nm]] <- covariates[[nm]][idx]
    }
  }
  for (nm in setdiff(names(subjects), c("gen_subject_uuid", "id", "subject_id"))) {
    if (!nm %in% names(events)) {
      events[[nm]] <- subjects[[nm]][match(events$gen_subject_uuid, subjects$gen_subject_uuid)]
    }
  }

  core <- c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "CMT", "RATE", "II", "ADDL",
            "SS", "CENS", "LIMIT", "DVID", "subject_id")
  if (nrow(endpoints) == 1L) {
    # Single endpoint: rxode2 treats a DVID column as an endpoint index and warns
    # when it is not 1..n, so only emit it for multi-endpoint (PD) datasets.
    events$DVID <- NULL
    core <- setdiff(core, "DVID")
  }
  out <- tibble::as_tibble(events[, c(core, setdiff(names(events), c(core, "gen_subject_uuid"))), drop = FALSE])
  attr(out, "vmx") <- list(
    data_version_id = md$meta$data_version_id,
    time_basis = basis,
    units = list(
      time = md$meta$units$time %||% "h",
      pk_concentration = endpoints$unit[[1]],
      dose = md$meta$units$dose %||% "mg"
    ),
    analyte = pk_analyte$name,
    eligibility = eligibility,
    eligibility_flag = pk$flag %||% dosing$flag,
    analysis_ready = strict,
    cmt = cmt,
    endpoints = endpoints,
    dropped = vapply(dropped, function(x) as.integer(x), integer(1)),
    n_subjects = length(unique(out$ID))
  )
  out
}

# ---- internals ---------------------------------------------------------------

vmx_nlmixr_cmt_map <- function(cmt) {
  default <- vmx_nlmixr_default_cmt()
  if (is.null(cmt)) return(default)
  if (!is.numeric(cmt) || is.null(names(cmt)) || anyNA(cmt) ||
      any(cmt != as.integer(cmt)) || any(cmt < 1) ||
      !all(names(cmt) %in% names(default))) {
    vmx_abort(
      sprintf(
        "`cmt` must be a named vector of positive integers over: %s.",
        paste(names(default), collapse = ", ")
      ),
      class = "vmx_usage_error"
    )
  }
  default[names(cmt)] <- as.integer(cmt)
  default
}

vmx_nlmixr_pd_selection <- function(pd_markers) {
  if (is.null(pd_markers) || isFALSE(pd_markers)) return(FALSE)
  if (isTRUE(pd_markers)) return(TRUE)
  vmx_nonempty_strings(pd_markers, "pd_markers", unique = TRUE)
}

vmx_nlmixr_flag_column <- function(eligibility) {
  switch(
    eligibility,
    after_qc = "eligible_for_modeling_after_qc",
    before_qc = "eligible_for_modeling_before_qc",
    all = NULL
  )
}

# Keep the rows admitted by the selected eligibility flag; count the rest.
# A table whose basis the server echoed (API 0.3 / the 0.2.x hotfix line) MUST
# carry a flag; a flat canonical table (API 0.2.2 without the hotfix) has none,
# and only `eligibility = "all"` applies there.
vmx_nlmixr_admit <- function(tbl, eligibility, domain) {
  flag <- vmx_nlmixr_flag_column(eligibility)
  if (is.null(flag)) return(list(rows = tbl, n_dropped = 0L, flag = NULL))
  if (!flag %in% names(tbl) && "eligible_for_modeling" %in% names(tbl)) {
    # Pre-QC-contract DataVersions (the API 0.2.x per-basis exports) carry a single
    # `eligible_for_modeling` flag; it is the admission filter of that line for both
    # the before- and after-QC modes.
    flag <- "eligible_for_modeling"
  }
  if (!flag %in% names(tbl)) {
    if (isTRUE(attr(tbl, "basis_echoed"))) {
      vmx_abort_response(
        sprintf("the `%s` table is projected on a time basis but has no `%s` column.", domain, flag),
        field = flag
      )
    }
    vmx_abort(
      sprintf(
        "The `%s` table has no `%s` column; this DataVersion predates the v0.3 eligibility flags. Use `eligibility = \"all\"` to assemble it unfiltered (review only).",
        domain, flag
      ),
      class = "vmx_usage_error"
    )
  }
  keep <- tbl[[flag]] %in% TRUE
  list(rows = tbl[keep, , drop = FALSE], n_dropped = sum(!keep), flag = flag)
}

# The selected basis' time column: `time_hours` aliases it on API 0.3; older
# tables fall back to the basis-named column (dosing on
# `nominal_from_observed_dose` projects `observed_time_hours`, schema §11.2).
vmx_nlmixr_time_column <- function(tbl, basis, domain) {
  candidates <- "time_hours"
  if (!is.null(basis)) {
    basis_col <- if (identical(domain, "dosing") && identical(basis, "nominal_from_observed_dose")) {
      "observed_time_hours"
    } else {
      paste0(basis, "_time_hours")
    }
    candidates <- c(candidates, basis_col)
  }
  col <- candidates[candidates %in% names(tbl)][1]
  if (is.na(col)) {
    vmx_abort_response(
      sprintf("the `%s` table carries no time column for the selected basis.", domain),
      field = "time_hours"
    )
  }
  as.numeric(tbl[[col]])
}

vmx_nlmixr_marker_key <- function(tbl) {
  if ("biomarker_name" %in% names(tbl)) return(as.character(tbl$biomarker_name))
  if ("biomarker_code" %in% names(tbl)) return(as.character(tbl$biomarker_code))
  rep("value", nrow(tbl))
}

# Pick one PK analyte among the admitted rows. `analyte` matches
# `biomarker_name` first; `biomarker_code` is consulted only when no name
# matches, and a match may never span two names. `all_rows` (the unfiltered
# table) tells "no eligible rows" apart from "no such analyte".
vmx_nlmixr_select_analyte <- function(pk, analyte, all_rows = pk) {
  key <- vmx_nlmixr_marker_key(pk)
  code <- if ("biomarker_code" %in% names(pk)) as.character(pk$biomarker_code) else key
  present <- unique(key[!is.na(key)])
  if (is.null(analyte)) {
    if (length(present) > 1L) {
      vmx_abort(
        sprintf(
          "The PK table carries %d analytes (%s); pass `analyte` to choose one.",
          length(present), paste(present, collapse = ", ")
        ),
        class = "vmx_usage_error"
      )
    }
    return(list(rows = pk, name = if (length(present)) present[[1]] else "pk"))
  }
  hit <- key %in% analyte
  if (!any(hit)) hit <- code %in% analyte
  if (!any(hit)) {
    all_key <- vmx_nlmixr_marker_key(all_rows)
    all_code <- if ("biomarker_code" %in% names(all_rows)) as.character(all_rows$biomarker_code) else all_key
    known <- any(all_key %in% analyte | all_code %in% analyte)
    vmx_abort(
      if (known) {
        sprintf("`analyte` '%s' has no rows admitted by the selected eligibility rule.", analyte)
      } else {
        sprintf("`analyte` '%s' is not in the PK table (available: %s).", analyte, paste(present, collapse = ", "))
      },
      class = "vmx_usage_error"
    )
  }
  names_hit <- unique(key[hit])
  if (length(names_hit) != 1L) {
    vmx_abort(
      sprintf(
        "`analyte` '%s' matches %d analytes by code (%s); name one of them by `biomarker_name`.",
        analyte, length(names_hit), paste(names_hit, collapse = ", ")
      ),
      class = "vmx_usage_error"
    )
  }
  list(rows = pk[hit, , drop = FALSE], name = names_hit[[1]])
}

# The DataVersion's own verdict on PK modeling eligibility for a basis, or NULL
# when the metadata does not carry it.
vmx_nlmixr_basis_pk_flag <- function(meta, basis, eligibility) {
  flag <- vmx_nlmixr_flag_column(eligibility)
  bases <- meta$time_bases
  if (is.null(flag) || is.null(basis) || !is.list(bases) || !is.list(bases[[basis]])) return(NULL)
  value <- bases[[basis]]$pk_modeling_eligibility[[flag]]
  if (is.logical(value) && length(value) == 1L) value else NULL
}

# Markers to include: those requested that exist in the pd table AND that the
# DataVersion marks eligible on the selected basis (eligibility-spec §9:
# "Selected PD markers additionally require post-QC modeling eligibility").
# An explicitly named ineligible marker is a usage error; under `TRUE` it is
# dropped and counted.
vmx_nlmixr_pd_markers <- function(pd, selection, meta, basis, eligibility, all_rows = pd) {
  key <- vmx_nlmixr_marker_key(pd)
  present <- unique(key[!is.na(key)])
  wanted <- if (isTRUE(selection)) present else selection
  missing <- setdiff(wanted, present)
  if (length(missing)) {
    served <- unique(stats::na.omit(vmx_nlmixr_marker_key(all_rows)))
    filtered_out <- intersect(missing, served)
    vmx_abort(
      if (length(filtered_out)) {
        sprintf(
          "%d requested PD marker(s) have no rows admitted by the selected eligibility rule (%s).",
          length(filtered_out), paste(filtered_out, collapse = ", ")
        )
      } else {
        sprintf(
          "%d requested PD marker(s) are not in the `pd` table (available: %s).",
          length(missing), paste(present, collapse = ", ")
        )
      },
      class = "vmx_usage_error"
    )
  }
  flag <- vmx_nlmixr_flag_column(eligibility)
  ineligible <- character()
  if (!is.null(flag)) {
    marker_flags <- vmx_nlmixr_marker_flags(meta, basis, flag)
    ineligible <- wanted[!is.na(marker_flags[wanted]) & !marker_flags[wanted]]
  }
  if (length(ineligible) && !isTRUE(selection)) {
    vmx_abort(
      sprintf(
        "%d requested PD marker(s) are not eligible for modeling on basis '%s' (%s).",
        length(ineligible), basis %||% "?", paste(ineligible, collapse = ", ")
      ),
      class = "vmx_usage_error"
    )
  }
  list(markers = setdiff(wanted, ineligible), n_dropped = length(ineligible))
}

# Named logical vector marker-name -> eligibility flag, from the DataVersion's
# per-basis `pd_marker_eligibility` (keyed by marker `gen_uuid`) joined to the
# `pd_markers` manifest. Empty when the DataVersion does not carry them.
vmx_nlmixr_marker_flags <- function(meta, basis, flag) {
  out <- logical()
  manifest <- meta$pd_markers
  bases <- meta$time_bases
  if (!is.list(manifest) || !is.list(bases) || is.null(basis) || !is.list(bases[[basis]])) return(out)
  elig <- bases[[basis]]$pd_marker_eligibility
  if (!is.list(elig) || !length(elig)) return(out)
  uuid_to_name <- stats::setNames(
    vapply(manifest, function(m) as.character(m$name %||% NA_character_), character(1)),
    vapply(manifest, function(m) as.character(m$gen_uuid %||% NA_character_), character(1))
  )
  for (entry in elig) {
    nm <- uuid_to_name[as.character(entry$gen_uuid %||% NA_character_)]
    if (is.na(nm) || is.null(entry[[flag]])) next
    out[[nm]] <- isTRUE(entry[[flag]])
  }
  out
}

# Row-wise numeric value from the typed columns only (`value_float`, then
# `value_int`); the untyped source `value` is not a contract column.
vmx_nlmixr_value <- function(tbl) {
  out <- rep(NA_real_, nrow(tbl))
  for (col in c("value_float", "value_int")) {
    if (col %in% names(tbl)) {
      vals <- suppressWarnings(as.numeric(tbl[[col]]))
      fill <- is.na(out) & !is.na(vals)
      out[fill] <- vals[fill]
    }
  }
  out
}

vmx_nlmixr_unit <- function(tbl, fallback) {
  for (col in c("unit", "value_unit")) {
    if (col %in% names(tbl)) {
      u <- unique(stats::na.omit(as.character(tbl[[col]])))
      if (length(u) == 1L) return(u)
      if (length(u) > 1L) return(NA_character_)
    }
  }
  if (is.null(fallback)) NA_character_ else fallback
}

vmx_nlmixr_optional <- function(tbl, col) {
  if (col %in% names(tbl)) tbl[[col]] else rep(NA, nrow(tbl))
}

# Enforce a per-row encodability condition: under strict admission a violation
# is a served-contract violation; otherwise the offending rows are dropped.
vmx_nlmixr_require <- function(ok, strict, message, field) {
  if (all(ok)) return(ok)
  if (strict) vmx_abort_response(message, field = field)
  ok
}

# Observation rows (PK or one PD marker) in NONMEM layout.
vmx_nlmixr_observations <- function(tbl, basis, dvid, cmt, domain, strict) {
  n <- nrow(tbl)
  time <- vmx_nlmixr_time_column(tbl, basis, domain)
  dv <- vmx_nlmixr_value(tbl)
  cens <- rep(0L, n)
  limit <- rep(NA_real_, n)
  censoring <- toupper(as.character(vmx_nlmixr_optional(tbl, "censoring")))
  lloq <- as.numeric(vmx_nlmixr_optional(tbl, "lloq"))
  uloq <- as.numeric(vmx_nlmixr_optional(tbl, "uloq"))
  blq <- censoring %in% "BLQ"
  alq <- censoring %in% "ALQ"
  ok <- vmx_nlmixr_require(
    !is.na(time), strict,
    sprintf("an admitted `%s` observation has no time on the selected basis.", domain), "time_hours"
  )
  ok <- ok & vmx_nlmixr_require(
    !(blq & is.na(lloq)), strict,
    sprintf("a BLQ row in the `%s` table has no `lloq`; the censoring interval cannot be encoded.", domain), "lloq"
  )
  ok <- ok & vmx_nlmixr_require(
    !(alq & is.na(uloq)), strict,
    sprintf("an ALQ row in the `%s` table has no `uloq`; the censoring interval cannot be encoded.", domain), "uloq"
  )
  ok <- ok & vmx_nlmixr_require(
    !(is.na(dv) & !blq & !alq), strict,
    sprintf("an admitted `%s` observation has no numeric value and no censoring mark.", domain), "value_float"
  )
  dv[blq] <- lloq[blq]
  cens[blq] <- 1L
  limit[blq] <- 0
  dv[alq] <- uloq[alq]
  cens[alq] <- -1L
  limit[alq] <- Inf
  rows <- vmx_nlmixr_event_frame(
    n,
    gen_subject_uuid = as.character(tbl$gen_subject_uuid),
    TIME = time,
    DV = dv,
    AMT = 0,
    EVID = 0L,
    MDV = 0L,
    CMT = as.integer(cmt),
    RATE = 0,
    II = 0,
    ADDL = 0L,
    SS = 0L,
    CENS = cens,
    LIMIT = limit,
    DVID = as.integer(dvid)
  )
  list(rows = rows[ok, , drop = FALSE], n_dropped = sum(!ok))
}

# data.frame constructor that tolerates n == 0 (base::data.frame refuses to
# recycle scalar columns against a zero-length vector).
vmx_nlmixr_event_frame <- function(n, ...) {
  cols <- lapply(list(...), function(v) if (length(v) == n) v else rep_len(v, n))
  as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE)
}

# Dose rows in NONMEM layout: AMT from canonical dose_mass, RATE from the
# server-owned canonical dose_rate (never re-derived from duration).
vmx_nlmixr_doses <- function(dosing, basis, cmt, strict) {
  n <- nrow(dosing)
  if (!"dose_mass" %in% names(dosing)) {
    vmx_abort_response("the `dosing` table has no `dose_mass` column.", field = "dose_mass")
  }
  time <- vmx_nlmixr_time_column(dosing, basis, "dosing")
  amt <- as.numeric(dosing$dose_mass)
  route <- as.character(vmx_nlmixr_optional(dosing, "route"))
  known <- setdiff(names(cmt), "observation")
  route_ok <- !is.na(route) & route %in% known
  infusion <- route_ok & route == "iv-infusion"
  dose_rate <- as.numeric(vmx_nlmixr_optional(dosing, "dose_rate"))
  ok <- vmx_nlmixr_require(
    !is.na(time), strict,
    "an admitted dose has no time on the selected basis.", "time_hours"
  )
  ok <- ok & vmx_nlmixr_require(
    !is.na(amt), strict,
    "an admitted dose has no canonical `dose_mass`; the contract requires it on eligible rows.", "dose_mass"
  )
  ok <- ok & vmx_nlmixr_require(
    route_ok, strict,
    sprintf(
      "%d admitted dose row(s) carry a null `route` or one outside the compartment map (accepted: %s).",
      sum(!route_ok), paste(known, collapse = ", ")
    ),
    "route"
  )
  ok <- ok & vmx_nlmixr_require(
    !(infusion & (is.na(dose_rate) | dose_rate <= 0)), strict,
    "an admitted `iv-infusion` dose has no positive canonical `dose_rate`; the contract requires it on eligible infusion rows.",
    "dose_rate"
  )
  rate <- rep(0, n)
  rate[infusion] <- dose_rate[infusion]
  cmt_idx <- rep(NA_integer_, n)
  cmt_idx[route_ok] <- as.integer(unname(cmt[route[route_ok]]))
  rows <- vmx_nlmixr_event_frame(
    n,
    gen_subject_uuid = as.character(dosing$gen_subject_uuid),
    TIME = time,
    DV = NA_real_,
    AMT = amt,
    EVID = 1L,
    MDV = 1L,
    CMT = cmt_idx,
    RATE = rate,
    II = 0,
    ADDL = 0L,
    SS = 0L,
    CENS = 0L,
    LIMIT = NA_real_,
    # rxode2 renumbers DVID to 1..n over every row, so dose rows take the PK
    # endpoint's id rather than a sentinel that would shift the numbering.
    DVID = 1L
  )
  list(rows = rows[ok, , drop = FALSE], n_dropped = sum(!ok))
}

# Dense integer IDs from the subjects table (the contract's subject key),
# plus the human subject_id and the arm / dose_group descriptors when served.
vmx_nlmixr_subject_map <- function(subjects) {
  if (!"gen_subject_uuid" %in% names(subjects)) {
    vmx_abort_response("the `subjects` table has no `gen_subject_uuid` column.", field = "gen_subject_uuid")
  }
  uuids <- as.character(subjects$gen_subject_uuid)
  if (anyDuplicated(uuids)) {
    vmx_abort_response("the `subjects` table repeats a `gen_subject_uuid`.", field = "gen_subject_uuid")
  }
  ids <- if ("subject_id" %in% names(subjects)) as.character(subjects$subject_id) else uuids
  ids[is.na(ids)] <- uuids[is.na(ids)]
  ord <- order(suppressWarnings(as.numeric(ids)), ids, na.last = TRUE)
  map <- data.frame(
    gen_subject_uuid = uuids[ord],
    subject_id = ids[ord],
    id = seq_along(ord),
    stringsAsFactors = FALSE
  )
  for (nm in intersect(c("arm", "dose_group"), names(subjects))) {
    map[[nm]] <- as.character(subjects[[nm]])[match(map$gen_subject_uuid, uuids)]
  }
  map
}

# Long covariate rows -> one column per covariate name, per subject, typed by
# the served `type` (schema §10.6): `categorical` -> `value_str`; `count` /
# `binary` -> `value_int`; continuous kinds -> `value_float`. A type-less
# (pre-0.3) table falls back to whichever typed column is populated.
vmx_nlmixr_covariates <- function(covariates, eligibility) {
  if (is.null(covariates) || nrow(covariates) == 0L || !"name" %in% names(covariates)) return(NULL)
  admitted <- vmx_nlmixr_admit(covariates, eligibility, "covariates")
  covariates <- admitted$rows
  subjects <- unique(as.character(covariates$gen_subject_uuid))
  out <- data.frame(gen_subject_uuid = subjects, stringsAsFactors = FALSE)
  types <- as.character(vmx_nlmixr_optional(covariates, "type"))
  for (nm in unique(as.character(covariates$name))) {
    sel <- which(covariates$name == nm)
    rows <- covariates[sel, , drop = FALSE]
    if (anyDuplicated(rows$gen_subject_uuid)) {
      vmx_abort_response(
        sprintf("covariate '%s' has more than one value for a subject.", nm),
        field = "covariates"
      )
    }
    numeric <- vmx_nlmixr_value(rows)
    textual <- as.character(vmx_nlmixr_optional(rows, "value_str"))
    values <- if (any(types[sel] %in% c("categorical", "string"))) {
      textual
    } else if (all(is.na(numeric)) && any(!is.na(textual))) {
      textual
    } else {
      numeric
    }
    out[[nm]] <- values[match(subjects, as.character(rows$gen_subject_uuid))]
  }
  out
}
