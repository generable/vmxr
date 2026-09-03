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
#   * `dose_mass` is the canonical administered mass in mg; `dose_rate` (mg/h) and
#     `dose_duration` (h) describe infusions; `route` is a closed vocabulary;
#   * PK `value_float` carries the canonical concentration, `censoring` is
#     `"BLQ"` / `"ALQ"` / null with the row's `lloq` / `uloq`.

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
#' Columns: `ID` (dense integer per subject), `TIME` (hours on the selected
#' time basis), `DV`, `AMT` (mg), `EVID` (0 observation / 1 dose), `MDV`, `CMT`,
#' `RATE` (mg/h, > 0 for infusions), `II`, `ADDL`, `SS` (always 0 — the
#' DataVersion already expands doses per administration), `CENS` / `LIMIT`
#' (censoring: `CENS = 1` with `DV = lloq` and `LIMIT = 0` for BLQ rows,
#' `CENS = -1` with `DV = uloq` and `LIMIT = Inf` for ALQ rows), `DVID`
#' (1 = PK, 2.. = PD markers when requested), `subject_id`, and one column per
#' subject-level covariate.
#'
#' Only rows the DataVersion marks as eligible for modeling are kept; the
#' after-QC flag is the contract's mandatory admission filter. Rows dropped by
#' eligibility, censoring encoding, or analyte selection are counted in the
#' `"vmx"` attribute so nothing disappears silently.
#'
#' @param dv A data-version id (`dv_...`) or `vmx_data_version`.
#' @param analyte PK analyte to assemble, matched against `biomarker_name` or
#'   `biomarker_code`. Required when the PK table carries more than one analyte.
#' @param time_basis Time basis to project on (e.g. `"observed"`). Defaults to
#'   the DataVersion's recommended basis; a basis the DataVersion does not list
#'   as available is refused.
#' @param eligibility Which modeling-eligibility flag admits a row:
#'   `"after_qc"` (default, the contract's admission filter), `"before_qc"`, or
#'   `"all"` (no filter — for review only; the result is not analysis-ready).
#' @param pd_markers PD markers to include as additional observation endpoints:
#'   `NULL`/`FALSE` (default, PK only), `TRUE` (every marker in the `pd`
#'   table), or a character vector of marker names.
#' @param cmt Named integer vector mapping dosing routes and `"observation"` to
#'   compartments; see [vmx_nlmixr_default_cmt()]. PD markers get compartments
#'   after the highest mapped value, in the order they are included.
#' @param client A `vmx_client`.
#' @return A tibble in NONMEM layout. The `"vmx"` attribute is a list with
#'   `data_version_id`, `time_basis`, `units`, `analyte`, `eligibility`, `cmt`,
#'   `endpoints` (a tibble of `dvid`, `name`, `cmt`, `unit`), `dropped`
#'   (named integer counts), and `n_subjects`.
#' @seealso [vmx_model_data()] for the underlying tidy tables.
#' @export
vmx_nlmixr_data <- function(dv, analyte = NULL, time_basis = NULL,
                            eligibility = c("after_qc", "before_qc", "all"),
                            pd_markers = NULL, cmt = NULL,
                            client = vmx_client()) {
  eligibility <- match.arg(eligibility)
  cmt <- vmx_nlmixr_cmt_map(cmt)
  if (!is.null(analyte)) {
    analyte <- vmx_nonempty_strings(analyte, "analyte", exactly_one = TRUE)
  }
  pd_selection <- vmx_nlmixr_pd_selection(pd_markers)

  md <- vmx_model_data(dv, time_basis = time_basis, client = client)
  basis <- md$meta$time_basis
  if (is.null(md$pk) || is.null(md$dosing)) {
    vmx_abort(
      "The DataVersion has no prepared `pk` and `dosing` tables; an nlmixr2 dataset needs both.",
      class = "vmx_usage_error"
    )
  }
  dropped <- list()

  # ---- subjects -----------------------------------------------------------
  subjects <- vmx_nlmixr_subject_map(md$subjects, md$pk, md$dosing)

  # ---- PK observations ----------------------------------------------------
  pk <- vmx_nlmixr_admit(md$pk, eligibility, "pk")
  dropped$pk_ineligible <- pk$n_dropped
  pk <- pk$rows
  pk_analyte <- vmx_nlmixr_select_analyte(pk, analyte)
  dropped$pk_other_analytes <- nrow(pk) - nrow(pk_analyte$rows)
  pk <- pk_analyte$rows
  pk_time <- vmx_nlmixr_time_column(pk, basis, "pk")
  pk_obs <- vmx_nlmixr_observations(
    pk,
    time = pk_time,
    dvid = 1L,
    cmt = unname(cmt[["observation"]]),
    domain = "pk"
  )

  # ---- doses --------------------------------------------------------------
  dosing <- vmx_nlmixr_admit(md$dosing, eligibility, "dosing")
  dropped$dosing_ineligible <- dosing$n_dropped
  dosing <- dosing$rows
  dose_time <- vmx_nlmixr_time_column(dosing, basis, "dosing")
  doses <- vmx_nlmixr_doses(dosing, time = dose_time, cmt = cmt)

  # ---- PD endpoints (optional) --------------------------------------------
  endpoints <- tibble::tibble(
    dvid = 1L,
    name = pk_analyte$name,
    cmt = unname(cmt[["observation"]]),
    unit = vmx_nlmixr_unit(pk, md$meta$units$pk_concentration)
  )
  pd_obs <- NULL
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
    markers <- vmx_nlmixr_pd_markers(pd, pd_selection)
    pd_time <- vmx_nlmixr_time_column(pd, basis, "pd")
    next_cmt <- max(cmt)
    pd_parts <- vector("list", length(markers))
    for (k in seq_along(markers)) {
      rows_k <- pd[vmx_nlmixr_marker_key(pd) == markers[[k]], , drop = FALSE]
      next_cmt <- next_cmt + 1L
      pd_parts[[k]] <- vmx_nlmixr_observations(
        rows_k,
        time = pd_time[vmx_nlmixr_marker_key(pd) == markers[[k]]],
        dvid = 1L + k,
        cmt = next_cmt,
        domain = "pd"
      )
      endpoints <- rbind(endpoints, tibble::tibble(
        dvid = 1L + k,
        name = markers[[k]],
        cmt = next_cmt,
        unit = vmx_nlmixr_unit(rows_k, NA_character_)
      ))
    }
    pd_obs <- do.call(rbind, pd_parts)
  }

  # ---- assemble -----------------------------------------------------------
  events <- rbind(pk_obs, doses, pd_obs)
  if (nrow(events) == 0L) {
    vmx_abort(
      "No eligible PK observations or dose events remain for this DataVersion, analyte, and time basis.",
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
  events$ID <- ids
  events$subject_id <- subjects$subject_id[match(events$gen_subject_uuid, subjects$gen_subject_uuid)]
  events <- events[order(events$ID, events$TIME, events$EVID, events$CMT, events$DVID), , drop = FALSE]

  covariates <- vmx_nlmixr_covariates(md$covariates, eligibility)
  if (!is.null(covariates)) {
    clash <- intersect(setdiff(names(covariates), "gen_subject_uuid"), names(events))
    if (length(clash)) {
      vmx_abort(
        sprintf(
          "Covariate name(s) collide with reserved event columns: %s.",
          paste(clash, collapse = ", ")
        ),
        class = "vmx_usage_error"
      )
    }
    idx <- match(events$gen_subject_uuid, covariates$gen_subject_uuid)
    for (nm in setdiff(names(covariates), "gen_subject_uuid")) {
      events[[nm]] <- covariates[[nm]][idx]
    }
  }
  extra_subject_cols <- setdiff(names(subjects), c("gen_subject_uuid", "id", "subject_id"))
  for (nm in extra_subject_cols) {
    if (!nm %in% names(events)) {
      events[[nm]] <- subjects[[nm]][match(events$gen_subject_uuid, subjects$gen_subject_uuid)]
    }
  }

  core <- c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "CMT", "RATE", "II", "ADDL",
            "SS", "CENS", "LIMIT", "DVID", "subject_id")
  out <- tibble::as_tibble(events[, c(core, setdiff(names(events), c(core, "gen_subject_uuid"))), drop = FALSE])
  rownames(out) <- NULL
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
vmx_nlmixr_admit <- function(tbl, eligibility, domain) {
  flag <- vmx_nlmixr_flag_column(eligibility)
  if (is.null(flag)) return(list(rows = tbl, n_dropped = 0L))
  if (!flag %in% names(tbl)) {
    vmx_abort(
      sprintf(
        "The `%s` table has no `%s` column; this DataVersion predates the v0.3 eligibility flags. Use `eligibility = \"all\"` to assemble it unfiltered.",
        domain, flag
      ),
      class = "vmx_usage_error"
    )
  }
  keep <- tbl[[flag]] %in% TRUE
  list(rows = tbl[keep, , drop = FALSE], n_dropped = sum(!keep))
}

vmx_nlmixr_time_column <- function(tbl, basis, domain) {
  candidates <- c("time_hours", if (!is.null(basis)) paste0(basis, "_time_hours"))
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

vmx_nlmixr_select_analyte <- function(pk, analyte) {
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
  hit <- key == analyte | code == analyte
  hit[is.na(hit)] <- FALSE
  if (!any(hit)) {
    vmx_abort(
      sprintf(
        "`analyte` '%s' is not in the PK table (available: %s).",
        analyte, paste(present, collapse = ", ")
      ),
      class = "vmx_usage_error"
    )
  }
  list(rows = pk[hit, , drop = FALSE], name = unique(key[hit])[[1]])
}

vmx_nlmixr_pd_markers <- function(pd, selection) {
  key <- vmx_nlmixr_marker_key(pd)
  present <- unique(key[!is.na(key)])
  if (isTRUE(selection)) return(present)
  missing <- setdiff(selection, present)
  if (length(missing)) {
    vmx_abort(
      sprintf(
        "PD marker(s) not in the `pd` table: %s (available: %s).",
        paste(missing, collapse = ", "), paste(present, collapse = ", ")
      ),
      class = "vmx_usage_error"
    )
  }
  selection
}

# Row-wise numeric value: the typed columns first (`value_float`, then
# `value_int`), falling back to the untyped `value` where both are null.
vmx_nlmixr_value <- function(tbl) {
  out <- rep(NA_real_, nrow(tbl))
  for (col in c("value_float", "value_int", "value")) {
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

# Observation rows (PK or one PD marker) in NONMEM layout.
vmx_nlmixr_observations <- function(tbl, time, dvid, cmt, domain) {
  n <- nrow(tbl)
  dv <- vmx_nlmixr_value(tbl)
  cens <- rep(0L, n)
  limit <- rep(NA_real_, n)
  censoring <- toupper(as.character(vmx_nlmixr_optional(tbl, "censoring")))
  lloq <- as.numeric(vmx_nlmixr_optional(tbl, "lloq"))
  uloq <- as.numeric(vmx_nlmixr_optional(tbl, "uloq"))
  blq <- censoring %in% "BLQ"
  alq <- censoring %in% "ALQ"
  if (any(blq & is.na(lloq))) {
    vmx_abort_response(
      sprintf("a BLQ row in the `%s` table has no `lloq`; the censoring interval cannot be encoded.", domain),
      field = "lloq"
    )
  }
  if (any(alq & is.na(uloq))) {
    vmx_abort_response(
      sprintf("an ALQ row in the `%s` table has no `uloq`; the censoring interval cannot be encoded.", domain),
      field = "uloq"
    )
  }
  dv[blq] <- lloq[blq]
  cens[blq] <- 1L
  limit[blq] <- 0
  dv[alq] <- uloq[alq]
  cens[alq] <- -1L
  limit[alq] <- Inf
  if (any(is.na(dv) & !blq & !alq)) {
    vmx_abort_response(
      sprintf("an eligible `%s` observation has no numeric value and no censoring mark.", domain),
      field = "value_float"
    )
  }
  data.frame(
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
    DVID = as.integer(dvid),
    stringsAsFactors = FALSE
  )
}

# Dose rows in NONMEM layout: AMT from canonical dose_mass, RATE for infusions.
vmx_nlmixr_doses <- function(dosing, time, cmt) {
  n <- nrow(dosing)
  if (!"dose_mass" %in% names(dosing)) {
    vmx_abort_response("the `dosing` table has no `dose_mass` column.", field = "dose_mass")
  }
  amt <- as.numeric(dosing$dose_mass)
  if (anyNA(amt)) {
    vmx_abort_response(
      "an eligible dose has no canonical `dose_mass`; the contract requires it on eligible rows.",
      field = "dose_mass"
    )
  }
  route <- as.character(vmx_nlmixr_optional(dosing, "route"))
  unknown <- setdiff(unique(route[!is.na(route)]), setdiff(names(cmt), "observation"))
  if (anyNA(route) || length(unknown)) {
    vmx_abort_response(
      sprintf(
        "eligible dose rows carry a `route` outside the compartment map (%s).",
        paste(c(unknown, if (anyNA(route)) "<null>"), collapse = ", ")
      ),
      field = "route"
    )
  }
  rate <- rep(0, n)
  infusion <- route == "iv-infusion"
  if (any(infusion)) {
    dose_rate <- as.numeric(vmx_nlmixr_optional(dosing, "dose_rate"))
    duration <- as.numeric(vmx_nlmixr_optional(dosing, "dose_duration"))
    from_rate <- infusion & !is.na(dose_rate) & dose_rate > 0
    from_duration <- infusion & !from_rate & !is.na(duration) & duration > 0
    rate[from_rate] <- dose_rate[from_rate]
    rate[from_duration] <- amt[from_duration] / duration[from_duration]
    if (any(infusion & !from_rate & !from_duration)) {
      vmx_abort_response(
        "an eligible `iv-infusion` dose has neither `dose_rate` nor `dose_duration`; RATE cannot be derived.",
        field = "dose_rate"
      )
    }
  }
  data.frame(
    gen_subject_uuid = as.character(dosing$gen_subject_uuid),
    TIME = time,
    DV = NA_real_,
    AMT = amt,
    EVID = 1L,
    MDV = 1L,
    CMT = as.integer(unname(cmt[route])),
    RATE = rate,
    II = 0,
    ADDL = 0L,
    SS = 0L,
    CENS = 0L,
    LIMIT = NA_real_,
    DVID = 0L,
    stringsAsFactors = FALSE
  )
}

# Dense integer IDs plus the human subject_id, arm and dose_group when served.
vmx_nlmixr_subject_map <- function(subjects, pk, dosing) {
  uuids <- character()
  ids <- character()
  if (!is.null(subjects) && "gen_subject_uuid" %in% names(subjects)) {
    uuids <- as.character(subjects$gen_subject_uuid)
    ids <- if ("subject_id" %in% names(subjects)) as.character(subjects$subject_id) else uuids
  }
  seen <- unique(c(as.character(pk$gen_subject_uuid), as.character(dosing$gen_subject_uuid)))
  extra <- setdiff(seen, uuids)
  if (length(extra)) {
    from_rows <- rbind(
      if ("subject_id" %in% names(pk)) data.frame(u = as.character(pk$gen_subject_uuid), s = as.character(pk$subject_id)) else NULL,
      if ("subject_id" %in% names(dosing)) data.frame(u = as.character(dosing$gen_subject_uuid), s = as.character(dosing$subject_id)) else NULL
    )
    uuids <- c(uuids, extra)
    ids <- c(ids, vapply(extra, function(u) {
      s <- if (!is.null(from_rows)) from_rows$s[match(u, from_rows$u)] else NA_character_
      if (is.na(s)) u else s
    }, character(1), USE.NAMES = FALSE))
  }
  ord <- order(suppressWarnings(as.numeric(ids)), ids, na.last = TRUE)
  map <- data.frame(
    gen_subject_uuid = uuids[ord],
    subject_id = ids[ord],
    id = seq_along(ord),
    stringsAsFactors = FALSE
  )
  if (!is.null(subjects)) {
    for (nm in intersect(c("arm", "dose_group"), names(subjects))) {
      map[[nm]] <- as.character(subjects[[nm]])[match(map$gen_subject_uuid, subjects$gen_subject_uuid)]
    }
  }
  map
}

# Long covariate rows -> one typed column per covariate name, per subject.
vmx_nlmixr_covariates <- function(covariates, eligibility) {
  if (is.null(covariates) || nrow(covariates) == 0L || !"name" %in% names(covariates)) return(NULL)
  flag <- vmx_nlmixr_flag_column(eligibility)
  if (!is.null(flag) && flag %in% names(covariates)) {
    covariates <- covariates[covariates[[flag]] %in% TRUE, , drop = FALSE]
  }
  subjects <- unique(as.character(covariates$gen_subject_uuid))
  out <- data.frame(gen_subject_uuid = subjects, stringsAsFactors = FALSE)
  for (nm in unique(as.character(covariates$name))) {
    rows <- covariates[covariates$name == nm, , drop = FALSE]
    num <- vmx_nlmixr_value(rows)
    chr <- as.character(vmx_nlmixr_optional(rows, "value_str"))
    values <- if (all(is.na(num)) && any(!is.na(chr))) chr else num
    if (anyDuplicated(rows$gen_subject_uuid)) {
      vmx_abort_response(
        sprintf("covariate '%s' has more than one value for a subject.", nm),
        field = "covariates"
      )
    }
    out[[nm]] <- values[match(subjects, as.character(rows$gen_subject_uuid))]
  }
  out
}
