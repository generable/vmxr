# Specimen-aware PK and PD selection for the event-dataset builders.
#
# A biomarker measured in more than one specimen is one marker with one
# biomarker–specimen pair per specimen (dataversion-schema §4.2, §11.6). An
# analysis input names one recorded specimen of a marker, or All specimens of it
# (api-contract §5.6.2). The builders never choose a specimen for the caller, and
# the DataVersion's recommended PK specimen is never used as an implicit choice.
#
# Eligibility for modeling is recorded per scope (api-contract §5.4;
# modeling-eligibility-spec §5.5):
#   * PK All specimens: the basis' `pk_modeling_eligibility`;
#   * one PK specimen: the basis' `pk_marker_eligibility` entry for that pair,
#     or the marker-analysis projection's when the DataVersion omits it;
#   * one PD pair: the basis' `pd_marker_eligibility` entry, joined on
#     (`gen_uuid`, `specimen_type`);
#   * PD All specimens: the marker-analysis projection's
#     `pd_all_specimens_eligibility`, or, when the DataVersion has no
#     marker-analysis metadata, eligible when any recorded pair of the marker is.
# A DataVersion that predates specimens (no `specimen_type` column, verdicts keyed
# by marker `gen_uuid` alone) keeps the marker-level behaviour.

# The `vmx_all_specimens()` sentinel, `vmx_pd_marker()` and `vmx_specimen_scope()`
# are shared with the analysis requests (specimens.R).

vmx_nlmixr_specimen_is_all <- function(x) inherits(x, "vmx_all_specimens")

# `pk_specimen` -> NULL, list(specimen_type = <s>) or list(all_specimens = TRUE),
# the R mirror of the contract's `PkSpecimenSelection`.
vmx_nlmixr_specimen_pk_arg <- function(pk_specimen) {
  if (is.null(pk_specimen)) return(NULL)
  vmx_specimen_scope(pk_specimen, "pk_specimen")
}

# `pd_markers` -> FALSE, TRUE, or a list of requests, each
# list(marker = <name> | NULL, gen_uuid = <uuid> | NULL, specimen_type = <s> | NULL,
# all_specimens = <lgl>). `specimen_type = NULL` without `all_specimens` means
# "no specimen named"; a request from `vmx_pd_marker()` names its marker by
# `gen_uuid`, resolved against the DataVersion's `pd_markers[]` later.
vmx_nlmixr_specimen_pd_arg <- function(pd_markers) {
  if (is.null(pd_markers) || isFALSE(pd_markers)) return(FALSE)
  if (isTRUE(pd_markers)) return(TRUE)
  usage <- function() {
    vmx_abort(
      paste0(
        "`pd_markers` must be `TRUE`, `FALSE`, a character vector of marker names, ",
        "a named list mapping each marker to specimen names or `vmx_all_specimens()`, ",
        "or `vmx_pd_marker()` selections."
      ),
      class = "vmx_usage_error"
    )
  }
  if (is.character(pd_markers) || is.factor(pd_markers)) {
    markers <- vmx_nonempty_strings(pd_markers, "pd_markers", unique = TRUE)
    return(lapply(markers, function(m) {
      list(marker = m, gen_uuid = NULL, specimen_type = NULL, all_specimens = FALSE)
    }))
  }
  if (inherits(pd_markers, "vmx_pd_marker") ||
      (is.list(pd_markers) && length(pd_markers) && is.null(names(pd_markers)) &&
         all(vapply(pd_markers, inherits, logical(1), "vmx_pd_marker")))) {
    # The same selections the modeling requests take; `direction` is a
    # run-scoped modeling assumption and plays no part in the data layout.
    sels <- vmx_pd_marker_body(pd_markers, direction = FALSE)
    return(lapply(sels, function(m) {
      list(marker = NULL, gen_uuid = m$gen_uuid, specimen_type = m$specimen_type,
           all_specimens = isTRUE(m$all_specimens))
    }))
  }
  if (!is.list(pd_markers) || !length(pd_markers) || vmx_nlmixr_specimen_is_all(pd_markers)) usage()
  markers <- names(pd_markers)
  if (is.null(markers) || anyNA(markers) || !all(nzchar(trimws(markers)))) usage()
  if (anyDuplicated(markers)) {
    vmx_abort("`pd_markers` must not name a marker twice.", class = "vmx_usage_error")
  }
  out <- list()
  for (m in markers) {
    spec <- pd_markers[[m]]
    if (vmx_nlmixr_specimen_is_all(spec)) {
      out[[length(out) + 1L]] <- list(marker = m, gen_uuid = NULL, specimen_type = NULL,
                                      all_specimens = TRUE)
      next
    }
    if (is.list(spec) && any(vapply(spec, vmx_nlmixr_specimen_is_all, logical(1)))) {
      # api-contract §5.6.2: All specimens and a specific specimen of the same
      # PD marker may not be selected together.
      vmx_abort(
        sprintf("`pd_markers` selects marker '%s' both pooled and by specimen; choose one.", m),
        class = "vmx_usage_error"
      )
    }
    if (!is.character(spec) && !is.factor(spec)) usage()
    spec <- vmx_nonempty_strings(spec, sprintf("pd_markers$%s", m), unique = TRUE)
    for (s in spec) {
      out[[length(out) + 1L]] <- list(marker = m, gen_uuid = NULL, specimen_type = s,
                                      all_specimens = FALSE)
    }
  }
  out
}

# Name the marker of each `vmx_pd_marker()` request from the DataVersion's
# `pd_markers[]` registry (`gen_uuid` -> `name`).
vmx_nlmixr_specimen_pd_names <- function(requests, meta) {
  manifest <- if (is.list(meta$pd_markers)) meta$pd_markers else list()
  lapply(requests, function(r) {
    if (!is.null(r$marker)) return(r)
    hit <- Filter(function(e) identical(vmx_nlmixr_specimen_str(e$gen_uuid), r$gen_uuid), manifest)
    name <- unique(vapply(hit, function(e) vmx_nlmixr_specimen_str(e$name), character(1)))
    if (length(name) != 1L || is.na(name)) {
      vmx_abort(
        sprintf("PD marker '%s' is not in the DataVersion's `pd_markers`.", r$gen_uuid),
        class = "vmx_usage_error"
      )
    }
    r$marker <- name
    r
  })
}

# Recorded specimens of a table's rows, sorted; character() when the table
# records no specimen (a DataVersion that predates specimens). `"unspecified"`
# is a recorded value like any other.
vmx_nlmixr_specimen_values <- function(tbl) {
  if (is.null(tbl) || !"specimen_type" %in% names(tbl)) return(character())
  sort(unique(stats::na.omit(as.character(tbl$specimen_type))))
}

vmx_nlmixr_specimen_column <- function(tbl) {
  if ("specimen_type" %in% names(tbl)) as.character(tbl$specimen_type) else rep(NA_character_, nrow(tbl))
}

vmx_nlmixr_specimen_list <- function(x) if (length(x)) paste(x, collapse = ", ") else "none recorded"

# Shared lookup state for one assembly: the DataVersion metadata, the selected
# basis and eligibility flag, and a lazily fetched marker-analysis projection
# (`GET /data-versions/{id}/marker-analysis`), read at most once and only when a
# scoped verdict is not on the DataVersion itself.
vmx_nlmixr_specimen_context <- function(meta, basis, eligibility, client) {
  cache <- new.env(parent = emptyenv())
  list(
    meta = meta,
    basis = basis,
    flag = vmx_nlmixr_flag_column(eligibility),
    marker_analysis = function() {
      if (!exists("value", envir = cache, inherits = FALSE)) {
        body <- vmx_get(
          client,
          paste0("/data-versions/", meta$data_version_id, "/marker-analysis")
        )
        if (!is.list(body) || !"marker_analysis" %in% names(body)) {
          vmx_abort_response(
            "required field 'marker analysis.marker_analysis' is missing.",
            field = "marker_analysis"
          )
        }
        assign("value", body$marker_analysis, envir = cache)
      }
      get("value", envir = cache, inherits = FALSE)
    }
  )
}

vmx_nlmixr_specimen_basis <- function(ctx) {
  bases <- ctx$meta$time_bases
  if (is.null(ctx$basis) || !is.list(bases) || !is.list(bases[[ctx$basis]])) return(NULL)
  bases[[ctx$basis]]
}

# The marker-analysis projection's summaries on the selected basis, or NULL when
# the DataVersion has no marker-analysis metadata.
vmx_nlmixr_specimen_analysis_basis <- function(ctx) {
  ma <- ctx$marker_analysis()
  if (is.null(ma)) return(NULL)
  basis <- if (is.list(ma$time_bases)) ma$time_bases[[ctx$basis]] else NULL
  if (!is.list(basis)) {
    vmx_abort_response(
      sprintf("marker analysis has no summaries for time basis '%s'.", ctx$basis),
      field = "marker_analysis.time_bases"
    )
  }
  basis
}

# A verdict entry's selected flag as TRUE / FALSE, or NA when not carried.
vmx_nlmixr_specimen_flag_value <- function(entry, flag) {
  value <- entry[[flag]]
  if (is.logical(value) && length(value) == 1L && !is.na(value)) value else NA
}

vmx_nlmixr_specimen_str <- function(x) as.character(x %||% NA_character_)

# Entries of a verdict array for one marker uuid (any, when NULL) and specimen.
# An entry without `specimen_type` is a marker-level verdict from a DataVersion
# that predates specimens; it matches any specimen of its marker.
vmx_nlmixr_specimen_match <- function(entries, uuids, specimen) {
  if (!is.list(entries)) return(list())
  Filter(function(e) {
    (is.null(uuids) || vmx_nlmixr_specimen_str(e$gen_uuid) %in% uuids) &&
      (is.null(e$specimen_type) || is.na(specimen) ||
         identical(vmx_nlmixr_specimen_str(e$specimen_type), specimen))
  }, entries)
}

vmx_nlmixr_specimen_label <- function(scope) {
  if (isTRUE(scope$all_specimens)) "All specimens" else sprintf("specimen '%s'", scope$specimen_type)
}

# ---- PK ----------------------------------------------------------------------

# Resolve the PK specimen scope. `rows` are the admitted rows of the selected
# analyte; `all_rows` are that analyte's rows before admission, so the specimens
# on offer do not depend on the eligibility rule. Returns the scoped rows, the
# accepted scope (`PkSpecimenSelection` shape, or NULL when nothing is recorded
# and nothing was selected), the scope's recorded verdict, and the count of
# admitted rows outside the scope.
vmx_nlmixr_specimen_pk_scope <- function(rows, all_rows, selection, ctx) {
  recorded <- vmx_nlmixr_specimen_values(all_rows)
  if (is.null(selection)) {
    if (length(recorded) > 1L) {
      vmx_abort(
        c(
          sprintf(
            "The PK data records %d specimens (%s); choose one with `pk_specimen`.",
            length(recorded), vmx_nlmixr_specimen_list(recorded)
          ),
          "i" = "Pass one specimen name, or `pk_specimen = vmx_all_specimens()` to pool them."
        ),
        class = "vmx_usage_error"
      )
    }
    # Zero or one recorded specimen: the rows are already that one scope, and
    # the marker-level behaviour applies.
    scope <- if (length(recorded)) list(specimen_type = recorded) else NULL
    return(list(rows = rows, scope = scope, verdict = NA, n_dropped = 0L))
  }
  uuids <- if ("gen_biomarker_uuid" %in% names(all_rows)) {
    unique(stats::na.omit(as.character(all_rows$gen_biomarker_uuid)))
  }
  if (length(uuids) == 0L) uuids <- NULL
  if (isTRUE(selection$all_specimens)) {
    verdict <- vmx_nlmixr_specimen_pk_pooled_verdict(ctx)
    if (length(recorded) > 1L) vmx_nlmixr_specimen_check_units(rows, NULL, "PK", "the PK marker")
    keep <- rep(TRUE, nrow(rows))
  } else {
    s <- selection$specimen_type
    if (!s %in% recorded) {
      vmx_abort(
        sprintf(
          "`pk_specimen` '%s' is not a recorded PK specimen (available: %s).",
          s, vmx_nlmixr_specimen_list(recorded)
        ),
        class = "vmx_usage_error"
      )
    }
    verdict <- vmx_nlmixr_specimen_pk_pair_verdict(ctx, uuids, s)
    keep <- vmx_nlmixr_specimen_column(rows) %in% s
  }
  if (isFALSE(verdict)) {
    vmx_abort(
      c(
        sprintf(
          "PK %s is not eligible for modeling on basis '%s' under `eligibility = \"%s\"`.",
          vmx_nlmixr_specimen_label(selection), ctx$basis %||% "?",
          if (identical(ctx$flag, "eligible_for_modeling_before_qc")) "before_qc" else "after_qc"
        ),
        "i" = "The table is readable for review with `eligibility = \"all\"`."
      ),
      class = "vmx_usage_error"
    )
  }
  list(rows = rows[keep, , drop = FALSE], scope = selection, verdict = verdict,
       n_dropped = sum(!keep))
}

vmx_nlmixr_specimen_pk_pooled_verdict <- function(ctx) {
  if (is.null(ctx$flag)) return(NA)
  basis <- vmx_nlmixr_specimen_basis(ctx)
  if (is.null(basis) || !is.list(basis$pk_modeling_eligibility)) return(NA)
  vmx_nlmixr_specimen_flag_value(basis$pk_modeling_eligibility, ctx$flag)
}

# One PK specimen's recorded verdict. The DataVersion's `pk_marker_eligibility`
# is optional; the marker-analysis projection carries the same entries. Without
# either, only All specimens is available for PK (dataversion-schema §9.1.6).
vmx_nlmixr_specimen_pk_pair_verdict <- function(ctx, uuids, specimen) {
  if (is.null(ctx$flag)) return(NA)
  basis <- vmx_nlmixr_specimen_basis(ctx)
  entries <- if (is.list(basis)) basis$pk_marker_eligibility
  if (is.null(entries)) {
    analysis <- vmx_nlmixr_specimen_analysis_basis(ctx)
    if (is.null(analysis)) {
      vmx_abort(
        c(
          sprintf(
            "This DataVersion records no single-specimen PK verdicts, so PK specimen '%s' cannot be selected for modeling.",
            specimen
          ),
          "i" = "Use `pk_specimen = vmx_all_specimens()`, or `eligibility = \"all\"` for review."
        ),
        class = "vmx_usage_error"
      )
    }
    entries <- analysis$pk_marker_eligibility
  }
  hit <- vmx_nlmixr_specimen_match(entries, uuids, specimen)
  hit <- Filter(function(e) !is.null(e$specimen_type), hit)
  if (length(hit) != 1L) {
    vmx_abort_response(
      sprintf("`pk_marker_eligibility` has %d entries for one PK specimen on basis '%s'.", length(hit), ctx$basis),
      field = "pk_marker_eligibility"
    )
  }
  vmx_nlmixr_specimen_flag_value(hit[[1]], ctx$flag)
}

# ---- PD ----------------------------------------------------------------------

# Resolve the PD selection into endpoints. `pd` are the admitted rows and
# `all_rows` the table before admission. Returns `selections` (one per endpoint:
# marker, specimen_type, all_specimens, the admitted row indices and the pooled
# unit when pooled) and the count of markers dropped as ineligible under `TRUE`.
# An explicitly requested scope that is ineligible is a usage error.
vmx_nlmixr_specimen_pd_select <- function(pd, all_rows, requests, ctx) {
  key <- vmx_nlmixr_marker_key(pd)
  spec <- vmx_nlmixr_specimen_column(pd)
  all_key <- vmx_nlmixr_marker_key(all_rows)
  all_spec <- vmx_nlmixr_specimen_column(all_rows)
  present <- unique(key[!is.na(key)])
  recorded_for <- function(m) sort(unique(stats::na.omit(all_spec[all_key %in% m])))
  implicit <- isTRUE(requests)
  if (!implicit) requests <- vmx_nlmixr_specimen_pd_names(requests, ctx$meta)
  if (implicit) {
    # Every admitted marker, and every admitted specimen of it as its own
    # endpoint; specimens are never pooled implicitly.
    requests <- list()
    for (m in present) {
      specs <- sort(unique(stats::na.omit(spec[key %in% m])))
      if (!length(specs)) specs <- NA_character_
      for (s in specs) {
        requests[[length(requests) + 1L]] <- list(
          marker = m, specimen_type = if (is.na(s)) NULL else s, all_specimens = FALSE
        )
      }
    }
  }
  wanted <- unique(vapply(requests, function(r) r$marker, character(1)))
  missing <- setdiff(wanted, present)
  if (length(missing)) {
    served <- unique(stats::na.omit(all_key))
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

  selections <- list()
  ineligible <- character()
  for (r in requests) {
    m <- r$marker
    recorded <- recorded_for(m)
    if (!r$all_specimens && is.null(r$specimen_type) && length(recorded) > 1L) {
      vmx_abort(
        c(
          sprintf(
            "PD marker '%s' records %d specimens (%s); choose in `pd_markers`.",
            m, length(recorded), vmx_nlmixr_specimen_list(recorded)
          ),
          "i" = sprintf(
            "Name the specimens, e.g. `pd_markers = list(%s = \"%s\")`, or pool them with `list(%s = vmx_all_specimens())`.",
            m, recorded[[1]], m
          )
        ),
        class = "vmx_usage_error"
      )
    }
    if (!is.null(r$specimen_type) && !r$specimen_type %in% recorded) {
      vmx_abort(
        sprintf(
          "PD marker '%s' has no specimen '%s' (available: %s).",
          m, r$specimen_type, vmx_nlmixr_specimen_list(recorded)
        ),
        class = "vmx_usage_error"
      )
    }
    # One recorded specimen: an unnamed specimen is that specimen.
    specimen <- r$specimen_type %||% if (!r$all_specimens && length(recorded) == 1L) recorded else NULL
    in_scope <- key %in% m & (r$all_specimens | is.null(specimen) | spec %in% specimen)
    label <- if (r$all_specimens) {
      sprintf("%s (All specimens)", m)
    } else if (!is.null(specimen) && length(recorded) > 1L) {
      sprintf("%s (%s)", m, specimen)
    } else {
      m
    }
    if (!any(in_scope)) {
      vmx_abort(
        sprintf("1 requested PD marker(s) have no rows admitted by the selected eligibility rule (%s).", label),
        class = "vmx_usage_error"
      )
    }
    verdict <- if (r$all_specimens) {
      vmx_nlmixr_specimen_pd_pooled_verdict(ctx, m, recorded)
    } else {
      vmx_nlmixr_specimen_pd_pair_verdict(ctx, m, specimen %||% NA_character_)
    }
    if (isFALSE(verdict)) {
      ineligible <- c(ineligible, label)
      next
    }
    unit <- NULL
    if (r$all_specimens && length(recorded) > 1L) {
      unit <- vmx_nlmixr_specimen_check_units(
        pd[in_scope, , drop = FALSE], vmx_nlmixr_specimen_registry_units(ctx$meta, m, recorded),
        "PD", sprintf("PD marker '%s'", m)
      )
    }
    selections[[length(selections) + 1L]] <- list(
      marker = m,
      specimen_type = if (r$all_specimens) NA_character_ else specimen %||% NA_character_,
      all_specimens = isTRUE(r$all_specimens),
      rows = which(in_scope),
      unit = unit
    )
  }
  if (length(ineligible) && !implicit) {
    vmx_abort(
      sprintf(
        "%d requested PD marker(s) are not eligible for modeling on basis '%s' (%s).",
        length(ineligible), ctx$basis %||% "?", paste(ineligible, collapse = ", ")
      ),
      class = "vmx_usage_error"
    )
  }
  list(selections = selections, n_dropped = length(ineligible))
}

# Registry uuids of a PD marker (by name) from the DataVersion's `pd_markers[]`.
vmx_nlmixr_specimen_pd_uuids <- function(meta, marker) {
  manifest <- meta$pd_markers
  if (!is.list(manifest)) return(character())
  hit <- Filter(function(e) identical(vmx_nlmixr_specimen_str(e$name), marker), manifest)
  unique(stats::na.omit(vapply(hit, function(e) vmx_nlmixr_specimen_str(e$gen_uuid), character(1))))
}

# Registry units of a PD marker's recorded specimens (`pd_markers[].unit`).
vmx_nlmixr_specimen_registry_units <- function(meta, marker, specimens) {
  manifest <- meta$pd_markers
  if (!is.list(manifest)) return(character())
  hit <- Filter(function(e) {
    identical(vmx_nlmixr_specimen_str(e$name), marker) &&
      vmx_nlmixr_specimen_str(e$specimen_type) %in% specimens
  }, manifest)
  vapply(hit, function(e) vmx_nlmixr_specimen_str(e$unit), character(1))
}

# One PD pair's recorded verdict, joined on (`gen_uuid`, `specimen_type`); NA
# when the DataVersion does not carry one (the marker-level behaviour).
vmx_nlmixr_specimen_pd_pair_verdict <- function(ctx, marker, specimen) {
  if (is.null(ctx$flag)) return(NA)
  basis <- vmx_nlmixr_specimen_basis(ctx)
  uuids <- vmx_nlmixr_specimen_pd_uuids(ctx$meta, marker)
  if (is.null(basis) || !length(uuids)) return(NA)
  hit <- vmx_nlmixr_specimen_match(basis$pd_marker_eligibility, uuids, specimen)
  values <- unique(vapply(hit, vmx_nlmixr_specimen_flag_value, logical(1), flag = ctx$flag))
  if (length(values) == 1L) values else NA
}

# A PD marker's All-specimens verdict: the marker-analysis projection's recorded
# verdict, or, without marker-analysis metadata, eligible when any recorded pair
# is (modeling-eligibility-spec §5.5). With one recorded specimen the pooled
# scope is that pair.
vmx_nlmixr_specimen_pd_pooled_verdict <- function(ctx, marker, recorded) {
  if (is.null(ctx$flag)) return(NA)
  if (length(recorded) <= 1L) {
    return(vmx_nlmixr_specimen_pd_pair_verdict(ctx, marker, if (length(recorded)) recorded else NA_character_))
  }
  uuids <- vmx_nlmixr_specimen_pd_uuids(ctx$meta, marker)
  analysis <- vmx_nlmixr_specimen_analysis_basis(ctx)
  if (!is.null(analysis)) {
    hit <- vmx_nlmixr_specimen_match(analysis$pd_all_specimens_eligibility, uuids, NA_character_)
    if (length(hit) != 1L) {
      vmx_abort_response(
        sprintf(
          "`pd_all_specimens_eligibility` has %d entries for one PD marker on basis '%s'.",
          length(hit), ctx$basis
        ),
        field = "pd_all_specimens_eligibility"
      )
    }
    return(vmx_nlmixr_specimen_flag_value(hit[[1]], ctx$flag))
  }
  pairs <- vapply(recorded, function(s) vmx_nlmixr_specimen_pd_pair_verdict(ctx, marker, s), logical(1))
  if (any(pairs %in% TRUE)) TRUE else if (all(pairs %in% FALSE)) FALSE else NA
}

# ---- units -------------------------------------------------------------------

# A pooled selection needs one explicitly known unit across its specimens
# (dataversion-schema §11.6; modeling-eligibility-spec §5.5). `rows` are the
# pooled rows, `registry` any registry units for the pooled pairs. Returns the
# common unit; differing or unreported units are a usage error naming them.
vmx_nlmixr_specimen_check_units <- function(rows, registry, domain, what) {
  row_units <- character()
  for (col in c("unit", "value_unit")) {
    if (col %in% names(rows)) {
      row_units <- as.character(rows[[col]])
      break
    }
  }
  units <- c(row_units, registry)
  if (!length(units)) units <- NA_character_
  units[is.na(units) | !nzchar(trimws(units))] <- "unspecified"
  found <- sort(unique(units))
  if (length(found) != 1L || identical(found, "unspecified")) {
    vmx_abort(
      c(
        sprintf(
          "Pooling the specimens of %s needs one explicitly known unit; found: %s.",
          what, paste(found, collapse = ", ")
        ),
        "i" = sprintf("Select one %s specimen instead.", domain)
      ),
      class = "vmx_usage_error"
    )
  }
  found
}
