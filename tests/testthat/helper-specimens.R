# Hand-built two-specimen API 0.3 DataVersion for the specimen-aware assembly
# tests, shaped from the public contract (DataVersion `pd_markers[]` one entry per
# biomarker and specimen; per-basis `pk_modeling_eligibility`,
# `pk_marker_eligibility` and `pd_marker_eligibility`; the marker-analysis read).
# It reuses the subjects, dosing and covariates bodies of helper-nlmixr.R.
#
# PK: one concentration biomarker ("concentration", bm-conc) in plasma and serum.
#   S1 (u1): plasma at 1 h (10) and 4 h (5); serum at 1 h (8) and 4 h (4).
#   S2 (u2): plasma at 1 h (20); serum at 2 h (15).
# PD: "effect" (bm-effect) in plasma and serum, both "%";
#     "resp" (bm-resp) in plasma only, unit "1";
#     "titer" (bm-titer) in plasma ("ng/mL") and serum ("mg/L").

spec_pk_body <- function(serum_unit = "mg/L") {
  row <- function(u, t, value, specimen, unit = "mg/L") {
    c(list(
      gen_measurement_uuid = sprintf("m-pk-%s-%s-%g", u, specimen, t),
      gen_subject_uuid = u, observed_time_hours = t, time_hours = t,
      gen_biomarker_uuid = "bm-conc", biomarker_name = "concentration",
      specimen_type = specimen, value_float = value, unit = unit,
      lloq = NULL, uloq = NULL, censoring = NULL
    ), nlmixr_flags())
  }
  list(
    data_version_id = "dv_1", domain = "pk", time_basis = "observed",
    columns = c(list(
      nlmixr_col("gen_measurement_uuid", "string"),
      nlmixr_col("gen_subject_uuid", "string"),
      nlmixr_col("observed_time_hours", "number"), nlmixr_col("time_hours", "number"),
      nlmixr_col("gen_biomarker_uuid", "string"), nlmixr_col("biomarker_name", "string"),
      nlmixr_col("specimen_type", "string"),
      nlmixr_col("value_float", "number"), nlmixr_col("unit", "string"),
      nlmixr_col("lloq", "number"), nlmixr_col("uloq", "number"), nlmixr_col("censoring", "string")
    ), nlmixr_flag_cols()),
    rows = list(
      row("u1", 1, 10, "plasma"),
      row("u1", 4, 5, "plasma"),
      row("u1", 1, 8, "serum", unit = serum_unit),
      row("u1", 4, 4, "serum", unit = serum_unit),
      row("u2", 1, 20, "plasma"),
      row("u2", 2, 15, "serum", unit = serum_unit)
    )
  )
}

spec_pd_body <- function() {
  row <- function(u, t, name, uuid, specimen, vf = NULL, vi = NULL, unit = NULL) {
    c(list(
      gen_measurement_uuid = sprintf("m-pd-%s-%s-%s-%g", u, name, specimen, t),
      gen_subject_uuid = u, observed_time_hours = t, time_hours = t,
      gen_biomarker_uuid = uuid, biomarker_name = name, specimen_type = specimen,
      value_float = vf, value_int = vi, unit = unit
    ), nlmixr_flags())
  }
  list(
    data_version_id = "dv_1", domain = "pd", time_basis = "observed",
    columns = c(list(
      nlmixr_col("gen_measurement_uuid", "string"),
      nlmixr_col("gen_subject_uuid", "string"),
      nlmixr_col("observed_time_hours", "number"), nlmixr_col("time_hours", "number"),
      nlmixr_col("gen_biomarker_uuid", "string"), nlmixr_col("biomarker_name", "string"),
      nlmixr_col("specimen_type", "string"),
      nlmixr_col("value_float", "number"), nlmixr_col("value_int", "integer"),
      nlmixr_col("unit", "string")
    ), nlmixr_flag_cols()),
    rows = list(
      row("u1", 1, "effect", "bm-effect", "plasma", vf = 50, unit = "%"),
      row("u2", 1, "effect", "bm-effect", "plasma", vf = 40, unit = "%"),
      row("u1", 2, "effect", "bm-effect", "serum", vf = 45, unit = "%"),
      row("u1", 1, "resp", "bm-resp", "plasma", vi = 1L, unit = "1"),
      row("u1", 1, "titer", "bm-titer", "plasma", vf = 3, unit = "ng/mL"),
      row("u2", 1, "titer", "bm-titer", "serum", vf = 0.002, unit = "mg/L")
    )
  )
}

spec_elig <- function(after = TRUE, before = TRUE) {
  list(eligible_for_modeling_before_qc = before, eligible_for_modeling_after_qc = after,
       modeling_ineligibility_reasons_before_qc = list(),
       additional_modeling_ineligibility_reasons_after_qc = list())
}

spec_pair_elig <- function(uuid, specimen, after = TRUE) {
  c(list(gen_uuid = uuid, specimen_type = specimen), spec_elig(after))
}

# `pk_pairs` / `pd_pairs` are named logical after-QC verdicts per pair;
# `pk_marker = FALSE` omits the optional root `pk_marker_eligibility`.
spec_dv_body <- function(pk_pooled = TRUE,
                         pk_pairs = c(plasma = TRUE, serum = TRUE),
                         pk_marker = TRUE,
                         pd_pairs = c(`effect/plasma` = TRUE, `effect/serum` = TRUE,
                                      `resp/plasma` = TRUE, `titer/plasma` = TRUE,
                                      `titer/serum` = TRUE)) {
  pd_registry <- list(
    list(gen_uuid = "bm-effect", name = "effect", specimen_type = "plasma",
         display_name = "effect", type = "continuous", unit = "%"),
    list(gen_uuid = "bm-effect", name = "effect", specimen_type = "serum",
         display_name = "effect", type = "continuous", unit = "%"),
    list(gen_uuid = "bm-resp", name = "resp", specimen_type = "plasma",
         display_name = "resp", type = "binary", unit = "1"),
    list(gen_uuid = "bm-titer", name = "titer", specimen_type = "plasma",
         display_name = "titer", type = "continuous", unit = "ng/mL"),
    list(gen_uuid = "bm-titer", name = "titer", specimen_type = "serum",
         display_name = "titer", type = "continuous", unit = "mg/L")
  )
  uuid_of <- c(effect = "bm-effect", resp = "bm-resp", titer = "bm-titer")
  pd_elig <- lapply(names(pd_pairs), function(k) {
    parts <- strsplit(k, "/", fixed = TRUE)[[1]]
    spec_pair_elig(uuid_of[[parts[[1]]]], parts[[2]], pd_pairs[[k]])
  })
  observed <- list(
    available = TRUE,
    pk_modeling_eligibility = spec_elig(pk_pooled),
    pd_marker_eligibility = pd_elig
  )
  if (pk_marker) {
    observed$pk_marker_eligibility <- lapply(
      names(pk_pairs), function(s) spec_pair_elig("bm-conc", s, pk_pairs[[s]])
    )
  }
  body <- nlmixr_dv_body(time_bases = list(
    observed = observed,
    nominal = list(available = FALSE, availability_reasons = list("no nominal times"))
  ))
  body$pd_markers <- pd_registry
  body$recommended_pk_specimen <- list(value = "plasma", reason = "most subjects")
  body
}

# The marker-analysis read. `pd_all` are named after-QC verdicts per PD marker;
# `present = FALSE` serves `marker_analysis: null` (no marker-analysis metadata).
spec_marker_analysis_body <- function(present = TRUE,
                                      pk_pairs = c(plasma = TRUE, serum = TRUE),
                                      pd_all = c(effect = TRUE, resp = TRUE, titer = FALSE)) {
  uuid_of <- c(effect = "bm-effect", resp = "bm-resp", titer = "bm-titer")
  # `NA` serialises to JSON null; an R NULL would serialise to `{}`.
  ma <- if (!present) NA else {
    list(
      pk_markers = lapply(c("plasma", "serum"), function(s) list(
        gen_uuid = "bm-conc", name = "concentration", specimen_type = s,
        display_name = "concentration", type = "positive_continuous", unit = "mg/L"
      )),
      pd_markers = spec_dv_body()$pd_markers,
      time_bases = list(observed = list(
        pk_marker_eligibility = lapply(
          names(pk_pairs), function(s) spec_pair_elig("bm-conc", s, pk_pairs[[s]])
        ),
        pd_marker_eligibility = spec_dv_body()$time_bases$observed$pd_marker_eligibility,
        pd_all_specimens_eligibility = lapply(
          names(pd_all), function(m) c(list(gen_uuid = uuid_of[[m]]), spec_elig(pd_all[[m]]))
        ),
        time_intervals_of_interest = list(pk = list(), pd = list(),
                                          pk_all_specimens = list(), pd_all_specimens = list())
      ))
    )
  }
  list(
    data_version_id = "dv_1",
    recommended_pk_specimen = list(value = "plasma", reason = "most subjects"),
    marker_analysis = ma
  )
}

# nlmixr_mock() plus the marker-analysis route; every request is logged.
spec_mock <- function(log = new.env(), dv = spec_dv_body(),
                      marker_analysis = spec_marker_analysis_body(),
                      pk = spec_pk_body(), pd = spec_pd_body()) {
  inner <- nlmixr_mock(log, dv = dv, tables = list(
    subjects = nlmixr_subjects_body(), pk = pk, dosing = nlmixr_dosing_body(),
    pd = pd, covariates = nlmixr_covariates_body()
  ))
  function(req) {
    if (grepl("/data-versions/dv_1/marker-analysis$", sub("\\?.*$", "", req$url))) {
      log$requests <- c(log$requests, req$url)
      return(httr2::response_json(body = marker_analysis))
    }
    inner(req)
  }
}
