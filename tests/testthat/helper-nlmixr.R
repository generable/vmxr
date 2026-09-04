# Synthetic API 0.3 DataVersion + prepared tables for the nlmixr2 assembly tests.
#
# Three subjects on the "observed" basis:
#   S1 (u1): oral 100 mg at 0 h and 24 h; drug PK at 1, 4, 24 (BLQ, lloq 0.1) and
#            48 h (QC-excluded: after_qc FALSE, before_qc TRUE); a metabolite row.
#   S2 (u2): iv-infusion 50 mg over 2 h (dose_rate 25 mg/h); drug PK at 1 h and
#            2 h (ALQ, uloq 100).
#   S3 (u3): placebo — zero dose retained but ineligible; its PK row ineligible.
# PD: "effect" (continuous) on S1/S2, "resp" (binary) on S1.
# Covariates: weight (numeric) and sex (string) for all three.

nlmixr_col <- function(name, type, unit = NULL) {
  c(list(name = name, type = type), if (!is.null(unit)) list(unit = unit))
}

nlmixr_flags <- function(before = TRUE, after = before) {
  list(
    time_basis = "observed",
    eligible_for_modeling_before_qc = before,
    qc_excluded = before && !after,
    eligible_for_modeling_after_qc = after
  )
}

nlmixr_flag_cols <- function() {
  list(
    nlmixr_col("time_basis", "string"),
    nlmixr_col("eligible_for_modeling_before_qc", "boolean"),
    nlmixr_col("qc_excluded", "boolean"),
    nlmixr_col("eligible_for_modeling_after_qc", "boolean")
  )
}

nlmixr_marker_elig <- function(effect = TRUE, resp = TRUE) {
  list(
    list(gen_uuid = "mk-effect", eligible_for_modeling_before_qc = TRUE,
         eligible_for_modeling_after_qc = effect),
    list(gen_uuid = "mk-resp", eligible_for_modeling_before_qc = TRUE,
         eligible_for_modeling_after_qc = resp)
  )
}

nlmixr_dv_body <- function(recommended = list(value = "observed", reason = "only eligible basis"),
                           time_bases = list(
                             observed = list(available = TRUE, pd_marker_eligibility = nlmixr_marker_elig()),
                             nominal = list(available = FALSE, availability_reasons = list("no nominal times"))
                           ),
                           table_availability = list(
                             subjects = TRUE, pk = TRUE, dosing = TRUE, pd = TRUE,
                             labs = FALSE, covariates = TRUE, qc_issues = TRUE
                           )) {
  list(
    data_version_id = "dv_1",
    status = "ready",
    n_subjects = 3L,
    units = list(time = "h", pk_concentration = "mg/L", dose = "mg"),
    recommended_time_basis = recommended,
    time_bases = time_bases,
    table_availability = table_availability,
    pd_markers = list(
      list(gen_uuid = "mk-effect", name = "effect", display_name = "effect", type = "continuous", unit = "%"),
      list(gen_uuid = "mk-resp", name = "resp", display_name = "resp", type = "binary", unit = "1")
    )
  )
}

# A DataVersion as an API 0.2 server describes it: no time_bases map, no
# recommended basis, and tables served without eligibility flags or a basis echo.
nlmixr_legacy_dv_body <- function() {
  nlmixr_dv_body(recommended = NULL, time_bases = list())
}

nlmixr_subjects_body <- function() {
  row <- function(u, sid, arm, dg) {
    c(list(gen_subject_uuid = u, subject_id = sid, arm = arm, dose_group = dg), nlmixr_flags())
  }
  list(
    data_version_id = "dv_1", domain = "subjects", time_basis = "observed",
    columns = c(list(
      nlmixr_col("gen_subject_uuid", "string"), nlmixr_col("subject_id", "string"),
      nlmixr_col("arm", "string"), nlmixr_col("dose_group", "string")
    ), nlmixr_flag_cols()),
    rows = list(
      row("u3", "S3", "placebo", "0 mg"),
      row("u1", "S1", "active", "100 mg"),
      row("u2", "S2", "active", "50 mg iv")
    )
  )
}

nlmixr_pk_body <- function() {
  row <- function(u, sid, t, value, name, censoring = NULL, lloq = NULL, uloq = NULL,
                  before = TRUE, after = before) {
    c(list(
      gen_subject_uuid = u, subject_id = sid, observed_time_hours = t, time_hours = t,
      biomarker_name = name, biomarker_code = paste0(name, "_code"),
      value_float = value, unit = "mg/L", lloq = lloq, uloq = uloq, censoring = censoring
    ), nlmixr_flags(before, after))
  }
  list(
    data_version_id = "dv_1", domain = "pk", time_basis = "observed",
    columns = c(list(
      nlmixr_col("gen_subject_uuid", "string"), nlmixr_col("subject_id", "string"),
      nlmixr_col("observed_time_hours", "number"), nlmixr_col("time_hours", "number"),
      nlmixr_col("biomarker_name", "string"), nlmixr_col("biomarker_code", "string"),
      nlmixr_col("value_float", "number", "mg/L"), nlmixr_col("unit", "string"),
      nlmixr_col("lloq", "number"), nlmixr_col("uloq", "number"), nlmixr_col("censoring", "string")
    ), nlmixr_flag_cols()),
    rows = list(
      row("u1", "S1", 1, 10, "drug"),
      row("u1", "S1", 4, 5, "drug"),
      row("u1", "S1", 24, NULL, "drug", censoring = "BLQ", lloq = 0.1),
      row("u1", "S1", 48, 3, "drug", before = TRUE, after = FALSE),
      row("u1", "S1", 1, 2, "metabolite"),
      row("u2", "S2", 1, 20, "drug"),
      row("u2", "S2", 2, NULL, "drug", censoring = "ALQ", uloq = 100),
      row("u3", "S3", 1, 0.5, "drug", before = FALSE)
    )
  )
}

nlmixr_dosing_body <- function() {
  row <- function(u, sid, t, mass, route, rate = NULL, duration = NULL, before = TRUE, after = before) {
    c(list(
      gen_subject_uuid = u, subject_id = sid, observed_time_hours = t, time_hours = t,
      dose_amount = mass, dose_amount_unit = "mg", dose_mass = mass, dose_mass_unit = "mg",
      dose_rate = rate, dose_duration = duration, route = route
    ), nlmixr_flags(before, after))
  }
  list(
    data_version_id = "dv_1", domain = "dosing", time_basis = "observed",
    columns = c(list(
      nlmixr_col("gen_subject_uuid", "string"), nlmixr_col("subject_id", "string"),
      nlmixr_col("observed_time_hours", "number"), nlmixr_col("time_hours", "number"),
      nlmixr_col("dose_amount", "number", "mg"), nlmixr_col("dose_amount_unit", "string"),
      nlmixr_col("dose_mass", "number", "mg"), nlmixr_col("dose_mass_unit", "string"),
      nlmixr_col("dose_rate", "number", "mg/h"), nlmixr_col("dose_duration", "number", "h"),
      nlmixr_col("route", "string")
    ), nlmixr_flag_cols()),
    rows = list(
      row("u1", "S1", 0, 100, "po"),
      row("u1", "S1", 24, 100, "po"),
      row("u2", "S2", 0, 50, "iv-infusion", rate = 25, duration = 2),
      row("u3", "S3", 0, 0, "po", before = FALSE)
    )
  )
}

nlmixr_pd_body <- function() {
  row <- function(u, sid, t, name, vf = NULL, vi = NULL, unit = NULL) {
    c(list(
      gen_subject_uuid = u, subject_id = sid, observed_time_hours = t, time_hours = t,
      biomarker_name = name, value_float = vf, value_int = vi, unit = unit
    ), nlmixr_flags())
  }
  list(
    data_version_id = "dv_1", domain = "pd", time_basis = "observed",
    columns = c(list(
      nlmixr_col("gen_subject_uuid", "string"), nlmixr_col("subject_id", "string"),
      nlmixr_col("observed_time_hours", "number"), nlmixr_col("time_hours", "number"),
      nlmixr_col("biomarker_name", "string"), nlmixr_col("value_float", "number"),
      nlmixr_col("value_int", "integer"), nlmixr_col("unit", "string")
    ), nlmixr_flag_cols()),
    rows = list(
      row("u1", "S1", 1, "effect", vf = 50, unit = "%"),
      row("u2", "S2", 1, "effect", vf = 40, unit = "%"),
      row("u1", "S1", 1, "resp", vi = 1L, unit = "1")
    )
  )
}

nlmixr_covariates_body <- function() {
  row <- function(u, name, vf = NULL, vs = NULL, unit = NULL) {
    c(list(
      gen_subject_uuid = u, name = name, label = name,
      type = if (is.null(vs)) "positive_continuous" else "categorical",
      value_int = NULL, value_float = vf, value_str = vs, unit = unit
    ), nlmixr_flags())
  }
  list(
    data_version_id = "dv_1", domain = "covariates", time_basis = "observed",
    columns = c(list(
      nlmixr_col("gen_subject_uuid", "string"), nlmixr_col("name", "string"),
      nlmixr_col("label", "string"), nlmixr_col("type", "string"),
      nlmixr_col("value_int", "integer"), nlmixr_col("value_float", "number"),
      nlmixr_col("value_str", "string"), nlmixr_col("unit", "string")
    ), nlmixr_flag_cols()),
    rows = list(
      row("u1", "weight", vf = 70, unit = "kg"), row("u2", "weight", vf = 80, unit = "kg"),
      row("u3", "weight", vf = 60, unit = "kg"),
      row("u1", "sex", vs = "M"), row("u2", "sex", vs = "F"), row("u3", "sex", vs = "F")
    )
  )
}

# A mock server routing by URL; records every request in `log` (an environment).
nlmixr_mock <- function(log = new.env(),
                        dv = nlmixr_dv_body(),
                        tables = list(
                          subjects = nlmixr_subjects_body(), pk = nlmixr_pk_body(),
                          dosing = nlmixr_dosing_body(), pd = nlmixr_pd_body(),
                          covariates = nlmixr_covariates_body()
                        ),
                        echo = TRUE) {
  log$requests <- character()
  function(req) {
    log$requests <- c(log$requests, req$url)
    path <- sub("\\?.*$", "", req$url)
    if (grepl("/data-versions/dv_1$", path)) {
      return(httr2::response_json(body = dv))
    }
    domain <- sub("^.*/tables/", "", path)
    body <- tables[[domain]]
    if (is.null(body)) {
      return(httr2::response_json(status_code = 404, body = list(
        error = list(code = "not_found", reason = "table_unavailable", message = "no table")
      )))
    }
    # echo the requested basis like API 0.3; omit it like API 0.2 when none was sent
    basis <- if (grepl("time_basis=", req$url)) sub("^.*time_basis=([^&]+).*$", "\\1", req$url) else NULL
    # `echo = FALSE` models API 0.2.2 as deployed: the parameter is ignored, nothing echoed
    body$time_basis <- if (echo) basis else NULL
    httr2::response_json(body = body)
  }
}
