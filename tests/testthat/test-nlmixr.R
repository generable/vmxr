# vmx_nlmixr_data(): NONMEM-layout assembly against the API 0.3 table shapes.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

nlmixr_core_cols <- c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "CMT", "RATE", "II",
                      "ADDL", "SS", "CENS", "LIMIT", "subject_id")

test_that("assembles doses and eligible PK observations in NONMEM layout", {
  log <- new.env()
  httr2::local_mocked_responses(nlmixr_mock(log))
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)

  expect_s3_class(ev, "tbl_df")
  expect_identical(names(ev)[seq_along(nlmixr_core_cols)], nlmixr_core_cols)
  expect_false("DVID" %in% names(ev))                   # single endpoint -> no DVID column
  # every table request carried the recommended basis
  table_urls <- grep("/tables/", log$requests, value = TRUE)
  expect_true(length(table_urls) >= 5)
  expect_true(all(grepl("time_basis=observed", table_urls)))

  # 5 eligible drug observations + 3 eligible doses; S3 (placebo) absent entirely
  expect_equal(nrow(ev), 8L)
  expect_equal(sum(ev$EVID == 0L), 5L)
  expect_equal(sum(ev$EVID == 1L), 3L)
  expect_equal(sort(unique(ev$ID)), c(1L, 2L))
  expect_equal(unique(ev$subject_id[ev$ID == 1L]), "S1")
  expect_equal(unique(ev$subject_id[ev$ID == 2L]), "S2")
  expect_false(any(ev$DV %in% 3))                       # QC-excluded row dropped
  expect_false(any(ev$AMT %in% 0 & ev$EVID == 1L))      # zero-dose placebo row dropped

  meta <- attr(ev, "vmx")
  expect_equal(meta$data_version_id, "dv_1")
  expect_equal(meta$time_basis, "observed")
  expect_equal(meta$analyte, "drug")
  expect_equal(meta$eligibility, "after_qc")
  expect_equal(meta$n_subjects, 2L)
  expect_equal(meta$units$pk_concentration, "mg/L")
  expect_equal(unname(meta$dropped[c("pk_ineligible", "pk_other_analytes", "dosing_ineligible")]),
               c(2L, 1L, 1L))
})

test_that("BLQ and ALQ rows are encoded with CENS/LIMIT, never dropped", {
  httr2::local_mocked_responses(nlmixr_mock())
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)
  blq <- ev[ev$ID == 1L & ev$EVID == 0L & ev$TIME == 24, ]
  expect_equal(nrow(blq), 1L)
  expect_equal(blq$DV, 0.1)
  expect_equal(blq$CENS, 1L)
  expect_equal(blq$LIMIT, 0)
  alq <- ev[ev$ID == 2L & ev$EVID == 0L & ev$TIME == 2, ]
  expect_equal(alq$DV, 100)
  expect_equal(alq$CENS, -1L)
  expect_equal(alq$LIMIT, Inf)
  expect_true(all(ev$CENS[ev$EVID == 1L] == 0L))
})

test_that("doses map route to compartment and infusions carry RATE", {
  httr2::local_mocked_responses(nlmixr_mock())
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)
  oral <- ev[ev$ID == 1L & ev$EVID == 1L, ]
  expect_equal(oral$TIME, c(0, 24))
  expect_true(all(oral$CMT == 1L))
  expect_true(all(oral$AMT == 100))
  expect_true(all(oral$RATE == 0))
  expect_true(all(oral$MDV == 1L))
  expect_true(all(is.na(oral$DV)))
  inf <- ev[ev$ID == 2L & ev$EVID == 1L, ]
  expect_equal(inf$CMT, 2L)
  expect_equal(inf$AMT, 50)
  expect_equal(inf$RATE, 25)
  expect_true(all(ev$CMT[ev$EVID == 0L] == 2L))
  expect_true(all(ev$II == 0 & ev$ADDL == 0L & ev$SS == 0L))
})

test_that("rows are ordered by subject, time, then dose before observation at ties", {
  httr2::local_mocked_responses(nlmixr_mock())
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)
  expect_false(is.unsorted(ev$ID))
  s1 <- ev[ev$ID == 1L, ]
  expect_false(is.unsorted(s1$TIME))
  at24 <- s1[s1$TIME == 24, ]
  expect_equal(at24$EVID, c(1L, 0L))                   # matches rxode2's tie resolution
})

test_that("IDs are dense over the subjects that have admitted rows", {
  # make the placebo subject sort in the middle so a gap would show
  subjects <- nlmixr_subjects_body()
  subjects$rows[[1]]$subject_id <- "S15"                 # u3 (all rows ineligible)
  subjects$rows[[2]]$subject_id <- "S1"
  subjects$rows[[3]]$subject_id <- "S20"
  httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = subjects, pk = nlmixr_pk_body(), dosing = nlmixr_dosing_body(),
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)
  expect_equal(sort(unique(ev$ID)), 1:2)
  expect_equal(unique(ev$subject_id[ev$ID == 2L]), "S20")
})

test_that("no admitted rows is a usage error, not a crash", {
  pk <- nlmixr_pk_body()
  pk$rows <- lapply(pk$rows, function(r) { r$eligible_for_modeling_after_qc <- FALSE; r })
  dv <- nlmixr_dv_body()
  dv$time_bases$observed$pk_modeling_eligibility <- list(
    eligible_for_modeling_before_qc = TRUE, eligible_for_modeling_after_qc = FALSE
  )
  httr2::local_mocked_responses(nlmixr_mock(dv = dv, tables = list(
    subjects = nlmixr_subjects_body(), pk = pk, dosing = nlmixr_dosing_body(),
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
  expect_error(vmx_nlmixr_data("dv_1", client = con),                 # nothing admitted at all
               class = "vmx_usage_error", regexp = "not eligible for modeling")
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con),
               class = "vmx_usage_error", regexp = "no rows admitted")
  # ... and the review mode still assembles them
  ev <- suppressWarnings(vmx_nlmixr_data("dv_1", analyte = "drug", eligibility = "all", client = con))
  expect_true(nrow(ev) > 0)
})

test_that("an analyte match may not span two analytes", {
  pk <- nlmixr_pk_body()
  # make the metabolite's code equal the drug's name
  pk$rows[[5]]$biomarker_code <- "drug"
  httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = nlmixr_subjects_body(), pk = pk, dosing = nlmixr_dosing_body(),
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
  by_name <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)   # name match wins
  expect_equal(attr(by_name, "vmx")$analyte, "drug")
  expect_equal(sum(by_name$EVID == 0L), 5L)
  pk$rows[[1]]$biomarker_code <- "shared"
  pk$rows[[5]]$biomarker_code <- "shared"
  httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = nlmixr_subjects_body(), pk = pk, dosing = nlmixr_dosing_body(),
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
  expect_error(vmx_nlmixr_data("dv_1", analyte = "shared", client = con),
               class = "vmx_usage_error", regexp = "matches 2 analytes")
})

test_that("covariates are typed by the served type, including binary via value_int", {
  cov <- nlmixr_covariates_body()
  bin <- function(u, v) c(list(gen_subject_uuid = u, name = "smoker", label = "smoker", type = "binary",
                               value_int = v, value_float = NULL, value_str = NULL, unit = NULL), nlmixr_flags())
  cov$rows <- c(cov$rows, list(bin("u1", 1L), bin("u2", 0L), bin("u3", 0L)))
  httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = nlmixr_subjects_body(), pk = nlmixr_pk_body(), dosing = nlmixr_dosing_body(),
    pd = nlmixr_pd_body(), covariates = cov
  )))
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)
  expect_type(ev$smoker, "double")
  expect_equal(unique(ev$smoker[ev$ID == 1L]), 1)
  expect_equal(unique(ev$smoker[ev$ID == 2L]), 0)
  expect_type(ev$sex, "character")
})

test_that("covariates and subject descriptors are joined wide by subject", {
  httr2::local_mocked_responses(nlmixr_mock())
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)
  expect_true(all(c("weight", "sex", "arm", "dose_group") %in% names(ev)))
  expect_equal(unique(ev$weight[ev$ID == 1L]), 70)
  expect_equal(unique(ev$weight[ev$ID == 2L]), 80)
  expect_equal(unique(ev$sex[ev$ID == 2L]), "F")
  expect_equal(unique(ev$dose_group[ev$ID == 2L]), "50 mg iv")
  expect_type(ev$weight, "double")
  expect_type(ev$sex, "character")
})

test_that("analyte selection is explicit when several analytes are present", {
  httr2::local_mocked_responses(nlmixr_mock())
  expect_error(vmx_nlmixr_data("dv_1", client = con), class = "vmx_usage_error")
  expect_error(vmx_nlmixr_data("dv_1", analyte = "nope", client = con), class = "vmx_usage_error")
  met <- vmx_nlmixr_data("dv_1", analyte = "metabolite", client = con)
  expect_equal(sum(met$EVID == 0L), 1L)
  expect_equal(attr(met, "vmx")$analyte, "metabolite")
  by_code <- vmx_nlmixr_data("dv_1", analyte = "drug_code", client = con)
  expect_equal(sum(by_code$EVID == 0L), 5L)
})

test_that("the eligibility flag is selectable; review modes warn and are not analysis-ready", {
  httr2::local_mocked_responses(nlmixr_mock())
  expect_warning(
    before <- vmx_nlmixr_data("dv_1", analyte = "drug", eligibility = "before_qc", client = con),
    "review only"
  )
  expect_true(any(before$DV %in% 3))                    # QC-excluded row admitted before QC
  expect_equal(unname(attr(before, "vmx")$dropped[["pk_ineligible"]]), 1L)
  expect_false(attr(before, "vmx")$analysis_ready)
  all_rows <- suppressWarnings(vmx_nlmixr_data("dv_1", analyte = "drug", eligibility = "all", client = con))
  expect_equal(sort(unique(all_rows$ID)), 1:3)
  expect_true(any(all_rows$AMT == 0 & all_rows$EVID == 1L))
  expect_equal(attr(all_rows, "vmx")$eligibility, "all")
  expect_false(attr(all_rows, "vmx")$analysis_ready)
  strict <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)
  expect_true(attr(strict, "vmx")$analysis_ready)
})

test_that("review modes drop unencodable retained-ineligible rows with a count", {
  dosing <- nlmixr_dosing_body()
  dosing$rows[[4]]$route <- "topical"              # ineligible placebo row, non-vocabulary route
  dosing$rows[[3]]["dose_rate"] <- list(NULL)      # infusion without canonical rate (null cell)
  dosing$rows[[3]]$eligible_for_modeling_after_qc <- FALSE
  dosing$rows[[3]]$eligible_for_modeling_before_qc <- FALSE
  httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = nlmixr_subjects_body(), pk = nlmixr_pk_body(), dosing = dosing,
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
  strict <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)
  expect_equal(sum(strict$EVID == 1L), 2L)          # both bad rows are ineligible -> filtered
  all_rows <- suppressWarnings(vmx_nlmixr_data("dv_1", analyte = "drug", eligibility = "all", client = con))
  expect_equal(sum(all_rows$EVID == 1L), 2L)        # ... and under `all` dropped as unencodable
  expect_equal(unname(attr(all_rows, "vmx")$dropped[["dosing_unencodable"]]), 2L)
})

test_that("PD markers become extra endpoints with their own CMT and DVID", {
  httr2::local_mocked_responses(nlmixr_mock())
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", pd_markers = TRUE, client = con)
  ends <- attr(ev, "vmx")$endpoints
  expect_equal(ends$dvid, 1:3)
  expect_equal(ends$name, c("drug", "effect", "resp"))
  expect_equal(ends$cmt, c(2L, 3L, 4L))
  expect_equal(ends$unit, c("mg/L", "%", "1"))
  expect_true("DVID" %in% names(ev))
  expect_identical(names(ev)[14:15], c("DVID", "subject_id"))
  expect_equal(sum(ev$DVID == 2L), 2L)
  expect_equal(sum(ev$DVID == 3L), 1L)
  expect_equal(ev$CMT[ev$DVID == 3L], 4L)
  expect_equal(ev$DV[ev$DVID == 3L], 1)
  expect_true(all(ev$DVID[ev$EVID == 1L] == 1L))       # dose rows share the PK endpoint id
  expect_setequal(unique(ev$DVID), 1:3)
  one <- vmx_nlmixr_data("dv_1", analyte = "drug", pd_markers = "effect", client = con)
  expect_equal(attr(one, "vmx")$endpoints$name, c("drug", "effect"))
  expect_error(
    vmx_nlmixr_data("dv_1", analyte = "drug", pd_markers = "nope", client = con),
    class = "vmx_usage_error"
  )
})

test_that("a PD marker the DataVersion marks ineligible on the basis is excluded", {
  dv <- nlmixr_dv_body(time_bases = list(
    observed = list(available = TRUE, pd_marker_eligibility = nlmixr_marker_elig(resp = FALSE)),
    nominal = list(available = FALSE)
  ))
  httr2::local_mocked_responses(nlmixr_mock(dv = dv))
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", pd_markers = TRUE, client = con)
  expect_equal(attr(ev, "vmx")$endpoints$name, c("drug", "effect"))
  expect_equal(unname(attr(ev, "vmx")$dropped[["pd_markers_ineligible"]]), 1L)
  expect_error(
    vmx_nlmixr_data("dv_1", analyte = "drug", pd_markers = "resp", client = con),
    class = "vmx_usage_error"
  )
  # before-QC review admits it again
  before <- suppressWarnings(vmx_nlmixr_data("dv_1", analyte = "drug", pd_markers = TRUE,
                                             eligibility = "before_qc", client = con))
  expect_equal(attr(before, "vmx")$endpoints$name, c("drug", "effect", "resp"))
})

test_that("an analytical row whose subject is missing from `subjects` is a served-contract violation", {
  pk <- nlmixr_pk_body()
  pk$rows[[1]]$gen_subject_uuid <- "u-orphan"
  httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = nlmixr_subjects_body(), pk = pk, dosing = nlmixr_dosing_body(),
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con),
               class = "vmx_response_error", regexp = "absent from the `subjects` table")
})

test_that("the compartment map can be overridden", {
  httr2::local_mocked_responses(nlmixr_mock())
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", cmt = c(po = 3L, observation = 5L), client = con)
  expect_true(all(ev$CMT[ev$EVID == 1L & ev$ID == 1L] == 3L))
  expect_true(all(ev$CMT[ev$EVID == 0L] == 5L))
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", cmt = c(bogus = 1L), client = con),
               class = "vmx_usage_error")
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", cmt = c(po = 0L), client = con),
               class = "vmx_usage_error")
})

test_that("an explicit time basis is honoured and an unavailable one refused", {
  log <- new.env()
  httr2::local_mocked_responses(nlmixr_mock(log))
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", time_basis = "observed", client = con)
  expect_equal(attr(ev, "vmx")$time_basis, "observed")
  expect_error(
    vmx_nlmixr_data("dv_1", analyte = "drug", time_basis = "nominal", client = con),
    class = "vmx_usage_error"
  )
  expect_error(
    vmx_nlmixr_data("dv_1", analyte = "drug", time_basis = "bogus", client = con),
    class = "vmx_usage_error"
  )
})

test_that("contract violations in the served tables fail loudly", {
  # an admitted infusion without the canonical rate (duration is never used to fill it)
  dosing <- nlmixr_dosing_body()
  dosing$rows[[3]]["dose_rate"] <- list(NULL)
  httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = nlmixr_subjects_body(), pk = nlmixr_pk_body(), dosing = dosing,
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con),
               class = "vmx_response_error", regexp = "dose_rate")

  # BLQ without an lloq
  pk <- nlmixr_pk_body()
  pk$rows[[3]]["lloq"] <- list(NULL)
  httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = nlmixr_subjects_body(), pk = pk, dosing = nlmixr_dosing_body(),
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con),
               class = "vmx_response_error", regexp = "lloq")

  # a served basis that differs from the requested one
  httr2::local_mocked_responses(function(req) {
    resp <- nlmixr_mock()(req)
    body <- httr2::resp_body_json(resp)
    if (!is.null(body$domain)) body$time_basis <- "nominal"
    httr2::response_json(body = body)
  })
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con), class = "vmx_response_error")

  # a v0.3 server that does not echo the basis at all
  httr2::local_mocked_responses(function(req) {
    resp <- nlmixr_mock()(req)
    body <- httr2::resp_body_json(resp)
    if (!is.null(body$domain)) body$time_basis <- NULL
    httr2::response_json(body = body)
  })
  expect_error(vmx_pk("dv_1", client = con), class = "vmx_response_error")
})

test_that("a v0.3 DataVersion with no recommended basis needs an explicit one", {
  dv <- nlmixr_dv_body(recommended = NULL)
  log <- new.env()
  httr2::local_mocked_responses(nlmixr_mock(log, dv = dv))
  expect_error(vmx_pk("dv_1", client = con), class = "vmx_usage_error")
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con), class = "vmx_usage_error")
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", time_basis = "observed", client = con)
  expect_equal(attr(ev, "vmx")$time_basis, "observed")
})

test_that("a pre-0.3 table without eligibility flags needs eligibility = 'all'", {
  strip <- function(body) {
    body$columns <- Filter(function(c) !c$name %in% c("eligible_for_modeling_before_qc", "qc_excluded", "eligible_for_modeling_after_qc"), body$columns)
    body$rows <- lapply(body$rows, function(r) {
      r[c("eligible_for_modeling_before_qc", "qc_excluded", "eligible_for_modeling_after_qc")] <- NULL
      r
    })
    body
  }
  tables <- list(
    subjects = strip(nlmixr_subjects_body()), pk = strip(nlmixr_pk_body()),
    dosing = strip(nlmixr_dosing_body()), pd = strip(nlmixr_pd_body()),
    covariates = strip(nlmixr_covariates_body())
  )
  # an API 0.2 server: no time_bases map, tables served without a basis echo
  httr2::local_mocked_responses(nlmixr_mock(dv = nlmixr_legacy_dv_body(), tables = tables))
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con), class = "vmx_usage_error")
  ev <- suppressWarnings(vmx_nlmixr_data("dv_1", analyte = "drug", eligibility = "all", client = con))
  expect_equal(sort(unique(ev$ID)), 1:3)
  expect_null(attr(ev, "vmx")$time_basis)

  # a v0.3 table (basis echoed) missing the flags is a served-contract violation, not a usage error
  httr2::local_mocked_responses(nlmixr_mock(tables = tables))
  expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con), class = "vmx_response_error")
})

test_that("vmx_data_version_table resolves the basis from a bare id and echoes it", {
  log <- new.env()
  httr2::local_mocked_responses(nlmixr_mock(log))
  tbl <- vmx_pk("dv_1", client = con)
  expect_equal(attr(tbl, "time_basis"), "observed")
  expect_match(log$requests[[1]], "/data-versions/dv_1$")
  expect_match(log$requests[[2]], "/tables/pk\\?time_basis=observed$")

  # explicit basis skips the DataVersion fetch
  log2 <- new.env()
  httr2::local_mocked_responses(nlmixr_mock(log2))
  vmx_pk("dv_1", time_basis = "observed", client = con)
  expect_length(log2$requests, 1L)
  expect_match(log2$requests[[1]], "/tables/pk\\?time_basis=observed$")
})

test_that("an API 0.2 DataVersion (no time_bases map) is fetched without the parameter", {
  log <- new.env()
  httr2::local_mocked_responses(nlmixr_mock(log, dv = nlmixr_legacy_dv_body()))
  tbl <- vmx_pk("dv_1", client = con)
  expect_null(attr(tbl, "time_basis"))
  expect_false(grepl("time_basis", log$requests[[2]]))
  # a bare-string recommendation (API 0.2 shape) still works
  log2 <- new.env()
  httr2::local_mocked_responses(nlmixr_mock(log2, dv = nlmixr_dv_body(recommended = "observed")))
  vmx_pk("dv_1", client = con)
  expect_match(log2$requests[[2]], "time_basis=observed$")
})

test_that("vmx_model_data carries the basis and the covariates table", {
  httr2::local_mocked_responses(nlmixr_mock())
  md <- vmx_model_data("dv_1", client = con)
  expect_equal(md$meta$time_basis, "observed")
  expect_s3_class(md$covariates, "tbl_df")
  expect_null(md$labs)
  expect_equal(attr(md$pk, "time_basis"), "observed")
})

test_that("the assembled events are accepted by rxode2", {
  skip_if_not_installed("rxode2")
  skip_if(!identical(Sys.getenv("VMXR_TEST_RXODE2"), "true"),
          "set VMXR_TEST_RXODE2=true to compile an rxode2 model")
  httr2::local_mocked_responses(nlmixr_mock())
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", client = con)
  mod <- rxode2::rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(central) <- ka * depot - cl / v * central
    cp <- central / v
  })
  sol <- rxode2::rxSolve(mod, c(ka = 1, cl = 5, v = 50), as.data.frame(ev))
  expect_true(nrow(sol) > 0)
  expect_true(all(is.finite(sol$cp)))
})

test_that("a recorded API 0.3 DataVersion fits a one-compartment model in nlmixr2", {
  skip_if_not_installed("nlmixr2est")
  skip_if(!identical(Sys.getenv("VMXR_TEST_NLMIXR2"), "true"),
          "set VMXR_TEST_NLMIXR2=true to run the nlmixr2 end-to-end fit")
  fixture <- function(name) {
    jsonlite::fromJSON(test_path("fixtures", "staging-dv", paste0(name, ".json")), simplifyVector = FALSE)
  }
  dv_body <- fixture("dv")
  httr2::local_mocked_responses(function(req) {
    path <- sub("\\?.*$", "", req$url)
    if (grepl("/data-versions/[^/]+$", path)) return(httr2::response_json(body = dv_body))
    domain <- sub("^.*/tables/", "", path)
    httr2::response_json(body = fixture(domain))
  })
  ev <- vmx_nlmixr_data(dv_body$data_version_id, client = con)
  expect_equal(attr(ev, "vmx")$n_subjects, 11L)      # one subject is ineligible on this basis
  one_cmt <- function() {
    ini({
      tka <- log(1); tcl <- log(3); tv <- log(70)
      eta.ka ~ 0.3; eta.cl ~ 0.1; eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      cp <- central / v
      cp ~ add(add.sd)
    })
  }
  fit <- suppressMessages(nlmixr2est::nlmixr2(one_cmt, as.data.frame(ev), est = "focei",
                                              control = nlmixr2est::foceiControl(print = 0)))
  expect_s3_class(fit, "nlmixr2FitCore")
  expect_true(is.finite(fit$objf))
})
