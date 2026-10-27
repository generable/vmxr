# vmx_nlmixr_data(): specimen-aware PK and PD selection on a hand-built
# two-specimen DataVersion (helper-specimens.R).

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

spec_obs <- function(ev, dvid = NULL) {
  obs <- ev[ev$EVID == 0L, ]
  if (!is.null(dvid)) obs <- obs[obs$DVID == dvid, ]
  obs
}

# ---- PK ----------------------------------------------------------------------

test_that("two PK specimens and no selection is an error that names them", {
  log <- new.env()
  httr2::local_mocked_responses(spec_mock(log))
  err <- expect_error(vmx_nlmixr_data("dv_1", client = con), class = "vmx_usage_error")
  expect_match(conditionMessage(err), "2 specimens (plasma, serum)", fixed = TRUE)
  expect_match(conditionMessage(err), "pk_specimen", fixed = TRUE)
  # the recommended specimen is never used as an implicit choice
  expect_false(any(grepl("marker-analysis", log$requests)))
})

test_that("one PK specimen keeps only its rows and records the scope", {
  log <- new.env()
  httr2::local_mocked_responses(spec_mock(log))
  ev <- vmx_nlmixr_data("dv_1", pk_specimen = "plasma", client = con)
  obs <- spec_obs(ev)
  expect_equal(sort(obs$DV), c(5, 10, 20))
  expect_true(all(obs$specimen_type == "plasma"))
  expect_true(all(is.na(ev$specimen_type[ev$EVID == 1L])))
  expect_identical(names(ev)[match("subject_id", names(ev)) + 1L], "specimen_type")
  meta <- attr(ev, "vmx")
  expect_equal(meta$pk_specimen, list(specimen_type = "plasma"))
  expect_equal(meta$endpoints$specimen_type, "plasma")
  expect_false(meta$endpoints$all_specimens)
  expect_equal(unname(meta$dropped[["pk_other_specimens"]]), 3L)
  expect_false("DVID" %in% names(ev))
  # the pair verdict was on the DataVersion; no marker-analysis read was needed
  expect_false(any(grepl("marker-analysis", log$requests)))
})

test_that("a PK specimen is admitted by its own recorded verdict", {
  dv <- spec_dv_body(pk_pairs = c(plasma = TRUE, serum = FALSE))
  httr2::local_mocked_responses(spec_mock(dv = dv))
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "serum", client = con),
    class = "vmx_usage_error", regexp = "specimen 'serum' is not eligible for modeling"
  )
  expect_s3_class(vmx_nlmixr_data("dv_1", pk_specimen = "plasma", client = con), "tbl_df")
  # the pooled verdict is a separate scope: still true here
  pooled <- vmx_nlmixr_data("dv_1", pk_specimen = vmx_all_specimens(), client = con)
  expect_equal(nrow(spec_obs(pooled)), 6L)
  # review mode reads the scope regardless of the verdict
  review <- suppressWarnings(
    vmx_nlmixr_data("dv_1", pk_specimen = "serum", eligibility = "all", client = con)
  )
  expect_true(all(spec_obs(review)$specimen_type == "serum"))
})

test_that("All specimens pools the PK specimens under the pooled verdict", {
  httr2::local_mocked_responses(spec_mock())
  ev <- vmx_nlmixr_data("dv_1", pk_specimen = vmx_all_specimens(), client = con)
  obs <- spec_obs(ev)
  expect_equal(nrow(obs), 6L)
  expect_setequal(obs$specimen_type, c("plasma", "serum"))   # each row keeps its specimen
  meta <- attr(ev, "vmx")
  expect_equal(meta$pk_specimen, list(all_specimens = TRUE))
  expect_true(meta$endpoints$all_specimens)
  expect_true(is.na(meta$endpoints$specimen_type))
  expect_equal(meta$units$pk_concentration, "mg/L")

  httr2::local_mocked_responses(spec_mock(dv = spec_dv_body(pk_pooled = FALSE)))
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = vmx_all_specimens(), client = con),
    class = "vmx_usage_error", regexp = "PK All specimens is not eligible for modeling"
  )
})

test_that("pooling PK specimens with different units is an error naming the units", {
  httr2::local_mocked_responses(spec_mock(pk = spec_pk_body(serum_unit = "ng/mL")))
  err <- expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = vmx_all_specimens(), client = con),
    class = "vmx_usage_error"
  )
  expect_match(conditionMessage(err), "found: mg/L, ng/mL", fixed = TRUE)
  # a single specimen is unaffected
  expect_s3_class(vmx_nlmixr_data("dv_1", pk_specimen = "plasma", client = con), "tbl_df")
})

test_that("an unrecorded PK specimen is refused with the recorded ones", {
  httr2::local_mocked_responses(spec_mock())
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "urine", client = con),
    class = "vmx_usage_error", regexp = "available: plasma, serum"
  )
  expect_error(vmx_nlmixr_data("dv_1", pk_specimen = 1, client = con), class = "vmx_usage_error")
  expect_error(vmx_nlmixr_data("dv_1", pk_specimen = c("plasma", "serum"), client = con),
               class = "vmx_usage_error")
})

test_that("without a root pk_marker_eligibility the marker-analysis verdict is used", {
  log <- new.env()
  dv <- spec_dv_body(pk_marker = FALSE)
  httr2::local_mocked_responses(spec_mock(
    log, dv = dv, marker_analysis = spec_marker_analysis_body(pk_pairs = c(plasma = FALSE, serum = TRUE))
  ))
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "plasma", client = con),
    class = "vmx_usage_error", regexp = "not eligible"
  )
  expect_true(any(grepl("/data-versions/dv_1/marker-analysis", log$requests)))
  ev <- vmx_nlmixr_data("dv_1", pk_specimen = "serum", client = con)
  expect_true(all(spec_obs(ev)$specimen_type == "serum"))

  # no marker-analysis metadata: only All specimens can be selected for modeling
  httr2::local_mocked_responses(spec_mock(
    dv = dv, marker_analysis = spec_marker_analysis_body(present = FALSE)
  ))
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "plasma", client = con),
    class = "vmx_usage_error", regexp = "no single-specimen PK verdicts"
  )
  expect_s3_class(
    vmx_nlmixr_data("dv_1", pk_specimen = vmx_all_specimens(), client = con), "tbl_df"
  )
})

# ---- PD ----------------------------------------------------------------------

test_that("a PD marker with two specimens and no specimen named is an error naming them", {
  httr2::local_mocked_responses(spec_mock())
  err <- expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "plasma", pd_markers = "effect", client = con),
    class = "vmx_usage_error"
  )
  expect_match(conditionMessage(err), "'effect' records 2 specimens (plasma, serum)", fixed = TRUE)
  # a single-specimen marker needs no specimen
  ev <- vmx_nlmixr_data("dv_1", pk_specimen = "plasma", pd_markers = "resp", client = con)
  expect_equal(attr(ev, "vmx")$endpoints$specimen_type, c("plasma", "plasma"))
})

test_that("each PD marker-and-specimen pair is its own endpoint", {
  httr2::local_mocked_responses(spec_mock())
  ev <- vmx_nlmixr_data(
    "dv_1", pk_specimen = "plasma",
    pd_markers = list(effect = c("plasma", "serum"), resp = "plasma"), client = con
  )
  ends <- attr(ev, "vmx")$endpoints
  expect_equal(ends$dvid, 1:4)
  expect_equal(ends$name, c("concentration", "effect", "effect", "resp"))
  expect_equal(ends$specimen_type, c("plasma", "plasma", "serum", "plasma"))
  expect_equal(ends$all_specimens, rep(FALSE, 4))
  expect_equal(ends$cmt, c(2L, 3L, 4L, 5L))
  expect_equal(ends$unit, c("mg/L", "%", "%", "1"))
  expect_equal(sort(spec_obs(ev, 2L)$DV), c(40, 50))
  expect_equal(spec_obs(ev, 3L)$DV, 45)
  expect_true(all(spec_obs(ev, 3L)$specimen_type == "serum"))
  expect_true(all(spec_obs(ev, 3L)$CMT == 4L))
})

test_that("pd_markers = TRUE gives every eligible pair its own endpoint, never pooled", {
  httr2::local_mocked_responses(spec_mock())
  ev <- vmx_nlmixr_data("dv_1", pk_specimen = "plasma", pd_markers = TRUE, client = con)
  ends <- attr(ev, "vmx")$endpoints
  expect_equal(paste(ends$name, ends$specimen_type)[-1],
               c("effect plasma", "effect serum", "resp plasma", "titer plasma", "titer serum"))
  expect_false(any(ends$all_specimens))
  expect_equal(ends$unit[ends$name == "titer"], c("ng/mL", "mg/L"))
})

test_that("PD eligibility is looked up by marker and specimen, not marker uuid alone", {
  # effect/plasma eligible, effect/serum not: a uuid-keyed lookup would let the
  # serum verdict overwrite the plasma one.
  dv <- spec_dv_body(pd_pairs = c(`effect/plasma` = TRUE, `effect/serum` = FALSE,
                                  `resp/plasma` = TRUE, `titer/plasma` = TRUE,
                                  `titer/serum` = TRUE))
  httr2::local_mocked_responses(spec_mock(dv = dv))
  ok <- vmx_nlmixr_data("dv_1", pk_specimen = "plasma",
                        pd_markers = list(effect = "plasma"), client = con)
  expect_equal(attr(ok, "vmx")$endpoints$specimen_type, c("plasma", "plasma"))
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "plasma", pd_markers = list(effect = "serum"), client = con),
    class = "vmx_usage_error", regexp = "not eligible for modeling on basis 'observed' \\(effect \\(serum\\)\\)"
  )
  all_pairs <- vmx_nlmixr_data("dv_1", pk_specimen = "plasma", pd_markers = TRUE, client = con)
  ends <- attr(all_pairs, "vmx")$endpoints
  expect_false(any(ends$name == "effect" & ends$specimen_type %in% "serum"))
  expect_true(any(ends$name == "effect" & ends$specimen_type %in% "plasma"))
  expect_equal(unname(attr(all_pairs, "vmx")$dropped[["pd_markers_ineligible"]]), 1L)
})

test_that("a pooled PD marker is one endpoint under its All-specimens verdict", {
  log <- new.env()
  httr2::local_mocked_responses(spec_mock(log))
  ev <- vmx_nlmixr_data("dv_1", pk_specimen = "plasma",
                        pd_markers = list(effect = vmx_all_specimens()), client = con)
  ends <- attr(ev, "vmx")$endpoints
  expect_equal(nrow(ends), 2L)
  expect_true(ends$all_specimens[[2]])
  expect_true(is.na(ends$specimen_type[[2]]))
  expect_equal(ends$unit[[2]], "%")
  obs <- spec_obs(ev, 2L)
  expect_equal(nrow(obs), 3L)
  expect_setequal(obs$specimen_type, c("plasma", "serum"))
  expect_true(any(grepl("marker-analysis", log$requests)))

  ma <- spec_marker_analysis_body(pd_all = c(effect = FALSE, resp = TRUE, titer = FALSE))
  httr2::local_mocked_responses(spec_mock(marker_analysis = ma))
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "plasma",
                    pd_markers = list(effect = vmx_all_specimens()), client = con),
    class = "vmx_usage_error", regexp = "effect \\(All specimens\\)"
  )
})

test_that("without marker-analysis metadata a pooled PD marker uses its recorded pair verdicts", {
  none <- spec_marker_analysis_body(present = FALSE)
  httr2::local_mocked_responses(spec_mock(marker_analysis = none))
  ev <- vmx_nlmixr_data("dv_1", pk_specimen = "plasma",
                        pd_markers = list(effect = vmx_all_specimens()), client = con)
  expect_true(attr(ev, "vmx")$endpoints$all_specimens[[2]])

  dv <- spec_dv_body(pd_pairs = c(`effect/plasma` = FALSE, `effect/serum` = FALSE,
                                  `resp/plasma` = TRUE, `titer/plasma` = TRUE,
                                  `titer/serum` = TRUE))
  httr2::local_mocked_responses(spec_mock(dv = dv, marker_analysis = none))
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "plasma",
                    pd_markers = list(effect = vmx_all_specimens()), client = con),
    class = "vmx_usage_error", regexp = "not eligible"
  )
})

test_that("pooling PD specimens with different units is an error naming the units", {
  # recorded pooled verdict true, so the unit requirement is what refuses it
  ma <- spec_marker_analysis_body(pd_all = c(effect = TRUE, resp = TRUE, titer = TRUE))
  httr2::local_mocked_responses(spec_mock(marker_analysis = ma))
  err <- expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "plasma",
                    pd_markers = list(titer = vmx_all_specimens()), client = con),
    class = "vmx_usage_error"
  )
  expect_match(conditionMessage(err), "PD marker 'titer'", fixed = TRUE)
  expect_match(conditionMessage(err), "found: mg/L, ng/mL", fixed = TRUE)
  # each specimen on its own is fine
  ev <- vmx_nlmixr_data("dv_1", pk_specimen = "plasma",
                        pd_markers = list(titer = c("plasma", "serum")), client = con)
  expect_equal(attr(ev, "vmx")$endpoints$unit[2:3], c("ng/mL", "mg/L"))
})

test_that("PD selections are validated", {
  httr2::local_mocked_responses(spec_mock())
  run <- function(pd) vmx_nlmixr_data("dv_1", pk_specimen = "plasma", pd_markers = pd, client = con)
  expect_error(run(list(effect = list(vmx_all_specimens(), "plasma"))),
               class = "vmx_usage_error", regexp = "both pooled and by specimen")
  expect_error(run(list(effect = "urine")), class = "vmx_usage_error",
               regexp = "no specimen 'urine' \\(available: plasma, serum\\)")
  expect_error(run(list("plasma")), class = "vmx_usage_error")
  expect_error(run(list(effect = 1)), class = "vmx_usage_error")
  expect_error(run(vmx_all_specimens()), class = "vmx_usage_error")
  expect_error(run(list(nope = "plasma")), class = "vmx_usage_error", regexp = "not in the `pd` table")
})

# ---- DataVersions that predate specimens ---------------------------------------

test_that("a DataVersion without specimen_type keeps the marker-level layout", {
  log <- new.env()
  httr2::local_mocked_responses(nlmixr_mock(log))
  ev <- vmx_nlmixr_data("dv_1", analyte = "drug", pd_markers = TRUE, client = con)
  expect_false("specimen_type" %in% names(ev))
  meta <- attr(ev, "vmx")
  expect_null(meta$pk_specimen)
  expect_identical(names(meta$endpoints), c("dvid", "name", "cmt", "unit", "has_repeated_times"))
  expect_false("pk_other_specimens" %in% names(meta$dropped))
  expect_false(any(grepl("marker-analysis", log$requests)))
  # naming a specimen the data does not record is refused
  expect_error(
    vmx_nlmixr_data("dv_1", analyte = "drug", pk_specimen = "plasma", client = con),
    class = "vmx_usage_error", regexp = "available: none recorded"
  )
})

test_that("vmx_pd_marker() selections name PD markers by uuid and specimen", {
  httr2::local_mocked_responses(spec_mock())
  ev <- vmx_nlmixr_data(
    "dv_1", pk_specimen = "plasma",
    pd_markers = list(
      vmx_pd_marker("bm-effect", "serum", "decreasing"),
      vmx_pd_marker("bm-resp", vmx_all_specimens())
    ),
    client = con
  )
  ends <- attr(ev, "vmx")$endpoints
  expect_equal(ends$name, c("concentration", "effect", "resp"))
  expect_equal(ends$specimen_type, c("plasma", "serum", NA))
  expect_equal(ends$all_specimens, c(FALSE, FALSE, TRUE))
  one <- vmx_nlmixr_data("dv_1", pk_specimen = "plasma",
                         pd_markers = vmx_pd_marker("bm-effect", "plasma"), client = con)
  expect_equal(attr(one, "vmx")$endpoints$specimen_type, c("plasma", "plasma"))
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "plasma",
                    pd_markers = vmx_pd_marker("bm-nope", "plasma"), client = con),
    class = "vmx_usage_error", regexp = "not in the DataVersion's `pd_markers`"
  )
  expect_error(
    vmx_nlmixr_data("dv_1", pk_specimen = "plasma", client = con, pd_markers = list(
      vmx_pd_marker("bm-effect", "plasma"), vmx_pd_marker("bm-effect", vmx_all_specimens())
    )),
    class = "vmx_usage_error"
  )
})

test_that("a specimen-aware dataset with the specimen_type column is accepted by rxode2", {
  skip_if_not_installed("rxode2")
  skip_if(!identical(Sys.getenv("VMXR_TEST_RXODE2"), "true"),
          "set VMXR_TEST_RXODE2=true to compile an rxode2 model")
  httr2::local_mocked_responses(spec_mock())
  ev <- vmx_nlmixr_data("dv_1", pk_specimen = vmx_all_specimens(), client = con)
  expect_true("specimen_type" %in% names(ev))
  mod <- rxode2::rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(central) <- ka * depot - cl / v * central
    cp <- central / v
  })
  sol <- rxode2::rxSolve(mod, c(ka = 1, cl = 5, v = 50), as.data.frame(ev))
  expect_true(nrow(sol) > 0)
  expect_true(all(is.finite(sol$cp)))
})
