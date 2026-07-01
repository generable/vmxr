# Analysis-log verb + obs-vs-pred reshape, using httr2 mocks.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

test_that("vmx_analysis_log paginates and forwards filters", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = list(
      study_id = "std_1",
      items = list(list(kind = "event", event_type = "nca.completed", outcome = "success")),
      next_cursor = NULL
    ))
  })
  tbl <- vmx_analysis_log("std_1", kind = "event",
                          since = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
                          client = con)
  expect_equal(nrow(tbl), 1L)
  expect_match(env$req$url, "/studies/std_1/analysis-log")
  expect_match(env$req$url, "kind=event")
  expect_match(env$req$url, "since=2026-01-01")   # POSIXct -> ISO-8601
})

test_that("vmx_analysis_log accepts a resource object", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = list(items = list(), next_cursor = NULL))
  })
  dv <- new_vmx_resource(list(data_version_id = "dv_9"), "vmx_data_version", "data_version_id")
  vmx_analysis_log("std_1", resource = dv, client = con)
  expect_match(env$req$url, "resource_id=dv_9")
})

test_that("vmx_fit_obs_vs_pred groups parallel arrays; bands go to 'extra'", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1",
    pk = list(
      subject_id = list("1", "1", "2"),
      time = list(0, 1.5, 0),
      observed_concentration = list(0, 12.3, 0),
      is_bloq = list(TRUE, FALSE, TRUE),
      predicted_concentration = list(p05 = list(0, 1, 0), p50 = list(0, 2, 0), p95 = list(0, 3, 0))
    ),
    pd_markers = list()
  ))))
  tbl <- vmx_fit_obs_vs_pred("mf_1", client = con)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 3L)                       # 3 observations
  expect_equal(tbl$subject_id, c("1", "1", "2"))
  expect_equal(tbl$observed_concentration, c(0, 12.3, 0))
  expect_type(tbl$is_bloq, "logical")
  # quantile bands are not columnar -> kept aside, not guessed into columns
  expect_false("predicted_concentration" %in% names(tbl))
  expect_false(is.null(attr(tbl, "extra")$predicted_concentration))
  expect_equal(attr(tbl, "model_fit_id"), "mf_1")
})
