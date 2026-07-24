# Analysis-log verb + obs-vs-pred reshape, using httr2 mocks.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

test_that("vmx_analysis_log paginates and forwards filters", {
  env <- new.env()
  env$urls <- character()
  i <- 0L
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    env$urls <- c(env$urls, req$url)
    i <<- i + 1L
    httr2::response_json(body = list(
      study_id = "std_1",
      items = list(list(
        kind = "event",
        event_type = if (i == 1L) "nca.completed" else "model.completed",
        outcome = "success"
      )),
      next_cursor = if (i == 1L) "older-events" else NA_character_,
      has_next_page = i == 1L
    ))
  })
  tbl <- vmx_analysis_log("std_1", kind = "event",
                          since = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
                          client = con)
  expect_equal(nrow(tbl), 2L)
  expect_match(env$req$url, "/studies/std_1/analysis-log")
  expect_match(env$req$url, "kind=event")
  expect_match(env$req$url, "since=2026-01-01")   # POSIXct -> ISO-8601
  expect_true(all(grepl("kind=event", env$urls, fixed = TRUE)))
  expect_match(env$urls[[2]], "cursor=older-events")
  expect_equal(attr(tbl, "vmx_metadata")$study_id, "std_1")
})

test_that("vmx_analysis_log validates study identity on every page", {
  i <- 0L
  httr2::local_mocked_responses(function(req) {
    i <<- i + 1L
    httr2::response_json(body = list(
      study_id = if (i == 1L) "std_1" else "std_other",
      items = list(),
      next_cursor = if (i == 1L) "next" else NA_character_,
      has_next_page = i == 1L
    ))
  })

  expect_error(
    vmx_analysis_log("std_1", client = con),
    class = "vmx_response_error"
  )
})

test_that("vmx_analysis_log accepts a resource object", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = list(
      study_id = "std_1",
      items = list(),
      next_cursor = NA_character_,
      has_next_page = FALSE
    ))
  })
  dv <- new_vmx_resource(list(data_version_id = "dv_9"), "vmx_data_version", "data_version_id")
  vmx_analysis_log("std_1", resource = dv, client = con)
  expect_match(env$req$url, "resource_id=dv_9")
})

test_that("vmx_analysis_log rejects an ambiguous since vector", {
  expect_error(
    vmx_analysis_log(
      "std_1",
      since = c("2026-01-01T00:00:00Z", "2026-02-01T00:00:00Z"),
      client = con
    ),
    class = "vmx_usage_error"
  )
})

test_that("vmx_fit_obs_vs_pred reshapes current Estimate envelopes", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1",
    pk = list(
      gen_measurement_uuid = list(
        "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaa1",
        "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaa2",
        "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaa3"
      ),
      gen_subject_uuid = list(
        "11111111-1111-4111-8111-111111111111",
        "11111111-1111-4111-8111-111111111111",
        "22222222-2222-4222-8222-222222222222"
      ),
      subject_id = list("1", "1", "2"),
      time = list(0, 1.5, 0),
      observed_concentration = list(NA_character_, 12.3, 8.2),
      is_bloq = list(TRUE, FALSE, TRUE),
      is_aloq = list(FALSE, FALSE, FALSE),
      lloq = list(1.0, NA_character_, 1.0),
      uloq = list(NA_character_, NA_character_, NA_character_),
      predicted_concentration = list(
        value_statistic = "mean",
        value = list(0.8, 11.8, 7.9),
        interval = list(
          kind = "confidence",
          level = 0.8,
          lower = list(0.4, 10.2, 6.7),
          upper = list(1.3, 13.5, 9.2)
        )
      ),
      units = list(time = "h", concentration = "ng/mL")
    ),
    pd_markers = list(
      effect_score = list(
        marker = list(
          gen_uuid = "33333333-3333-4333-8333-333333333333",
          name = "effect_score"
        ),
        gen_measurement_uuid = list(
          "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb1"
        ),
        gen_subject_uuid = list(
          "11111111-1111-4111-8111-111111111111"
        ),
        subject_id = list("1"),
        time = list(0),
        observed = list(42),
        predicted = list(
          value_statistic = "median",
          value = list(41.5),
          interval = list(
            kind = "credible",
            level = 0.9,
            lower = list(39),
            upper = list(44)
          )
        ),
        units = list(time = "h", observed = "score", predicted = "score")
      )
    )
  ))))
  tbl <- vmx_fit_obs_vs_pred("mf_1", client = con)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 3L)
  expect_equal(tbl$subject_id, c("1", "1", "2"))
  expect_equal(tbl$observed_concentration, c(NA_real_, 12.3, 8.2))
  expect_type(tbl$is_bloq, "logical")
  expect_equal(tbl$predicted_value, c(0.8, 11.8, 7.9))
  expect_equal(tbl$predicted_interval_lower, c(0.4, 10.2, 6.7))
  expect_equal(tbl$predicted_interval_upper, c(1.3, 13.5, 9.2))
  expect_equal(tbl$predicted_value_statistic, rep("mean", 3))
  expect_equal(tbl$predicted_interval_kind, rep("confidence", 3))
  expect_equal(tbl$predicted_interval_level, rep(0.8, 3))
  expect_equal(attr(tbl, "units"), list(time = "h", concentration = "ng/mL"))
  expect_false("units" %in% names(tbl))

  pd <- attr(tbl, "pd_markers")$effect_score
  expect_equal(pd$predicted_value, 41.5)
  expect_equal(pd$predicted_interval_kind, "credible")
  expect_equal(pd$predicted_interval_level, 0.9)
  expect_equal(
    attr(pd, "units"),
    list(time = "h", observed = "score", predicted = "score")
  )
  expect_equal(
    attr(pd, "marker")$gen_uuid,
    "33333333-3333-4333-8333-333333333333"
  )
  expect_equal(attr(tbl, "model_fit_id"), "mf_1")
})

test_that("vmx_fit_obs_vs_pred rejects misaligned prediction arrays", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1",
    pk = list(
      gen_measurement_uuid = list(
        "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaa1",
        "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaa2"
      ),
      gen_subject_uuid = list(
        "11111111-1111-4111-8111-111111111111",
        "22222222-2222-4222-8222-222222222222"
      ),
      subject_id = list("1", "2"),
      time = list(0, 1),
      observed_concentration = list(0, 5),
      is_bloq = list(FALSE, FALSE),
      is_aloq = list(FALSE, FALSE),
      predicted_concentration = list(
        value_statistic = "median",
        value = list(0, 5),
        interval = list(
          kind = "credible", level = 0.95,
          lower = list(0), upper = list(0, 6)
        )
      ),
      units = list(time = "h", concentration = "ng/mL")
    ),
    pd_markers = list()
  ))))
  expect_error(
    vmx_fit_obs_vs_pred("mf_1", client = con),
    class = "vmx_response_error"
  )
})
