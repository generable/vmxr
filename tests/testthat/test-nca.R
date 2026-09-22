# Unit tests for the NCA verbs and the NCA poller, using httr2 mocks.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

nca_item <- function(id, status = "queued") {
  list(nca_id = id, data_version_id = "dv_1", status = status,
       time_basis = "observed", created_at = "2026-01-01T00:00:00Z")
}

capture_req <- function(body) {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = body)
  }, env = parent.frame())
  env
}

test_that("vmx_nca_analyses returns all server-owned pages", {
  env <- new.env()
  i <- 0L
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    i <<- i + 1L
    httr2::response_json(body = list(
      items = list(nca_item(paste0("nca_", i), "completed")),
      next_cursor = if (i == 1L) "opaque-next-page" else NA_character_,
      has_next_page = i == 1L
    ))
  })
  tbl <- vmx_nca_analyses(data_version = "dv_1", client = con)
  expect_equal(tbl$nca_id, c("nca_1", "nca_2"))
  expect_match(env$req$url, "data_version_id=dv_1")
  expect_match(env$req$url, "cursor=opaque-next-page")
})

test_that("vmx_nca creates without waiting and posts the right body", {
  env <- capture_req(nca_item("nca_9"))
  nca <- vmx_nca("dv_1", "observed", wait = FALSE, client = con)
  expect_s3_class(nca, "vmx_nca_analysis")
  expect_equal(env$req$body$data$data_version_id, "dv_1")
  expect_equal(env$req$body$data$time_basis, "observed")
  expect_match(env$req$url, "/nca-analyses$")
})

test_that("vmx_nca validates scalar controls and retry ids", {
  expect_error(
    vmx_nca(
      "dv_1", c("observed", "nominal"),
      wait = FALSE, client = con
    ),
    class = "vmx_usage_error"
  )
  expect_error(
    vmx_nca(
      "dv_1", "observed", retried_from = "run_wrong",
      wait = FALSE, client = con
    ),
    class = "vmx_usage_error"
  )
})

test_that("vmx_nca with wait=TRUE polls to a terminal state", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = nca_item("nca_9", "queued")),   # create
    httr2::response_json(body = nca_item("nca_9", "running")),  # poll 1
    httr2::response_json(body = nca_item("nca_9", "completed")) # poll 2
  ))
  nca <- vmx_nca("dv_1", "observed", wait = TRUE, interval = 0.001,
                 progress = FALSE, client = con)
  expect_equal(nca$status, "completed")
})

test_that("vmx_wait on an NCA raises on failure", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = nca_item("nca_9", "failed"))
  ))
  nca <- new_vmx_resource(nca_item("nca_9", "queued"), "vmx_nca_analysis", "nca_id")
  expect_error(vmx_wait(nca, interval = 0.001, progress = FALSE, client = con),
               class = "vmx_api_error")
})

test_that("degraded is treated as a (non-error) terminal state", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = nca_item("nca_9", "degraded"))
  ))
  nca <- new_vmx_resource(nca_item("nca_9", "queued"), "vmx_nca_analysis", "nca_id")
  out <- vmx_wait(nca, interval = 0.001, progress = FALSE, client = con)
  expect_equal(out$status, "degraded")
})

test_that("vmx_nca_result reshapes point_estimates into a tidy tibble", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      nca_id = "nca_1", data_version_id = "dv_1", status = "completed",
      time_basis = "observed",
      subject_id = list("S1", "S2"),
      gen_subject_uuid = list(
        "11111111-1111-4111-8111-111111111111",
        "22222222-2222-4222-8222-222222222222"
      ),
      point_estimates = list(cmax = list(10.5, 12.2), auc_inf = list(100, 200)),
      quantities = list(
        list(
          name = "cmax", display_name = "Cmax", unit = "ng/mL",
          explanation = "Maximum observed concentration."
        ),
        list(
          name = "auc_inf", display_name = "AUCinf", unit = "ng*h/mL",
          explanation = "Area under the concentration-time curve."
        )
      ),
      excluded_subjects = list(list(
        gen_subject_uuid = "33333333-3333-4333-8333-333333333333",
        subject_id = "S3",
        reasons = list("insufficient_terminal_points")
      )),
      units = list(cmax = "ng/mL", auc_inf = "ng*h/mL"),
      worker_version = "nca/0.2.3",
      trigger_source = list(
        type = "system",
        user_id = NA_character_,
        system_component = "event_router",
        reason = "data_version_eligible"
      ),
      retried_from = NA_character_
    ))
  ))
  tbl <- vmx_nca_result("nca_1", client = con)
  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$subject_id, c("S1", "S2"))
  expect_equal(tbl$cmax, c(10.5, 12.2))
  expect_equal(tbl$auc_inf, c(100, 200))
  expect_equal(attr(tbl, "quantities")[[1]]$display_name, "Cmax")
  expect_equal(attr(tbl, "units")$auc_inf, "ng*h/mL")
  expect_equal(attr(tbl, "worker_version"), "nca/0.2.3")
  expect_equal(
    attr(tbl, "excluded_subjects")[[1]]$gen_subject_uuid,
    "33333333-3333-4333-8333-333333333333"
  )
})

test_that("vmx_nca_result reads a single-interval 0.3 items[] response", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = list(
      nca_id = "nca_1", data_version_id = "dv_1", status = "completed",
      worker_version = "nca/0.8.0",
      trigger_source = list(
        type = "user", user_id = "usr_1",
        system_component = NA_character_, reason = "user_request"
      ),
      retried_from = NA_character_, stale_data_version = FALSE,
      current_data_version_id = "dv_1", rerun_warning = NA_character_,
      inputs = list(time_basis = "observed", bloq_handling = "discard"),
      items = list(list(
        item_index = 1L, label = "First dosing interval",
        gen_subject_uuid = list(
          "11111111-1111-4111-8111-111111111111",
          "22222222-2222-4222-8222-222222222222"
        ),
        subject_id = list("S1", "S2"),
        resolved_dosing_interval_indices = list(1L, NA_integer_),
        # second subject has no resolved interval -> JSON null (NA serialises to
        # null); jsonlite would render a literal R NULL as `{}`, not `null`.
        resolved_time_intervals_hours = list(
          list(start_time_hours = 0, end_time_hours = 24),
          NA
        ),
        point_estimates = list(
          auc_interval = list(1180.1, NA),
          cmax = list(10.5, 12.2)
        ),
        quantities = list(
          list(name = "auc_interval", display_name = "AUC",
               unit = "ng/mL*h", explanation = "Area under the curve."),
          list(name = "cmax", display_name = "Cmax",
               unit = "ng/mL", explanation = "Maximum concentration.")
        ),
        not_estimable_reasons = list(
          list(),
          list(auc_interval = list("insufficient_terminal_points"))
        ),
        excluded_subjects = list(),
        units = list(auc_interval = "ng/mL*h", cmax = "ng/mL")
      )),
      next_cursor = NA_character_, has_next_page = FALSE
    ))
  })
  tbl <- vmx_nca_result("nca_1", client = con)
  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$item_index, c(1L, 1L))
  expect_equal(tbl$label, rep("First dosing interval", 2))
  expect_equal(tbl$interval_start_hours, c(0, NA))
  expect_equal(tbl$interval_end_hours, c(24, NA))
  expect_equal(tbl$subject_id, c("S1", "S2"))
  expect_equal(tbl$auc_interval, c(1180.1, NA))
  expect_equal(tbl$cmax, c(10.5, 12.2))
  # not_estimable_reasons is surfaced per subject; the null auc carries its reason
  expect_equal(tbl$not_estimable_reasons[[1]], list())
  expect_equal(
    tbl$not_estimable_reasons[[2]]$auc_interval[[1]],
    "insufficient_terminal_points"
  )
  expect_equal(attr(tbl, "time_basis"), "observed")
  expect_equal(attr(tbl, "units")$cmax, "ng/mL")
  expect_equal(attr(tbl, "worker_version"), "nca/0.8.0")
  # not_estimable_reasons must be a list-column, not a metric column
  expect_true("not_estimable_reasons" %in% names(tbl))
})

test_that("vmx_nca_result reads a recorded 0.3 result byte-for-byte", {
  # Recorded from a staging workspace (API 0.3, worker nca/0.13.4) on 2026-09-14; see
  # fixtures/nca-result-03/README.md. Served as raw bytes so jsonlite's
  # re-serialisation cannot change nulls or empty objects on the way in.
  body <- readBin(
    test_path("fixtures", "nca-result-03", "result.json"), "raw",
    file.size(test_path("fixtures", "nca-result-03", "result.json"))
  )
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response(
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      body = body
    )
  })
  tbl <- vmx_nca_result("nca_fixture0300000000000000000000", client = con)
  expect_match(env$req$url, "/nca-analyses/nca_fixture0300000000000000000000/result$")

  # one row per subject for the single item; identity + interval columns first
  expect_equal(nrow(tbl), 32L)
  expect_equal(
    names(tbl)[1:6],
    c("item_index", "label", "interval_start_hours", "interval_end_hours",
      "subject_id", "gen_subject_uuid")
  )
  expect_equal(tbl$item_index, rep(1L, 32))
  expect_equal(tbl$label, rep("First dosing interval", 32))
  expect_type(tbl$subject_id, "character")
  expect_equal(anyDuplicated(tbl$gen_subject_uuid), 0L)
  expect_equal(anyDuplicated(tbl$subject_id), 0L)

  # a single-dose study: the server resolves no interval for any subject
  expect_true(all(is.na(tbl$interval_start_hours)))
  expect_true(all(is.na(tbl$interval_end_hours)))

  # all 19 served quantities become numeric columns, in the served order
  metrics <- vapply(attr(tbl, "quantities"), `[[`, "", "name")
  expect_length(metrics, 19L)
  expect_true(all(metrics %in% names(tbl)))
  expect_setequal(names(attr(tbl, "units")), metrics)
  expect_equal(attr(tbl, "units")$cmax, "mg/L")
  expect_equal(attr(tbl, "units")$auc_inf, "mg/L*h")

  # populated quantities are complete; tau-dependent ones are all-NA with a
  # per-subject reason carried in the list-column
  for (m in c("cmax", "tmax", "auc_inf", "auc_last", "t_half", "cl", "v",
              "n_observations", "terminal_rate_constant")) {
    expect_false(anyNA(tbl[[m]]), info = m)
    expect_type(tbl[[m]], "double")
  }
  for (m in c("c_avg", "auc_tau", "auc_interval", "auc_interval_duration",
              "auc_tau_interval_duration")) {
    expect_true(all(is.na(tbl[[m]])), info = m)
  }
  expect_length(tbl$not_estimable_reasons, 32L)
  expect_equal(
    tbl$not_estimable_reasons[[1]]$auc_tau[[1]],
    "intended_tau_not_available_for_subject"
  )
  expect_equal(
    tbl$not_estimable_reasons[[1]]$auc_interval[[1]],
    "open_ended_dosing_interval"
  )
  expect_equal(tbl$subject_id[[1]], "9")
  expect_equal(tbl$cmax[[1]], 12.9)

  # envelope metadata rides along as attributes
  expect_equal(attr(tbl, "nca_id"), "nca_fixture0300000000000000000000")
  expect_equal(attr(tbl, "data_version_id"), "dv_fixture0300000000000000000000000")
  expect_equal(attr(tbl, "status"), "completed")
  expect_equal(attr(tbl, "time_basis"), "observed")
  expect_equal(attr(tbl, "worker_version"), "nca/0.13.4")
  expect_equal(attr(tbl, "trigger_source")$system_component, "event_router")
  expect_equal(length(attr(tbl, "excluded_subjects")), 0L)
})

test_that("vmx_nca_result assembles a multi-interval, multi-page 0.3 result", {
  env <- new.env()
  i <- 0L
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    i <<- i + 1L
    idx <- i
    httr2::response_json(body = list(
      nca_id = "nca_1", data_version_id = "dv_1", status = "completed",
      worker_version = "nca/0.8.0",
      trigger_source = list(
        type = "system", user_id = NA_character_,
        system_component = "event_router", reason = "data_version_eligible"
      ),
      retried_from = NA_character_, stale_data_version = FALSE,
      current_data_version_id = "dv_1", rerun_warning = NA_character_,
      inputs = list(time_basis = "observed", bloq_handling = "discard"),
      items = list(list(
        item_index = idx, label = paste("Interval", idx),
        gen_subject_uuid = list(
          "11111111-1111-4111-8111-111111111111",
          "22222222-2222-4222-8222-222222222222"
        ),
        subject_id = list("S1", "S2"),
        resolved_time_intervals_hours = list(
          list(start_time_hours = (idx - 1) * 24, end_time_hours = idx * 24),
          list(start_time_hours = (idx - 1) * 24, end_time_hours = idx * 24)
        ),
        point_estimates = list(cmax = list(10 * idx, 20 * idx)),
        quantities = list(list(
          name = "cmax", display_name = "Cmax",
          unit = "ng/mL", explanation = "Maximum concentration."
        )),
        not_estimable_reasons = list(list(), list()),
        excluded_subjects = list(),
        units = list(cmax = "ng/mL")
      )),
      next_cursor = if (idx == 1L) "cursor-page-2" else NA_character_,
      has_next_page = idx == 1L
    ))
  })
  tbl <- vmx_nca_result("nca_1", client = con)
  # both intervals across both pages assembled without truncation
  expect_equal(nrow(tbl), 4L)
  expect_equal(tbl$item_index, c(1L, 1L, 2L, 2L))
  expect_equal(tbl$label, c("Interval 1", "Interval 1", "Interval 2", "Interval 2"))
  expect_equal(tbl$interval_start_hours, c(0, 0, 24, 24))
  expect_equal(tbl$interval_end_hours, c(24, 24, 48, 48))
  expect_equal(tbl$cmax, c(10, 20, 20, 40))
  # the second page was fetched with the server-provided opaque cursor
  expect_match(env$req$url, "cursor=cursor-page-2")
})

nca_item_0_3 <- function(item_index, metrics, subject = "S1",
                          uuid = "11111111-1111-4111-8111-111111111111",
                          next_cursor = NA_character_, has_next_page = FALSE) {
  quantities <- lapply(names(metrics), function(nm) list(
    name = nm, display_name = toupper(nm), unit = "ng/mL",
    explanation = "A PK quantity."
  ))
  units <- stats::setNames(as.list(rep("ng/mL", length(metrics))), names(metrics))
  list(
    nca_id = "nca_1", data_version_id = "dv_1", status = "completed",
    inputs = list(time_basis = "observed", bloq_handling = "discard"),
    items = list(list(
      item_index = item_index, label = paste("Interval", item_index),
      gen_subject_uuid = list(uuid), subject_id = list(subject),
      point_estimates = lapply(metrics, function(v) list(v)),
      quantities = quantities,
      not_estimable_reasons = list(list()),
      excluded_subjects = list(), units = units
    )),
    next_cursor = next_cursor, has_next_page = has_next_page
  )
}

test_that("vmx_nca_result handles an empty 0.3 items[] collection", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      nca_id = "nca_1", data_version_id = "dv_1", status = "completed",
      inputs = list(time_basis = "observed", bloq_handling = "discard"),
      items = list(), next_cursor = NA_character_, has_next_page = FALSE
    ))
  ))
  tbl <- vmx_nca_result("nca_1", client = con)
  expect_equal(nrow(tbl), 0L)
  expect_true(all(
    c("item_index", "label", "subject_id", "gen_subject_uuid",
      "not_estimable_reasons") %in% names(tbl)
  ))
})

test_that("vmx_nca_result unions differing metric columns across items", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      nca_id = "nca_1", data_version_id = "dv_1", status = "completed",
      inputs = list(time_basis = "observed", bloq_handling = "discard"),
      items = list(
        nca_item_0_3(1L, list(cmax = 5))$items[[1]],
        nca_item_0_3(2L, list(auc = 9))$items[[1]]
      ),
      next_cursor = NA_character_, has_next_page = FALSE
    ))
  ))
  tbl <- vmx_nca_result("nca_1", client = con)
  expect_equal(nrow(tbl), 2L)
  expect_true(all(c("cmax", "auc") %in% names(tbl)))
  # each metric present only for its own item; the other row is NA
  expect_equal(tbl$cmax, c(5, NA))
  expect_equal(tbl$auc, c(NA, 9))
  # per-item quantities are exposed as the de-duplicated union
  expect_setequal(vapply(attr(tbl, "quantities"), `[[`, "", "name"), c("cmax", "auc"))
})

test_that("vmx_nca_result rejects a dropped page (non-contiguous item_index)", {
  i <- 0L
  httr2::local_mocked_responses(function(req) {
    i <<- i + 1L
    idx <- if (i == 1L) 1L else 3L  # page 2 skips index 2 -> gap
    httr2::response_json(body = nca_item_0_3(
      idx, list(cmax = idx),
      next_cursor = if (i == 1L) "cursor-page-2" else NA_character_,
      has_next_page = i == 1L
    ))
  })
  expect_error(vmx_nca_result("nca_1", client = con), class = "vmx_response_error")
})

test_that("vmx_nca_result fails loudly on a repeated pagination cursor", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = nca_item_0_3(
      1L, list(cmax = 1), next_cursor = "loop", has_next_page = TRUE
    ))
  })
  expect_error(vmx_nca_result("nca_1", client = con), class = "vmx_response_error")
})

test_that("vmx_nca_result surfaces not_estimable_reasons on the flat 0.2.2 shape", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      nca_id = "nca_1", data_version_id = "dv_1", status = "completed",
      time_basis = "observed",
      subject_id = list("S1", "S2"),
      gen_subject_uuid = list(
        "11111111-1111-4111-8111-111111111111",
        "22222222-2222-4222-8222-222222222222"
      ),
      point_estimates = list(cmax = list(10.5, NA)),
      not_estimable_reasons = list(
        list(),
        list(cmax = list("insufficient_terminal_points"))
      ),
      quantities = list(list(
        name = "cmax", display_name = "Cmax",
        unit = "ng/mL", explanation = "Maximum concentration."
      )),
      excluded_subjects = list(), units = list(cmax = "ng/mL"),
      trigger_source = list(
        type = "system", system_component = "event_router",
        reason = "data_version_eligible"
      )
    ))
  ))
  tbl <- vmx_nca_result("nca_1", client = con)
  expect_equal(nrow(tbl), 2L)
  expect_true("not_estimable_reasons" %in% names(tbl))
  expect_equal(tbl$cmax, c(10.5, NA))
  expect_equal(
    tbl$not_estimable_reasons[[2]]$cmax[[1]],
    "insufficient_terminal_points"
  )
  # the flat 0.2.2 shape has no per-interval dimension
  expect_false("item_index" %in% names(tbl))
})

test_that("vmx_nca_result rejects misaligned point-estimate arrays", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      nca_id = "nca_1",
      data_version_id = "dv_1",
      status = "completed",
      time_basis = "observed",
      subject_id = list("S1", "S2"),
      gen_subject_uuid = list(
        "11111111-1111-4111-8111-111111111111",
        "22222222-2222-4222-8222-222222222222"
      ),
      point_estimates = list(cmax = list(10.5)),
      quantities = list(list(
        name = "cmax", display_name = "Cmax", unit = "ng/mL",
        explanation = "Maximum observed concentration."
      )),
      excluded_subjects = list(),
      units = list(cmax = "ng/mL"),
      worker_version = "nca/0.2.3",
      trigger_source = list(
        type = "user",
        user_id = "usr_1",
        system_component = NA_character_,
        reason = "user_request"
      ),
      retried_from = NA_character_
    ))
  ))
  expect_error(
    vmx_nca_result("nca_1", client = con),
    class = "vmx_response_error"
  )
})

test_that("vmx_nca_result protects subject-identity columns", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      nca_id = "nca_1",
      data_version_id = "dv_1",
      status = "completed",
      time_basis = "observed",
      subject_id = list("S1"),
      gen_subject_uuid = list(
        "11111111-1111-4111-8111-111111111111"
      ),
      point_estimates = list(subject_id = list(10)),
      quantities = list(),
      excluded_subjects = list(),
      units = list(),
      trigger_source = list(
        type = "system",
        system_component = "event_router",
        reason = "data_version_eligible"
      )
    ))
  ))
  expect_error(
    vmx_nca_result("nca_1", client = con),
    class = "vmx_response_error"
  )
})
