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

test_that("vmx_nca_analyses returns one server-owned page", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = list(
      items = list(nca_item("nca_1", "completed")),
      next_cursor = "opaque-next-page",
      has_next_page = TRUE
    ))
  })
  tbl <- vmx_nca_analyses(data_version = "dv_1", client = con)
  expect_equal(nrow(tbl), 1L)
  expect_match(env$req$url, "data_version_id=dv_1")
  expect_equal(vmx_next_cursor(tbl), "opaque-next-page")
  expect_true(vmx_has_next_page(tbl))
})

test_that("vmx_nca creates without waiting and posts the right body", {
  env <- capture_req(nca_item("nca_9"))
  nca <- vmx_nca("dv_1", "observed", wait = FALSE, client = con)
  expect_s3_class(nca, "vmx_nca_analysis")
  expect_equal(env$req$body$data$data_version_id, "dv_1")
  expect_equal(env$req$body$data$time_basis, "observed")
  expect_match(env$req$url, "/nca-analyses$")
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
