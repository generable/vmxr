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

test_that("vmx_nca_analyses forwards filters and paginates", {
  env <- new.env(); i <- 0
  httr2::local_mocked_responses(function(req) {
    env$req <- req; i <<- i + 1
    if (i == 1) httr2::response_json(body = list(items = list(nca_item("nca_1", "completed")), next_cursor = "c"))
    else httr2::response_json(body = list(items = list(nca_item("nca_2", "failed")), next_cursor = NULL))
  })
  tbl <- vmx_nca_analyses(data_version = "dv_1", client = con)
  expect_equal(nrow(tbl), 2L)
  expect_match(env$req$url, "data_version_id=dv_1")
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
  nca <- vmx_nca("dv_1", "observed", wait = TRUE, interval = 0,
                 progress = FALSE, client = con)
  expect_equal(nca$status, "completed")
})

test_that("vmx_wait on an NCA raises on failure", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = nca_item("nca_9", "failed"))
  ))
  nca <- new_vmx_resource(nca_item("nca_9", "queued"), "vmx_nca_analysis", "nca_id")
  expect_error(vmx_wait(nca, interval = 0, progress = FALSE, client = con),
               class = "vmx_api_error")
})

test_that("degraded is treated as a (non-error) terminal state", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = nca_item("nca_9", "degraded"))
  ))
  nca <- new_vmx_resource(nca_item("nca_9", "queued"), "vmx_nca_analysis", "nca_id")
  out <- vmx_wait(nca, interval = 0, progress = FALSE, client = con)
  expect_equal(out$status, "degraded")
})

test_that("vmx_nca_result reshapes point_estimates into a tidy tibble", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      nca_id = "nca_1", data_version_id = "dv_1", status = "completed",
      time_basis = "observed",
      subject_id = list("S1", "S2"),
      gen_subject_uuid = list("u1", "u2"),
      point_estimates = list(cmax = list(10.5, NULL), auc = list(100, 200)),
      quantities = list(list(name = "cmax", display_name = "Cmax", unit = "ng/mL",
                             explanation = "peak"))
    ))
  ))
  tbl <- vmx_nca_result("nca_1", client = con)
  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$subject_id, c("S1", "S2"))
  expect_equal(tbl$cmax, c(10.5, NA_real_))   # null -> NA
  expect_equal(tbl$auc, c(100, 200))
  expect_equal(attr(tbl, "quantities")[[1]]$display_name, "Cmax")
})
