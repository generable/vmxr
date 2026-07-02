# Unit tests for the modeling verbs, using httr2 mocks. Payloads mirror the
# real staging artifact shapes.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

run_item <- function(id = "run_1", status = "queued") {
  list(run_id = id, data_version_id = "dv_1", status = status, time_basis = "observed")
}
capture_one <- function(body) {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = body)
  }, env = parent.frame())
  env
}

test_that("vmx_model_catalog flattens categories into a tibble", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    pk = list(list(name = "one_cmt", display_name = "1-compartment")),
    pd = list(list(name = "emax", display_name = "Emax"))
  ))))
  tbl <- vmx_model_catalog(client = con)
  expect_equal(nrow(tbl), 2L)
  expect_setequal(tbl$category, c("pk", "pd"))
  expect_true("display_name" %in% names(tbl))
})

test_that("vmx_model_build parses pd_marker shorthand and posts the body", {
  env <- capture_one(run_item("run_9"))
  run <- vmx_model_build("dv_1", "observed",
                         pd_marker = "GEN_abc:decreasing",
                         covariate = c("WT", "AGE"), wait = FALSE, client = con)
  expect_s3_class(run, "vmx_model_build_run")
  body <- env$req$body$data
  expect_equal(body$data_version_id, "dv_1")
  expect_equal(body$covariates, list("WT", "AGE"))
  expect_equal(body$pd_markers, list(list(gen_uuid = "GEN_abc", direction = "decreasing")))
})

test_that("vmx_model_build rejects malformed pd_marker", {
  expect_error(
    vmx_model_build("dv_1", "observed", pd_marker = "GEN_abc:sideways", client = con),
    class = "vmx_usage_error"
  )
})

test_that("vmx_model_build wait=TRUE polls to terminal", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = run_item("run_9", "queued")),
    httr2::response_json(body = run_item("run_9", "running")),
    httr2::response_json(body = run_item("run_9", "succeeded"))
  ))
  run <- vmx_model_build("dv_1", "observed", wait = TRUE, interval = 0,
                         progress = FALSE, client = con)
  expect_equal(run$status, "succeeded")
})

test_that("vmx_model_build_report_create posts report request", {
  env <- capture_one(list(run_id = "run_9", status = "queued", subject_plot_mode = "none"))
  out <- vmx_model_build_report_create("run_9", subject_plot_mode = "none", client = con)
  expect_equal(out$status, "queued")
  expect_equal(env$req$body$data$subject_plot_mode, "none")
  expect_match(env$req$url, "/model-build-runs/run_9/report$")
})

test_that("vmx_wait on a build run raises on failure/cancelled", {
  httr2::local_mocked_responses(list(httr2::response_json(body = run_item("run_9", "cancelled"))))
  run <- new_vmx_resource(run_item("run_9"), "vmx_model_build_run", "run_id")
  expect_error(vmx_wait(run, interval = 0, progress = FALSE, client = con),
               class = "vmx_api_error")
})

test_that("vmx_model_fits and vmx_model_fit work", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    items = list(list(model_fit_id = "mf_1", run_id = "run_1", data_version_id = "dv_1",
                      model_type = "pk", status = "succeeded")),
    next_cursor = NULL
  ))))
  fits <- vmx_model_fits(run = "run_1", client = con)
  expect_equal(fits$model_fit_id, "mf_1")

  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    metadata = list(a = 1), model = list(b = 2), inference = list(c = 3)
  ))))
  fit <- vmx_model_fit("mf_1", client = con)
  expect_s3_class(fit, "vmx_model_fit")
  expect_equal(vmx_resource_id(fit), "mf_1")
})

test_that("vmx_model_fit_postprocessor_status calls the current endpoint", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1", status = "succeeded"
  ))))
  out <- vmx_model_fit_postprocessor_status("mf_1", client = con)
  expect_equal(out$model_fit_id, "mf_1")
  expect_equal(out$status, "succeeded")
})

test_that("vmx_fit_subject_estimates reshapes to tidy long", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1",
    gen_subject_uuid = list("u1", "u2"),
    subject_id = list("1", "2"),
    estimates = list(list(
      name = "CL", display_name = "Clearance", unit = "L/h",
      value = list(0.6, 0.8),
      interval = list(lower = list(0.5, 0.7), upper = list(0.7, 0.9)),
      value_statistic = "median", kind = "structural", model_type = "pk"
    ))
  ))))
  tbl <- vmx_fit_subject_estimates("mf_1", client = con)
  expect_equal(nrow(tbl), 2L)             # 2 subjects x 1 parameter
  expect_equal(tbl$subject_id, c("1", "2"))
  expect_equal(tbl$name, c("CL", "CL"))
  expect_equal(tbl$value, c(0.6, 0.8))
  expect_equal(tbl$ci_lower, c(0.5, 0.7))
})

test_that("vmx_fit_global_estimates reshapes to one row per parameter", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1",
    estimates = list(
      list(name = "CL", display_name = "Clearance", unit = "L/h", value = 0.74,
           interval = list(lower = 0.62, upper = 0.92, level = 0.95),
           value_statistic = "median", kind = "structural", model_type = "pk"),
      list(name = "sigma", display_name = "Noise", unit = "dimensionless", value = 0.04,
           interval = list(lower = 0.03, upper = 0.06, level = 0.95),
           value_statistic = "median", kind = "observation_model", model_type = "pk",
           description = "obs noise")
    )
  ))))
  tbl <- vmx_fit_global_estimates("mf_1", client = con)
  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$name, c("CL", "sigma"))
  expect_equal(tbl$value, c(0.74, 0.04))
  expect_equal(tbl$ci_upper, c(0.92, 0.06))
  expect_equal(tbl$level, c(0.95, 0.95))
})
