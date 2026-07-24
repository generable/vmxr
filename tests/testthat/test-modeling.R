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

test_that("PK-only modeling options and builds send an explicit empty marker selection", {
  options_env <- capture_one(list(
    data_version_id = "dv_1",
    options = list(
      pd_markers = list(),
      time_basis = "observed",
      covariates = list()
    ),
    modeling_population = list(),
    available_covariates = list()
  ))
  vmx_modeling_options("dv_1", "observed", client = con)
  expect_equal(options_env$req$body$data$pd_markers, list())

  build_env <- capture_one(run_item("run_10"))
  vmx_model_build("dv_1", "observed", client = con)
  expect_equal(build_env$req$body$data$pd_markers, list())
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
  run <- vmx_model_build("dv_1", "observed", wait = TRUE, interval = 0.001,
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
  expect_error(vmx_wait(run, interval = 0.001, progress = FALSE, client = con),
               class = "vmx_api_error")
})

test_that("vmx_model_fits and vmx_model_fit work", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    items = list(list(model_fit_id = "mf_1", run_id = "run_1", data_version_id = "dv_1",
                      model_type = "pk", status = "succeeded")),
    next_cursor = NA_character_,
    has_next_page = FALSE
  ))))
  fits <- vmx_model_fits(run = "run_1", client = con)
  expect_equal(fits$model_fit_id, "mf_1")
  expect_false(vmx_has_next_page(fits))

  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    metadata = list(model_fit_id = "mf_1", a = 1),
    model = list(b = 2),
    inference = list(c = 3)
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
    schema_version = "1.0",
    gen_subject_uuid = list(
      "11111111-1111-4111-8111-111111111111",
      "22222222-2222-4222-8222-222222222222"
    ),
    subject_id = list("1", "2"),
    estimates = list(list(
      name = "CL", display_name = "Clearance", unit = "L/h",
      value = list(0.6, 0.8),
      interval = list(
        kind = "confidence", level = 0.8,
        lower = list(0.5, 0.7), upper = list(0.7, 0.9)
      ),
      value_statistic = "mean", kind = "structural", model_type = "pk",
      computation = "population_and_random_effects"
    ))
  ))))
  tbl <- vmx_fit_subject_estimates("mf_1", client = con)
  expect_equal(nrow(tbl), 2L)             # 2 subjects x 1 parameter
  expect_equal(tbl$subject_id, c("1", "2"))
  expect_equal(tbl$name, c("CL", "CL"))
  expect_equal(tbl$value, c(0.6, 0.8))
  expect_equal(tbl$interval_lower, c(0.5, 0.7))
  expect_equal(tbl$interval_upper, c(0.7, 0.9))
  expect_equal(tbl$value_statistic, c("mean", "mean"))
  expect_equal(tbl$interval_kind, c("confidence", "confidence"))
  expect_equal(tbl$interval_level, c(0.8, 0.8))
  expect_equal(
    tbl$computation,
    c("population_and_random_effects", "population_and_random_effects")
  )
  expect_equal(attr(tbl, "model_fit_id"), "mf_1")
  expect_equal(attr(tbl, "schema_version"), "1.0")
})

test_that("vmx_fit_global_estimates reshapes to one row per parameter", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1",
    estimates = list(
      list(name = "CL", display_name = "Clearance", unit = "L/h", value = 0.74,
           interval = list(
             kind = "credible", lower = 0.62, upper = 0.92, level = 0.95
           ),
           value_statistic = "median", kind = "structural", model_type = "pk"),
      list(
        name = "theta_weight_CL", display_name = "weight effect on CL",
        unit = "dimensionless", value = 0.04,
        interval = list(
          kind = "confidence", lower = 0.01, upper = 0.07, level = 0.8
        ),
        value_statistic = "mean", kind = "covariate_effect", model_type = "pk",
        target_parameter = "CL", covariate = "weight",
        feature_scale = "log_normalized"
      )
    )
  ))))
  tbl <- vmx_fit_global_estimates("mf_1", client = con)
  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$name, c("CL", "theta_weight_CL"))
  expect_equal(tbl$value, c(0.74, 0.04))
  expect_equal(tbl$interval_upper, c(0.92, 0.07))
  expect_equal(tbl$interval_kind, c("credible", "confidence"))
  expect_equal(tbl$interval_level, c(0.95, 0.8))
  expect_equal(tbl$value_statistic, c("median", "mean"))
  expect_equal(tbl$target_parameter, c(NA, "CL"))
  expect_equal(tbl$covariate, c(NA, "weight"))
  expect_equal(tbl$feature_scale, c(NA, "log_normalized"))
  expect_equal(attr(tbl, "model_fit_id"), "mf_1")
})

test_that("subject estimates fail loudly on misaligned arrays", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1",
    gen_subject_uuid = list(
      "11111111-1111-4111-8111-111111111111",
      "22222222-2222-4222-8222-222222222222"
    ),
    subject_id = list("1", "2"),
    estimates = list(list(
      name = "CL", display_name = "Clearance", unit = "L/h",
      value_statistic = "median", value = list(0.6),
      interval = list(
        kind = "credible", level = 0.95,
        lower = list(0.5, 0.7), upper = list(0.7, 0.9)
      ),
      kind = "structural", model_type = "pk"
    ))
  ))))
  expect_error(
    vmx_fit_subject_estimates("mf_1", client = con),
    class = "vmx_response_error"
  )
})

test_that("global estimates fail loudly when scalar values arrive as arrays", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1",
    estimates = list(list(
      name = "CL", display_name = "Clearance", unit = "L/h",
      value_statistic = "median", value = list(0.6, 0.8),
      interval = list(
        kind = "credible", level = 0.95, lower = 0.5, upper = 0.9
      ),
      kind = "structural", model_type = "pk"
    ))
  ))))
  expect_error(
    vmx_fit_global_estimates("mf_1", client = con),
    class = "vmx_response_error"
  )
})

test_that("vmx_fit_vpc preserves server-provided trajectory semantics", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    model_fit_id = "mf_1",
    model_type = "pk",
    subjects = list(list(
      gen_subject_uuid = "11111111-1111-4111-8111-111111111111",
      subject_id = "1",
      observed = list(),
      time_grids = list(list(
        label = "all times",
        time = list(0, 1),
        trajectory_count = 20,
        concentration = list(
          name = "concentration", model_type = "pk", unit = "ng/mL",
          value_statistic = "mean", value = list(0, 10),
          interval = list(
            kind = "confidence", level = 0.8,
            lower = list(0, 8), upper = list(0, 12)
          )
        )
      ))
    )),
    dose_groups = list()
  ))))
  out <- vmx_fit_vpc("mf_1", client = con)
  estimate <- out$subjects[[1]]$time_grids[[1]]$concentration
  expect_equal(estimate$value_statistic, "mean")
  expect_equal(estimate$interval$kind, "confidence")
  expect_equal(estimate$interval$level, 0.8)
})
