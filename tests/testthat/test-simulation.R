# Unit tests for the simulation verbs, using httr2 mocks.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

job_item <- function(id = "simjob_1", status = "queued") {
  terminal <- status %in% c("succeeded", "failed", "cancelled")
  list(
    simulation_job_id = id,
    data_version_id = "dv_1",
    model_fit_id = "mf_1",
    run_id = "run_1",
    source_pk_model_fit_id = NA_character_,
    dosing_input_id = "simdose_1",
    kind = "population",
    status = status,
    trigger_source = list(
      type = "user",
      user_id = "usr_1",
      system_component = NA_character_,
      reason = "user_request"
    ),
    retried_from = NA_character_,
    created_at = "2026-04-28T12:00:00Z",
    started_at = if (status == "queued") NA_character_ else "2026-04-28T12:00:05Z",
    updated_at = "2026-04-28T12:00:30Z",
    completed_at = if (terminal) "2026-04-28T12:10:00Z" else NA_character_,
    failure_reason = if (status == "failed") {
      "Simulation exceeded the runtime limit."
    } else {
      NA_character_
    }
  )
}
capture_one <- function(body) {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = body)
  }, env = parent.frame())
  env
}
di <- new_vmx_resource(
  list(dosing_input_id = "simdose_1"),
  "vmx_dosing_input",
  "dosing_input_id"
)

test_that("vmx_dosing_input posts text + scenario_names", {
  env <- capture_one(list(dosing_input_id = "simdose_9"))
  d <- vmx_dosing_input("mf_1", "100 mg qd x7", c("s1", "s2"), client = con)
  expect_s3_class(d, "vmx_dosing_input")
  expect_equal(vmx_resource_id(d), "simdose_9")
  expect_equal(env$req$body$data$dosing_text, "100 mg qd x7")
  expect_equal(env$req$body$data$scenario_names, list("s1", "s2"))
  expect_match(env$req$url, "/model-fits/mf_1/simulation-dosing-inputs$")
})

test_that("vmx_dosing_input_status fetches dosing input status", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    dosing_input_id = "simdose_1",
    model_fit_id = "mf_1",
    status = "succeeded",
    dosing_text = "100 mg once daily for 7 days",
    description = "One oral dose every 24 hours for 7 days.",
    assumptions = list(),
    error_reason = NA_character_,
    error_message = NA_character_,
    trigger_source = list(
      type = "user", user_id = "usr_1", reason = "simulation_dosing_input"
    ),
    created_at = "2026-06-01T12:00:00Z",
    updated_at = "2026-06-01T12:00:05Z",
    completed_at = "2026-06-01T12:00:05Z"
  ))))
  out <- vmx_dosing_input_status("simdose_1", client = con)
  expect_s3_class(out, "vmx_dosing_input")
  expect_equal(out$status, "succeeded")
})

test_that("vmx_sim_existing_subject builds subject records from a data.frame", {
  env <- capture_one(job_item("simjob_9"))
  subj <- data.frame(gen_subject_uuid = c("u1", "u2"), subject_name = c("A", "B"))
  job <- vmx_sim_existing_subject("mf_1", di, subj, client = con)
  expect_s3_class(job, "vmx_simulation_job")
  body <- env$req$body$data
  expect_equal(body$dosing_input_id, "simdose_1")
  expect_equal(body$subjects[[1]], list(gen_subject_uuid = "u1", subject_name = "A"))
  expect_match(env$req$url, "/existing-subject-simulation-jobs$")
})

test_that("vmx_sim_existing_subject_from_text posts dosing text", {
  env <- capture_one(job_item("simjob_9"))
  subj <- data.frame(gen_subject_uuid = "u1", subject_name = "A")
  vmx_sim_existing_subject_from_text("mf_1", "100 mg qd", subj, min_timepoints = 300, client = con)
  expect_equal(env$req$body$data$dosing_text, "100 mg qd")
  expect_equal(env$req$body$data$min_timepoints, 300)
  expect_match(env$req$url, "/existing-subject-simulation-jobs/from-text$")
})

test_that("vmx_sim_hypothetical_subject nests covariates", {
  env <- capture_one(job_item("simjob_9"))
  subj <- data.frame(subject_name = "H1", WT = 70, AGE = 40)
  vmx_sim_hypothetical_subject("mf_1", di, subj, client = con)
  rec <- env$req$body$data$subjects[[1]]
  expect_equal(rec$subject_name, "H1")
  expect_equal(rec$covariates, list(WT = 70, AGE = 40))
})

test_that("vmx_sim_hypothetical_subject_from_text nests covariates", {
  env <- capture_one(job_item("simjob_9"))
  subj <- data.frame(subject_name = "H1", WT = 70)
  vmx_sim_hypothetical_subject_from_text("mf_1", "100 mg qd", subj, client = con)
  expect_equal(env$req$body$data$dosing_text, "100 mg qd")
  expect_equal(env$req$body$data$subjects[[1]]$covariates, list(WT = 70))
  expect_match(env$req$url, "/hypothetical-subject-simulation-jobs/from-text$")
})

test_that("vmx_sim_population posts scenario_name", {
  env <- capture_one(job_item("simjob_9"))
  vmx_sim_population(
    "mf_1", "simdose_1", "high-dose", min_timepoints = 300, client = con
  )
  expect_equal(env$req$body$data$dosing_input_id, "simdose_1")
  expect_equal(env$req$body$data$scenario_name, "high-dose")
  expect_equal(env$req$body$data$min_timepoints, 300)
  expect_match(env$req$url, "/population-simulation-jobs$")
})

test_that("simulation creation validates bounded controls and retry ids", {
  expect_error(
    vmx_sim_population(
      "mf_1", "simdose_1", "high-dose",
      min_timepoints = 9, client = con
    ),
    class = "vmx_usage_error"
  )
  expect_error(
    vmx_sim_population(
      "mf_1", "simdose_1", "high-dose",
      retried_from = "run_wrong", client = con
    ),
    class = "vmx_usage_error"
  )
  expect_error(
    vmx_dosing_input(
      "mf_1", "100 mg qd", c("same", "same"), client = con
    ),
    class = "vmx_usage_error"
  )
})

test_that("vmx_sim_population_from_text posts dosing text", {
  env <- capture_one(job_item("simjob_9"))
  vmx_sim_population_from_text("mf_1", "100 mg qd", "high-dose", client = con)
  expect_equal(env$req$body$data$dosing_text, "100 mg qd")
  expect_equal(env$req$body$data$scenario_name, "high-dose")
  expect_match(env$req$url, "/population-simulation-jobs/from-text$")
})

test_that("vmx_sim_jobs lists jobs for a fit", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    items = list(job_item("simjob_1", "succeeded")),
    next_cursor = NA_character_,
    has_next_page = FALSE
  ))))
  out <- vmx_sim_jobs("mf_1", client = con)
  expect_equal(out$simulation_job_id, "simjob_1")
})

test_that("vmx_sim_status / cancel type the result", {
  httr2::local_mocked_responses(list(httr2::response_json(body = job_item("simjob_1", "running"))))
  expect_s3_class(vmx_sim_status("simjob_1", client = con), "vmx_simulation_job")

  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    simulation_job_id = "simjob_1",
    status = "cancelling",
    cancel_requested_at = "2026-04-28T12:01:00Z"
  ))))
  cancelled <- vmx_sim_cancel("simjob_1", client = con)
  expect_s3_class(cancelled, "vmx_simulation_job")
  expect_equal(cancelled$status, "cancelling")
})

test_that("vmx_wait on a sim job polls to success and raises on failure", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = job_item("simjob_1", "running")),
    httr2::response_json(body = job_item("simjob_1", "succeeded"))
  ))
  job <- new_vmx_resource(job_item("simjob_1"), "vmx_simulation_job", "simulation_job_id")
  out <- vmx_wait(job, interval = 0.001, progress = FALSE, client = con)
  expect_equal(out$status, "succeeded")

  httr2::local_mocked_responses(list(httr2::response_json(body = job_item("simjob_1", "failed"))))
  expect_error(vmx_wait(job, interval = 0.001, progress = FALSE, client = con),
               class = "vmx_api_error")
})

test_that("sim create with wait=TRUE forwards polling controls", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = job_item("simjob_9", "queued")),   # create
    httr2::response_json(body = job_item("simjob_9", "succeeded")) # poll
  ))
  job <- vmx_sim_population("mf_1", "simdose_1", "s", wait = TRUE, interval = 0.001,
                           progress = FALSE, client = con)
  expect_equal(job$status, "succeeded")
})

test_that("vmx_sim_result preserves server-provided trajectory semantics", {
  env <- capture_one(list(
    schema_version = "vmm.simulation_summary.v1",
    simulation_version_id = "sv_1",
    model_fit_id = "mf_1",
    model_type = "pk",
    time_basis = "observed",
    simulation_kind = "population",
    simulation_description = paste(
      "Estimate value and server-provided interval of simulated population-level",
      "concentration and quantities grouped over simulated subjects."
    ),
    scenario_name = "population high-dose scenario",
    grouping_variable = "dose_group",
    units = list(time = "h", concentration = "ng/mL"),
    series = list(list(
      group_name = "dose_group",
      group_level = "simulated: high",
      time = list(0, 1, 2),
      pk = list(
        concentration = list(
          name = "concentration",
          model_type = "pk",
          unit = "ng/mL",
          value_statistic = "mean",
          value = list(0, 10, 8),
          interval = list(
            kind = "confidence",
            level = 0.8,
            lower = list(0, 8, 6),
            upper = list(0, 12, 10)
          )
        )
      )
    )),
    quantities = list(
      summary_description = paste(
        "Estimate value and server-provided interval of simulated",
        "population-level quantities grouped over simulated subjects."
      ),
      group_name = "dose_group",
      group_level = list("simulated: high"),
      rows = list()
    )
  ))
  out <- vmx_sim_result(
    "simjob_1", grouping_variable = "dose_group", client = con
  )
  estimate <- out$series[[1]]$pk$concentration
  expect_equal(estimate$value_statistic, "mean")
  expect_equal(estimate$interval$kind, "confidence")
  expect_equal(estimate$interval$level, 0.8)
  expect_match(env$req$url, "grouping_variable=dose_group")
})

test_that("vmx_sim_result rejects an ambiguous grouping variable", {
  expect_error(
    vmx_sim_result(
      "simjob_1", grouping_variable = c("arm", "dose_group"), client = con
    ),
    class = "vmx_usage_error"
  )
})

test_that("vmx_sim_result rejects a malformed success payload", {
  env <- capture_one(list(
    schema_version = "vmm.simulation_summary.v1",
    simulation_version_id = "sv_1",
    model_fit_id = "mf_1",
    model_type = "pk",
    time_basis = "observed",
    simulation_kind = "population",
    quantities = list()
  ))
  expect_error(
    vmx_sim_result("simjob_1", client = con),
    class = "vmx_response_error"
  )
})

test_that("simulation rejects legacy dosing-input ids", {
  expect_error(
    vmx_sim_population("mf_1", "di_1", "high-dose", client = con),
    class = "vmx_usage_error"
  )
})
