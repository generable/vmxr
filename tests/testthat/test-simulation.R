# Unit tests for the simulation verbs, using httr2 mocks.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

job_item <- function(id = "simjob_1", status = "queued") {
  list(simulation_job_id = id, status = status)
}
capture_one <- function(body) {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = body)
  }, env = parent.frame())
  env
}
di <- new_vmx_resource(list(dosing_input_id = "di_1"), "vmx_dosing_input", "dosing_input_id")

test_that("vmx_dosing_input posts text + scenario_names", {
  env <- capture_one(list(dosing_input_id = "di_9"))
  d <- vmx_dosing_input("mf_1", "100 mg qd x7", c("s1", "s2"), client = con)
  expect_s3_class(d, "vmx_dosing_input")
  expect_equal(env$req$body$data$dosing_text, "100 mg qd x7")
  expect_equal(env$req$body$data$scenario_names, list("s1", "s2"))
  expect_match(env$req$url, "/model-fits/mf_1/simulation-dosing-inputs$")
})

test_that("vmx_dosing_input_status fetches dosing input status", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    dosing_input_id = "di_1", status = "succeeded"
  ))))
  out <- vmx_dosing_input_status("di_1", client = con)
  expect_s3_class(out, "vmx_dosing_input")
  expect_equal(out$status, "succeeded")
})

test_that("vmx_sim_existing_subject builds subject records from a data.frame", {
  env <- capture_one(job_item("simjob_9"))
  subj <- data.frame(gen_subject_uuid = c("u1", "u2"), subject_name = c("A", "B"))
  job <- vmx_sim_existing_subject("mf_1", di, subj, client = con)
  expect_s3_class(job, "vmx_simulation_job")
  body <- env$req$body$data
  expect_equal(body$dosing_input_id, "di_1")
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
  vmx_sim_population("mf_1", "di_1", "high-dose", min_timepoints = 300, client = con)
  expect_equal(env$req$body$data$dosing_input_id, "di_1")
  expect_equal(env$req$body$data$scenario_name, "high-dose")
  expect_equal(env$req$body$data$min_timepoints, 300)
  expect_match(env$req$url, "/population-simulation-jobs$")
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
    items = list(job_item("simjob_1", "succeeded")), next_cursor = NULL
  ))))
  out <- vmx_sim_jobs("mf_1", client = con)
  expect_equal(out$simulation_job_id, "simjob_1")
})

test_that("vmx_sim_status / cancel type the result", {
  httr2::local_mocked_responses(list(httr2::response_json(body = job_item("simjob_1", "running"))))
  expect_s3_class(vmx_sim_status("simjob_1", client = con), "vmx_simulation_job")
})

test_that("vmx_wait on a sim job polls to success and raises on failure", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = job_item("simjob_1", "running")),
    httr2::response_json(body = job_item("simjob_1", "succeeded"))
  ))
  job <- new_vmx_resource(job_item("simjob_1"), "vmx_simulation_job", "simulation_job_id")
  out <- vmx_wait(job, interval = 0, progress = FALSE, client = con)
  expect_equal(out$status, "succeeded")

  httr2::local_mocked_responses(list(httr2::response_json(body = job_item("simjob_1", "failed"))))
  expect_error(vmx_wait(job, interval = 0, progress = FALSE, client = con),
               class = "vmx_api_error")
})

test_that("sim create with wait=TRUE forwards polling controls", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = job_item("simjob_9", "queued")),   # create
    httr2::response_json(body = job_item("simjob_9", "succeeded")) # poll
  ))
  job <- vmx_sim_population("mf_1", "di_1", "s", wait = TRUE, interval = 0,
                           progress = FALSE, client = con)
  expect_equal(job$status, "succeeded")
})
